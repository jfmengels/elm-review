module Lint.Rule.NoUnusedVariables exposing (rule)

{-|

@docs rule


# Fail

    a n =
        n + 1

    b =
        a 2


# Success

    a n =
        n + 1

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node, range, value)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Lint exposing (Rule, lint)
import Lint.Error exposing (Error)
import Lint.Rule exposing (Direction(..), Implementation, createRule)
import List.Nonempty as Nonempty exposing (Nonempty)
import Set exposing (Set)


{-| Reports variables that are declared but never used.

    rules =
        [ NoUnusedVariables.rule
        ]

-}
rule : Rule
rule input =
    lint input implementation


type alias Scope =
    { declared : Dict String Range
    , used : Set String
    }


type alias Context =
    { scopes : Nonempty Scope
    , exposesEverything : Bool
    , imports : Dict String Range
    }


emptyScope : Scope
emptyScope =
    Scope Dict.empty Set.empty


error : Range -> String -> Error
error range_ name =
    Error "NoUnusedVariables" ("Variable `" ++ name ++ "` is not used") range_


initialContext : Context
initialContext =
    { scopes = Nonempty.fromElement emptyScope
    , exposesEverything = False
    , imports = Dict.empty
    }


implementation : Implementation Context
implementation =
    createRule
        initialContext
        (\v ->
            { v
                | visitModuleDefinition = visitModuleDefinition
                , visitImport = visitImport
                , visitDeclaration = visitDeclaration
                , visitExpression = visitExpression
                , visitEnd = visitEnd
            }
        )


visitModuleDefinition : Context -> Node Module -> ( List Error, Context )
visitModuleDefinition ctx moduleNode =
    case Module.exposingList (value moduleNode) of
        All _ ->
            ( [], { ctx | exposesEverything = True } )

        Explicit list ->
            let
                names =
                    List.filterMap
                        (\node ->
                            case value node of
                                FunctionExpose name ->
                                    Just name

                                TypeOrAliasExpose name ->
                                    Just name

                                TypeExpose { name } ->
                                    Just name

                                InfixExpose name ->
                                    -- Just name
                                    Nothing
                        )
                        list
            in
            ( [], markAllAsUsed names ctx )


visitImport : Context -> Node Import -> ( List Error, Context )
visitImport ctx node =
    let
        exposed =
            node
                |> value
                |> .exposingList

        declaredImports =
            exposed
                |> Maybe.map (value >> collectFromExposing)
                |> Maybe.withDefault []

        moduleName =
            Maybe.withDefault (value node |> .moduleName) (value node |> .moduleAlias)
    in
    case Maybe.map value exposed of
        Just (All _) ->
            -- Do not attempt to report an import that exposes all
            ( [], ctx )

        _ ->
            if List.isEmpty declaredImports then
                -- Only register the module name
                ( []
                , register
                    (range moduleName)
                    (value moduleName |> getModuleName)
                    ctx
                )

            else
                -- Only register the exposed variables
                ( []
                , List.foldl
                    (\( range, name ) context -> register range name context)
                    ctx
                    declaredImports
                )


visitExpression : Context -> Direction -> Node Expression -> ( List Error, Context )
visitExpression ctx direction node =
    case ( direction, value node ) of
        ( Enter, FunctionOrValue [] name ) ->
            ( [], markAsUsed name ctx )

        ( Enter, FunctionOrValue moduleName name ) ->
            ( [], markAsUsed (getModuleName moduleName) ctx )

        ( Enter, OperatorApplication name _ _ _ ) ->
            ( [], markAsUsed name ctx )

        ( Enter, PrefixOperator name ) ->
            ( [], markAsUsed name ctx )

        ( Enter, LetExpression { declarations } ) ->
            let
                newContext =
                    List.foldl
                        (\declaration context ->
                            case value declaration of
                                LetFunction function ->
                                    registerFunction function context

                                LetDestructuring pattern _ ->
                                    context
                        )
                        { ctx | scopes = Nonempty.cons emptyScope ctx.scopes }
                        declarations
            in
            ( [], newContext )

        ( Exit, LetExpression _ ) ->
            let
                ( errors, remainingUsed ) =
                    makeReport (Nonempty.head ctx.scopes)

                ctxWithPoppedScope =
                    { ctx | scopes = Nonempty.pop ctx.scopes }
            in
            ( errors
            , markAllAsUsed remainingUsed ctxWithPoppedScope
            )

        _ ->
            ( [], ctx )


visitDeclaration : Context -> Direction -> Node Declaration -> ( List Error, Context )
visitDeclaration ctx direction node =
    case ( direction, value node ) of
        ( Enter, FunctionDeclaration function ) ->
            let
                declaration =
                    value function.declaration

                namesUsedInSignature =
                    function.signature
                        |> Maybe.map (value >> .typeAnnotation >> collectNamesFromTypeAnnotation)
                        |> Maybe.withDefault []

                newContext =
                    ctx
                        |> register (range declaration.name) (value declaration.name)
                        |> markAllAsUsed namesUsedInSignature
            in
            ( [], newContext )

        ( Enter, CustomTypeDeclaration { name } ) ->
            ( [], register (range name) (value name) ctx )

        ( Enter, AliasDeclaration { name } ) ->
            ( [], register (range name) (value name) ctx )

        _ ->
            ( [], ctx )


visitEnd : Context -> ( List Error, Context )
visitEnd ctx =
    let
        errors =
            if ctx.exposesEverything then
                []

            else
                ctx.scopes
                    |> Nonempty.head
                    |> makeReportRoot ctx.imports
    in
    ( errors, ctx )


registerFunction : Function -> Context -> Context
registerFunction function ctx =
    let
        declaration =
            value function.declaration
    in
    register (range declaration.name) (value declaration.name) ctx


collectFromExposing : Exposing -> List ( Range, String )
collectFromExposing exposing_ =
    case exposing_ of
        All _ ->
            []

        Explicit list ->
            List.filterMap
                (\node ->
                    case value node of
                        FunctionExpose name ->
                            Just ( range node, name )

                        InfixExpose name ->
                            Just ( range node, name )

                        _ ->
                            Nothing
                )
                list


collectNamesFromTypeAnnotation : Node TypeAnnotation -> List String
collectNamesFromTypeAnnotation node =
    case value node of
        FunctionTypeAnnotation a b ->
            collectNamesFromTypeAnnotation a ++ collectNamesFromTypeAnnotation b

        Typed nameNode params ->
            let
                name =
                    nameNode
                        |> value
                        |> Tuple.second
            in
            name :: List.concatMap collectNamesFromTypeAnnotation params

        Record list ->
            list
                |> List.map (value >> Tuple.second)
                |> List.concatMap collectNamesFromTypeAnnotation

        GenericRecord name list ->
            list
                |> value
                |> List.map (value >> Tuple.second)
                |> List.concatMap collectNamesFromTypeAnnotation

        _ ->
            []


register : Range -> String -> Context -> Context
register range name ctx =
    let
        scopes =
            mapNonemptyHead
                (\scope ->
                    { scope | declared = Dict.insert name range scope.declared }
                )
                ctx.scopes
    in
    { ctx | scopes = scopes }


markAllAsUsed : List String -> Context -> Context
markAllAsUsed names ctx =
    List.foldl markAsUsed ctx names


markAsUsed : String -> Context -> Context
markAsUsed name ctx =
    let
        scopes =
            mapNonemptyHead
                (\scope ->
                    { scope | used = Set.insert name scope.used }
                )
                ctx.scopes
    in
    { ctx | scopes = scopes }


getModuleName : List String -> String
getModuleName name =
    String.join "." name


makeReport : Scope -> ( List Error, List String )
makeReport { declared, used } =
    let
        nonUsedVars =
            Set.diff used (Set.fromList <| Dict.keys declared)
                |> Set.toList

        errors =
            Dict.filter (\key _ -> not <| Set.member key used) declared
                |> Dict.toList
                |> List.map (\( key, range ) -> error range key)
    in
    ( errors, nonUsedVars )


makeReportRoot : Dict String Range -> Scope -> List Error
makeReportRoot imports { declared, used } =
    let
        nonUsedVariablesErrors =
            Dict.filter (\key _ -> not <| Set.member key used) declared
                |> Dict.toList
                |> List.map (\( key, range ) -> error range key)

        nonUsedImportErrors =
            Dict.filter (\key _ -> not <| Set.member key used) imports
                |> Dict.toList
                |> List.map (\( key, range ) -> error range key)
    in
    nonUsedImportErrors ++ nonUsedVariablesErrors


mapNonemptyHead : (a -> a) -> Nonempty a -> Nonempty a
mapNonemptyHead fn nonempty =
    let
        newHead =
            fn (Nonempty.head nonempty)
    in
    Nonempty.replaceHead newHead nonempty
