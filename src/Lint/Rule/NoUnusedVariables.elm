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


type VariableType
    = Variable
    | ImportedModule
    | ImportedVariable
    | ImportedType
    | ImportedOperator
    | ModuleAlias
    | Type
    | Port


type alias Scope =
    { declared : Dict String ( VariableType, Range )
    , used : Set String
    }


type alias Context =
    { scopes : Nonempty Scope
    , exposesEverything : Bool
    }


emptyScope : Scope
emptyScope =
    Scope Dict.empty Set.empty


error : VariableType -> Range -> String -> Error
error variableType range_ name =
    Error
        "NoUnusedVariables"
        (variableTypeToString variableType ++ " `" ++ name ++ "` is not used" ++ variableTypeWarning variableType)
        range_


variableTypeToString : VariableType -> String
variableTypeToString value =
    case value of
        Variable ->
            "Variable"

        ImportedModule ->
            "Imported module"

        ImportedVariable ->
            "Imported variable"

        ImportedType ->
            "Imported type"

        ImportedOperator ->
            "Imported operator"

        ModuleAlias ->
            "Module alias"

        Type ->
            "Type"

        Port ->
            "Port"


variableTypeWarning : VariableType -> String
variableTypeWarning value =
    case value of
        Variable ->
            ""

        ImportedModule ->
            ""

        ImportedVariable ->
            ""

        ImportedType ->
            ""

        ImportedOperator ->
            ""

        ModuleAlias ->
            ""

        Type ->
            ""

        Port ->
            " (Warning: Removing this port may break your application if it is used in the JS code)"


initialContext : Context
initialContext =
    { scopes = Nonempty.fromElement emptyScope
    , exposesEverything = False
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
    in
    case Maybe.map value exposed of
        Nothing ->
            let
                ( variableType, moduleName ) =
                    case value node |> .moduleAlias of
                        Just moduleAlias ->
                            ( ModuleAlias, moduleAlias )

                        Nothing ->
                            ( ImportedModule, value node |> .moduleName )
            in
            ( []
            , register
                variableType
                (range moduleName)
                (value moduleName |> getModuleName)
                ctx
            )

        Just declaredImports ->
            ( []
            , List.foldl
                (\( variableType, range, name ) context -> register variableType range name context)
                ctx
                (collectFromExposing declaredImports)
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
                        |> register Variable (range declaration.name) (value declaration.name)
                        |> markAllAsUsed namesUsedInSignature
            in
            ( [], newContext )

        ( Enter, CustomTypeDeclaration { name } ) ->
            ( [], register Type (range name) (value name) ctx )

        ( Enter, AliasDeclaration { name } ) ->
            ( [], register Type (range name) (value name) ctx )

        ( Enter, PortDeclaration { name, typeAnnotation } ) ->
            ( []
            , ctx
                |> markAllAsUsed (collectNamesFromTypeAnnotation typeAnnotation)
                |> register Port (range name) (value name)
            )

        ( Enter, InfixDeclaration _ ) ->
            ( [], ctx )

        ( Enter, Destructuring _ _ ) ->
            ( [], ctx )

        ( Exit, _ ) ->
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
                    |> makeReport
                    |> Tuple.first
    in
    ( errors, ctx )


registerFunction : Function -> Context -> Context
registerFunction function ctx =
    let
        declaration =
            value function.declaration
    in
    register Variable (range declaration.name) (value declaration.name) ctx


collectFromExposing : Exposing -> List ( VariableType, Range, String )
collectFromExposing exposing_ =
    case exposing_ of
        All _ ->
            []

        Explicit list ->
            List.filterMap
                (\node ->
                    case value node of
                        FunctionExpose name ->
                            Just ( ImportedVariable, range node, name )

                        InfixExpose name ->
                            Just ( ImportedOperator, range node, name )

                        TypeOrAliasExpose name ->
                            Just ( ImportedType, range node, name )

                        TypeExpose { name, open } ->
                            case open of
                                Just openRange ->
                                    Nothing

                                Nothing ->
                                    Just ( ImportedType, range node, name )
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
                    case value nameNode of
                        ( [], str ) ->
                            str

                        ( moduleName, _ ) ->
                            getModuleName moduleName
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

        Tupled list ->
            List.concatMap collectNamesFromTypeAnnotation list

        GenericType _ ->
            []

        Unit ->
            []


register : VariableType -> Range -> String -> Context -> Context
register variableType range name ctx =
    let
        scopes =
            mapNonemptyHead
                (\scope ->
                    { scope | declared = Dict.insert name ( variableType, range ) scope.declared }
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
                |> List.map (\( key, ( variableType, range ) ) -> error variableType range key)
    in
    ( errors, nonUsedVars )


mapNonemptyHead : (a -> a) -> Nonempty a -> Nonempty a
mapNonemptyHead fn nonempty =
    let
        newHead =
            fn (Nonempty.head nonempty)
    in
    Nonempty.replaceHead newHead nonempty
