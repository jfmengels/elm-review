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


type alias Scope =
    { declared : Dict String Range
    , used : Set String
    }


type alias Context =
    { scopes : Nonempty Scope
    , exposesEverything : Bool
    }


emptyScope : Scope
emptyScope =
    Scope Dict.empty Set.empty


{-| Reports variables that are declared but never used.

    rules =
        [ NoUnusedVariables.rule
        ]

-}
rule : Rule
rule input =
    lint input implementation


implementation : Implementation Context
implementation =
    createRule
        (Context (Nonempty.fromElement emptyScope) False)
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
        newContext =
            value node
                |> .exposingList
                |> Maybe.map (value >> collectFromExposing)
                |> Maybe.withDefault []
                |> List.foldl (\( range, name ) context -> register range name context) ctx
    in
    ( [], newContext )


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


markAllAsUsed : List String -> Context -> Context
markAllAsUsed names ctx =
    List.foldl markAsUsed ctx names


error : Range -> String -> Error
error range_ name =
    Error "NoUnusedVariables" ("Variable `" ++ name ++ "` is not used") range_


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


visitExpression : Context -> Direction -> Node Expression -> ( List Error, Context )
visitExpression ctx direction node =
    case ( direction, value node ) of
        ( Enter, FunctionOrValue [] name ) ->
            ( [], markAsUsed name ctx )

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


registerFunction : Function -> Context -> Context
registerFunction function ctx =
    let
        declaration =
            value function.declaration
    in
    register (range declaration.name) (value declaration.name) ctx


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


makeReport : Scope -> ( List Error, List String )
makeReport { declared, used } =
    let
        nonUsedVars =
            Set.diff used (Set.fromList <| Dict.keys declared)
                |> Set.toList

        errors =
            Dict.filter (\key _ -> not <| Set.member key used) declared
                |> Dict.toList
                |> List.map (\( key, node ) -> error node key)
    in
    ( errors, nonUsedVars )



-- ( Enter, Variable names ) ->
--     case names of
--         [ name ] ->
--             ( [], { ctx | scopes = addUsedToStack ctx.scopes [ name ] } )
--
--         _ ->
--             ( [], ctx )
--
-- ( Enter, LetExpression declarations ) ->
--     let
--         variables =
--             List.map Tuple.first declarations
--                 |> List.filterMap variableName
--                 |> List.concat
--                 |> Set.fromList
--
--         newScope =
--             Scope variables Set.empty
--     in
--     ( [], { ctx | scopes = newScope :: ctx.scopes } )
--
-- ( Exit, LetExpression _ ) ->
--     let
--         ( errors, variablesUsedButNotFromThisScope ) =
--             ctx.scopes
--                 |> List.head
--                 |> makeReport
--
--         newScopes =
--             List.drop 1 ctx.scopes
--     in
--     ( errors, { ctx | scopes = addUsedToStack newScopes (Set.toList variablesUsedButNotFromThisScope) } )
-- addUsedToStack : List Scope -> List String -> List Scope
-- addUsedToStack scopes variables =
--     let
--         lastScope =
--             case List.head scopes of
--                 Nothing ->
--                     Debug.log "Unexpected Empty scope stack" emptyScope
--
--                 Just scope ->
--                     { scope | used = Set.union scope.used (Set.fromList variables) }
--     in
--     lastScope :: List.drop 1 scopes
--
--
-- addFoundToStack : List Scope -> List String -> List Scope
-- addFoundToStack scopes variables =
--     let
--         lastScope =
--             case List.head scopes of
--                 Nothing ->
--                     Debug.log "Unexpected Empty scope stack" emptyScope
--
--                 Just scope ->
--                     { scope | declared = Set.union scope.declared (Set.fromList variables) }
--     in
--     lastScope :: List.drop 1 scopes
-- makeReport : Maybe Scope -> ( List Error, Set String )
-- makeReport maybeScope =
--     case maybeScope of
--         Nothing ->
--             Debug.log "Unexpected Empty scope stack" ( [], Set.empty )
--
--         Just scope ->
--             let
--                 notUsed =
--                     Set.diff scope.declared scope.used
--
--                 variablesUsedButNotFromThisScope =
--                     Set.diff scope.used scope.declared
--
--                 errors =
--                     Set.diff scope.declared scope.used
--                         |> Set.toList
--                         |> List.sort
--                         |> List.map error
--             in
--             ( errors, variablesUsedButNotFromThisScope )
--
-- variableName : Expression -> Maybe (List String)
-- variableName expr =
--     case expr of
--         Variable names ->
--             Just names
--
--         Application var _ ->
--             variableName var
--
--         _ ->
--             Nothing
--
--
-- getExported : ExportSet -> Set String
-- getExported exportType =
--     case exportType of
--         -- Ignore as this case is handled by `exposesEverything`
--         AllExport ->
--             Set.empty
--
--         SubsetExport exports ->
--             List.map getExported exports
--                 |> List.foldl Set.union Set.empty
--
--         FunctionExport name ->
--             Set.singleton name
--
--         TypeExport name _ ->
--             Set.singleton name
--
--
-- addExposedVariables : Context -> Ast.Statement.ExportSet -> Context
-- addExposedVariables ctx exportType =
--     { ctx
--         | scopes =
--             getExported exportType
--                 |> Set.toList
--                 |> addUsedToStack ctx.scopes
--     }
--
--
-- statementFn : Context -> Direction Statement -> ( List Error, Context )
-- statementFn ctx node =
--     case node of
--         Enter (FunctionDeclaration name args body) ->
--             ( [], { ctx | scopes = addFoundToStack ctx.scopes [ name ] } )
--
--         Enter (ModuleDeclaration names AllExport) ->
--             ( [], { ctx | exposesEverything = True } )
--
--         Enter (PortModuleDeclaration names AllExport) ->
--             ( [], { ctx | exposesEverything = True } )
--
--         Enter (ImportStatement module_ alias_ (Just (SubsetExport imported))) ->
--             let
--                 variables =
--                     List.foldl
--                         (\var res ->
--                             case var of
--                                 FunctionExport name ->
--                                     name :: res
--
--                                 _ ->
--                                     res
--                         )
--                         []
--                         imported
--             in
--             ( [], { ctx | scopes = addFoundToStack ctx.scopes variables } )
--
--         Enter (ModuleDeclaration names exportType) ->
--             ( [], addExposedVariables ctx exportType )
--
--         Enter (PortModuleDeclaration names exportType) ->
--             ( [], addExposedVariables ctx exportType )
--
--         _ ->
--             ( [], ctx )
--
--
-- moduleEndFn : Context -> ( List Error, Context )
-- moduleEndFn ctx =
--     let
--         ( errors, _ ) =
--             if ctx.exposesEverything then
--                 ( [], Set.empty )
--
--             else
--                 ctx.scopes
--                     |> List.head
--                     |> makeReport
--     in
--     ( errors, ctx )


mapNonemptyHead : (a -> a) -> Nonempty a -> Nonempty a
mapNonemptyHead fn nonempty =
    let
        newHead =
            fn (Nonempty.head nonempty)
    in
    Nonempty.replaceHead newHead nonempty
