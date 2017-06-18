module Lint.Rules.NoUnusedVariables exposing (rule)

{-|
@docs rule

# Fail

    module Main exposing (a)
    a n =
        n + 1
    b = a 2

# Success

    module Main exposing (a)
    a n =
        n + 1
-}

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Lint exposing (doNothing, lint)
import Lint.Types exposing (LintRule, Direction(..), LintError, LintRuleImplementation)
import Set exposing (Set)


type alias Scope =
    { declared : Set String
    , used : Set String
    }


type alias Context =
    { scopes : List Scope
    , exportsEverything : Bool
    }


emptyScope : Scope
emptyScope =
    Scope Set.empty Set.empty


{-| Reports variables that are declared but never used.

    rules =
        [ NoUnusedVariables.rule
        ]
-}
rule : LintRule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = moduleEndFn
    , initialContext = Context [ Scope Set.empty Set.empty ] False
    }


createError : String -> LintError
createError name =
    LintError "NoUnusedVariables" ("Variable `" ++ name ++ "` is not used")


addUsedToStack : List Scope -> List String -> List Scope
addUsedToStack scopes variables =
    let
        lastScope =
            case List.head scopes of
                Nothing ->
                    Debug.crash "Unexpected Empty scope stack" emptyScope

                Just scope ->
                    { scope | used = Set.union scope.used (Set.fromList variables) }
    in
        lastScope :: (List.drop 1 scopes)


addFoundToStack : List Scope -> List String -> List Scope
addFoundToStack scopes variables =
    let
        lastScope =
            case List.head scopes of
                Nothing ->
                    Debug.crash "Unexpected Empty scope stack" emptyScope

                Just scope ->
                    { scope | declared = Set.union scope.declared (Set.fromList variables) }
    in
        lastScope :: (List.drop 1 scopes)


makeReport : Maybe Scope -> ( List LintError, Set String )
makeReport scope =
    case scope of
        Nothing ->
            Debug.crash "Unexpected Empty scope stack" ( [], Set.empty )

        Just scope ->
            let
                notUsed =
                    Set.diff scope.declared scope.used

                variablesUsedButNotFromThisScope =
                    Set.diff scope.used scope.declared

                errors =
                    Set.diff scope.declared scope.used
                        |> Set.toList
                        |> List.sort
                        |> List.map createError
            in
                ( errors, variablesUsedButNotFromThisScope )


variableName : Expression -> Maybe (List String)
variableName expr =
    case expr of
        Variable names ->
            Just names

        Application var _ ->
            variableName var

        _ ->
            Nothing


expressionFn : Context -> Direction Expression -> ( List LintError, Context )
expressionFn ctx node =
    case node of
        Enter (Variable names) ->
            case names of
                [ name ] ->
                    ( [], { ctx | scopes = addUsedToStack ctx.scopes [ name ] } )

                _ ->
                    ( [], ctx )

        Enter (Let declarations body) ->
            let
                variables =
                    List.map Tuple.first declarations
                        |> List.filterMap variableName
                        |> List.concat
                        |> Set.fromList

                newScope =
                    Scope variables Set.empty
            in
                ( [], { ctx | scopes = newScope :: ctx.scopes } )

        Exit (Let _ _) ->
            let
                ( errors, variablesUsedButNotFromThisScope ) =
                    ctx.scopes
                        |> List.head
                        |> makeReport

                newScopes =
                    List.drop 1 ctx.scopes
            in
                ( errors, { ctx | scopes = addUsedToStack newScopes (Set.toList variablesUsedButNotFromThisScope) } )

        _ ->
            ( [], ctx )


getExported : ExportSet -> Set String
getExported exportType =
    case exportType of
        -- Ignore as this case is handled by `exportsEverything`
        AllExport ->
            Set.empty

        SubsetExport exports ->
            List.map getExported exports
                |> List.foldl Set.union Set.empty

        FunctionExport name ->
            Set.singleton name

        TypeExport name _ ->
            Set.singleton name


addExposedVariables : Context -> Ast.Statement.ExportSet -> Context
addExposedVariables ctx exportType =
    { ctx
        | scopes =
            getExported exportType
                |> Set.toList
                |> addUsedToStack ctx.scopes
    }


statementFn : Context -> Direction Statement -> ( List LintError, Context )
statementFn ctx node =
    case node of
        Enter (FunctionDeclaration name args body) ->
            ( [], { ctx | scopes = addFoundToStack ctx.scopes [ name ] } )

        Enter (ModuleDeclaration names AllExport) ->
            ( [], { ctx | exportsEverything = True } )

        Enter (PortModuleDeclaration names AllExport) ->
            ( [], { ctx | exportsEverything = True } )

        Enter (ImportStatement module_ alias_ (Just (SubsetExport imported))) ->
            let
                variables =
                    List.foldl
                        (\var res ->
                            case var of
                                FunctionExport name ->
                                    name :: res

                                _ ->
                                    res
                        )
                        []
                        imported
            in
                ( [], { ctx | scopes = addFoundToStack ctx.scopes variables } )

        Enter (ModuleDeclaration names exportType) ->
            ( [], addExposedVariables ctx exportType )

        Enter (PortModuleDeclaration names exportType) ->
            ( [], addExposedVariables ctx exportType )

        _ ->
            ( [], ctx )


moduleEndFn : Context -> ( List LintError, Context )
moduleEndFn ctx =
    let
        ( errors, _ ) =
            if ctx.exportsEverything then
                ( [], Set.empty )
            else
                ctx.scopes
                    |> List.head
                    |> makeReport
    in
        ( errors, ctx )
