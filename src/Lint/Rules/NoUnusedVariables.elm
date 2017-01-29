module Lint.Rules.NoUnusedVariables exposing (rule)

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, Error, Direction(..))
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


rule : String -> List Error
rule input =
    lint input implementation


implementation : LintRule Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = moduleEndFn
    , initialContext = Context [ Scope Set.empty Set.empty ] False
    }


createError : String -> Error
createError name =
    Error "NoUnusedVariables" ("Variable `" ++ name ++ "` is not used")


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


makeReport : Maybe Scope -> ( List Error, Set String )
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


expressionFn : Context -> Direction Expression -> ( List Error, Context )
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


statementFn : Context -> Direction Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        Enter (FunctionDeclaration name args body) ->
            ( [], { ctx | scopes = addFoundToStack ctx.scopes [ name ] } )

        Enter (ModuleDeclaration names AllExport) ->
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
            ( []
            , { ctx
                | scopes =
                    getExported exportType
                        |> Set.toList
                        |> addUsedToStack ctx.scopes
              }
            )

        _ ->
            ( [], ctx )


getNotUsed : Set comparable -> Set comparable -> List comparable
getNotUsed declared used =
    Set.diff declared used
        |> Set.toList
        |> List.sort


moduleEndFn : Context -> ( List Error, Context )
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
