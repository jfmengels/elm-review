module ScopeTest exposing (all)

import Dict exposing (Dict)
import Elm.Syntax.File
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Expect
import Review.FileParser
import Review.Project as Project exposing (Project)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.Project.Valid as ValidProject
import Review.Resources.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Resources.ScopeInternal as Internal exposing (Binding, Scope(..))
import Review.Scope as Scope
import Review.ScopeProvider as ScopeProvider exposing (ScopeProvider)
import Review.Test.Dependencies
import Set exposing (Set)
import Test exposing (Test, describe, test)
import Unicode


all : Test
all =
    describe "Scope"
        [ test "should list all known top-level elements" <|
            \() ->
                """
module A exposing (a, b)
a = 1
b = a
c = a + b
"""
                    |> getScopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.names
                    |> Set.toList
                    |> Expect.equal [ "a", "b", "c" ]
        , test "should get the binding for a variable name" <|
            \() ->
                """
module A exposing (a, b)
a = 1
b = a
c = a + b
"""
                    |> getScopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.bindings
                    |> Dict.toList
                    |> List.map (\( key, scope ) -> ( key, scope.range ))
                    |> Expect.equal
                        [ ( "a", { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } )
                        , ( "b", { start = { row = 4, column = 1 }, end = { row = 4, column = 2 } } )
                        , ( "c", { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } )
                        ]
        , test "should find all references to a top-level variable" <|
            \() ->
                """
module A exposing (a, b)
a = 1
b = a
c = a + b
"""
                    |> getScopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.bindings
                    |> Dict.toList
                    |> List.map (\( key, scope ) -> ( key, scope.references ))
                    |> Expect.equal
                        [ ( "a"
                          , [ { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            , { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            ]
                          )
                        , ( "b", [ { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } ] )
                        , ( "c", [] )
                        ]
        , test "top-level scope should have the scope of top-level functions as its children" <|
            \() ->
                let
                    source : String
                    source =
                        """
module A exposing (..)

fn n = n + 1
noScope = "noScope"
nestedScope = (\\n -> n)
letScope =
    let a = "letScope"
        subFnScope n = "subFnScope"
    in a
caseScopes =
    case 0 of
        0 -> "0branch"
        _ -> (\\lambda -> lambda)
"""

                    lines : List String
                    lines =
                        String.lines source
                in
                source
                    |> getScopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.children
                    |> List.map (\s -> extractSourceCode lines (Scope.range s))
                    |> Expect.equal
                        [ "(\\lambda -> lambda)"
                        , "\"0branch\""
                        , "n"
                        , "fn n = n + 1"
                        ]
        , test "should get the scope at a given range" <|
            \() ->
                let
                    source : String
                    source =
                        """
module A exposing (..)

fn n = n + 1
noScope = "noScope"
nestedScope = (\\n -> n)
letScope =
    let a = "letScope"
        subFnScope n = "subFnScope"
    in a
caseScopes =
    case 0 of
        0 -> "THISISTHETARGET"
        _ -> (\\lambda -> lambda)
"""

                    lines : List String
                    lines =
                        String.lines source

                    scopeProvider : ScopeProvider
                    scopeProvider =
                        getScopeProvider source
                in
                ScopeProvider.at scopeProvider { start = { row = 13, column = 14 }, end = { row = 13, column = 30 } }
                    |> (\s -> extractSourceCode lines (Scope.range s))
                    |> Expect.equal "\"THISISTHETARGET\""
        ]


getScopeProvider : String -> ScopeProvider
getScopeProvider source =
    let
        ast : Elm.Syntax.File.File
        ast =
            parseOrCrash source

        module_ : OpaqueProjectModule
        module_ =
            ProjectModule.create
                { ast = ast
                , isInSourceDirectories = True
                , moduleId = 0
                , path = "src/A.elm"
                , source = source
                }

        moduleName : ModuleName
        moduleName =
            ProjectModule.moduleName module_

        ( _, scopeProvider, projectCache ) =
            ModuleNameLookupTable.compute moduleName
                module_
                { dependencies = ValidProject.computeDependencies (Project.directDependencies Review.Test.Dependencies.projectWithElmCore)
                , elmJsonContentHash = Nothing
                , lookupTables = Dict.empty
                , modules = Dict.empty
                , types = Nothing
                }

        ( _, scopeProviderOnReRun, _ ) =
            ModuleNameLookupTable.compute moduleName module_ projectCache
    in
    if scopeProvider == scopeProviderOnReRun then
        runChecks scopeProvider

    else
        Debug.todo "Oh no, scope provider is not the same on rerun!"


parseOrCrash : String -> Elm.Syntax.File.File
parseOrCrash source =
    case Review.FileParser.parse source of
        Ok ast ->
            ast

        Err err ->
            Debug.todo ("Invalid source:\n" ++ Debug.toString err)


runChecks : ScopeProvider -> ScopeProvider
runChecks scopeProvider =
    case
        topLevelIsActuallyTopLevel scopeProvider
            --|> scopeIdsAreUnique Set.empty
            --|> allParentsPointToParent
            |> Result.andThen (\() -> bindingIdsAreUnique Dict.empty [ ScopeProvider.topLevel scopeProvider ])
    of
        Ok () ->
            scopeProvider

        Err error ->
            Debug.todo error


topLevelIsActuallyTopLevel : ScopeProvider -> Result String ()
topLevelIsActuallyTopLevel scopeProvider =
    let
        topLevel : Scope
        topLevel =
            ScopeProvider.topLevel scopeProvider
    in
    case Scope.parentId topLevel of
        Just _ ->
            Err ("Oh no! The top-level is not really the top-level!\n\n" ++ Debug.toString topLevel)

        Nothing ->
            Ok ()


bindingIdsAreUnique : Dict Int Binding -> List Scope -> Result String ()
bindingIdsAreUnique seenBindings scopes =
    case scopes of
        [] ->
            Ok ()

        (Scope scope) :: rest ->
            case
                Dict.foldl
                    (\_ binding acc ->
                        case acc of
                            Err _ ->
                                acc

                            Ok soFar ->
                                let
                                    (Internal.BindingId id) =
                                        binding.id
                                in
                                case Dict.get id soFar of
                                    Nothing ->
                                        Ok (Dict.insert id binding soFar)

                                    Just previousBinding ->
                                        [ "Duplicate binding ids!"
                                        , Debug.toString previousBinding
                                        , Debug.toString binding
                                        ]
                                            |> String.join "\n\n"
                                            |> Err
                    )
                    (Ok seenBindings)
                    scope.bindings
            of
                Ok bindings ->
                    bindingIdsAreUnique bindings (scope.children ++ rest)

                Err error ->
                    Err error


extractSourceCode : List String -> Range -> String
extractSourceCode lines { start, end } =
    case List.drop (start.row - 1) lines of
        [] ->
            ""

        firstLine :: rest ->
            if start.row == end.row then
                Unicode.slice
                    (start.column - 1)
                    (end.column - 1)
                    firstLine

            else
                let
                    { lastLine, linesTaken } =
                        takeNLines (end.row - start.row - 1) rest ""
                in
                Unicode.dropLeft (start.column - 1) firstLine
                    ++ linesTaken
                    ++ (case lastLine of
                            Just lastLine_ ->
                                "\n" ++ Unicode.left (end.column - 1) lastLine_

                            Nothing ->
                                ""
                       )


takeNLines : Int -> List String -> String -> { lastLine : Maybe String, linesTaken : String }
takeNLines n lines linesTaken =
    if n <= 0 then
        { lastLine = List.head lines, linesTaken = linesTaken }

    else
        case lines of
            [] ->
                { lastLine = Nothing, linesTaken = linesTaken }

            line :: rest ->
                takeNLines (n - 1)
                    rest
                    (linesTaken ++ "\n" ++ line)
