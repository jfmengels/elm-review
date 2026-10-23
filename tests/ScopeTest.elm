module ScopeTest exposing (all)

import Dict exposing (Dict)
import Elm.Syntax.File
import Elm.Syntax.Range exposing (Range)
import Expect
import Review.FileParser
import Review.Project as Project exposing (Project)
import Review.Project.ProjectModule as ProjectModule
import Review.Project.Valid as ValidProject
import Review.Resources.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Scope as Scope
import Review.ScopeProvider as ScopeProvider exposing (ScopeProvider)
import Review.Test.Dependencies
import Set
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
                    |> dictGetOrFail "a"
                    |> .range
                    |> Expect.equal { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }
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
                    |> dictGetOrFail "a"
                    |> .references
                    |> Expect.equal
                        [ { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                        , { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
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
    in a
caseScopes =
    case 0 of
        0 -> "0branch"
        _ -> (\\lambda -> lambda)
"""

                    lines : List String
                    lines =
                        String.lines source

                    scopeProvider : ScopeProvider
                    scopeProvider =
                        getScopeProvider source
                in
                scopeProvider
                    |> ScopeProvider.topLevel
                    |> ScopeProvider.children scopeProvider
                    |> List.map Scope.range
                    |> List.map (extractSourceCode lines)
                    |> Expect.equal
                        [ "(\\lambda -> lambda)"
                        , "\"0branch\""
                        ]
        ]


getScopeProvider : String -> ScopeProvider
getScopeProvider source =
    let
        ( _, scopeProvider, _ ) =
            ModuleNameLookupTable.compute [ "A" ]
                (ProjectModule.create
                    { ast = parseOrCrash source
                    , isInSourceDirectories = True
                    , moduleId = 0
                    , path = "src/A.elm"
                    , source = source
                    }
                )
                { dependencies = ValidProject.computeDependencies (Project.directDependencies Review.Test.Dependencies.projectWithElmCore)
                , elmJsonContentHash = Nothing
                , lookupTables = Dict.empty
                , modules = Dict.empty
                , types = Nothing
                }
    in
    scopeProvider


parseOrCrash : String -> Elm.Syntax.File.File
parseOrCrash source =
    case Review.FileParser.parse source of
        Ok ast ->
            ast

        Err err ->
            Debug.todo ("Invalid source:\n" ++ Debug.toString err)


dictGetOrFail : comparable -> Dict comparable a -> a
dictGetOrFail key dict =
    case Dict.get key dict of
        Just a ->
            a

        Nothing ->
            Debug.todo ("Could not get key `" ++ Debug.toString key ++ "`")


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
