module ScopeTest exposing (all)

import Dict
import Elm.Syntax.File
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
                    |> Dict.get "a"
                    |> failOnNothing "Could not find a binding `a`"
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
                    |> Dict.get "a"
                    |> failOnNothing "Could not find a binding `a`"
                    |> .references
                    |> Expect.equal
                        [ { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                        , { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
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


failOnNothing : String -> Maybe a -> a
failOnNothing message maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.todo message
