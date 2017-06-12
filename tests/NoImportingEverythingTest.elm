port module NoImportingEverythingTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoImportingEverything exposing (rule)
import Lint.Types exposing (LintRule, LintError)
import TestUtil exposing (expectErrors)


error : String -> LintError
error =
    LintError "NoImportingEverything"


tests : List Test
tests =
    [ test "should not report imports that do not expose anything" <|
        \() ->
            rule """
            import Html
            import Http
            """
                |> expectErrors []
    , test "should not report imports that expose functions by name" <|
        \() ->
            rule """
            import Html exposing (a)
            import Http exposing (a, b)
            """
                |> expectErrors []
    , test "should report imports that expose everything" <|
        \() ->
            rule """
            import Html exposing (..)
            """
                |> expectErrors
                    [ error "Do not expose everything from Html"
                    ]
    , test "should report imports from sub-modules" <|
        \() ->
            rule """
            import Html.App exposing (..)
            """
                |> expectErrors
                    [ error "Do not expose everything from Html.App"
                    ]
    , test "should report imports from sub-modules (multiple dots)" <|
        \() ->
            rule """
            import Html.Foo.Bar exposing (..)
            """
                |> expectErrors
                    [ error "Do not expose everything from Html.Foo.Bar"
                    ]
    , test "should not report when declaring a module that exposes everything" <|
        \() ->
            rule "module Main exposing (..)"
                |> expectErrors []
    ]


all : Test
all =
    describe "NoDuplicatedImports" tests
