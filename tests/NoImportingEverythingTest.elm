module NoImportingEverythingTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoImportingEverything exposing (rule, Configuration)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : Configuration -> String -> LintResult
testRule options =
    ruleTester (rule options)


error : String -> LintError
error =
    LintError "NoImportingEverything"


tests : List Test
tests =
    [ test "should not report imports that do not expose anything" <|
        \() ->
            """
            import Html
            import Http
            """
                |> testRule { exceptions = [] }
                |> expectErrors []
    , test "should not report imports that expose functions by name" <|
        \() ->
            """
            import Html exposing (a)
            import Http exposing (a, b)
            """
                |> testRule { exceptions = [] }
                |> expectErrors []
    , test "should report imports that expose everything" <|
        \() ->
            "import Html exposing (..)"
                |> testRule { exceptions = [] }
                |> expectErrors
                    [ error "Do not expose everything from Html" ]
    , test "should report imports from sub-modules" <|
        \() ->
            "import Html.App exposing (..)"
                |> testRule { exceptions = [] }
                |> expectErrors [ error "Do not expose everything from Html.App" ]
    , test "should report imports from sub-modules (multiple dots)" <|
        \() ->
            "import Html.Foo.Bar exposing (..)"
                |> testRule { exceptions = [] }
                |> expectErrors [ error "Do not expose everything from Html.Foo.Bar" ]
    , test "should not report imports that expose everything that are in the exception list" <|
        \() ->
            "import Html exposing (..)"
                |> testRule { exceptions = [ "Html" ] }
                |> expectErrors []
    , test "should not report imports from sub-modules that are in the exception list" <|
        \() ->
            "import Html.App exposing (..)"
                |> testRule { exceptions = [ "Html.App" ] }
                |> expectErrors []
    , test "should not report imports from sub-modules (multiple dots)" <|
        \() ->
            "import Html.Foo.Bar exposing (..)"
                |> testRule { exceptions = [ "Html.Foo.Bar" ] }
                |> expectErrors []
    , test "should not report when declaring a module that exposes everything" <|
        \() ->
            "module Main exposing (..)"
                |> testRule { exceptions = [] }
                |> expectErrors []
    ]


all : Test
all =
    describe "NoImportingEverything" tests
