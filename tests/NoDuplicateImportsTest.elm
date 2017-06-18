module NoDuplicateImportsTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoDuplicateImports exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> LintError
error =
    LintError "NoDuplicateImports"


tests : List Test
tests =
    [ test "should not report imports that are called only once" <|
        \() ->
            testRule """
            import Regex
            import Http
            import Html exposing (..)
            import Lib exposing (a)
            """
                |> expectErrors []
    , test "should report duplicated imports" <|
        \() ->
            testRule """
            import Regex
            import Regex
            import Http
            """
                |> expectErrors [ error "Regex was imported several times" ]
    , test "should report duplicated imports when several modules are imported twice" <|
        \() ->
            testRule """
            import Regex
            import Regex
            import Http
            import Http
            """
                |> expectErrors
                    [ error "Http was imported several times"
                    , error "Regex was imported several times"
                    ]
    , test "should only report duplicated imports once" <|
        \() ->
            testRule """
            import Regex
            import Regex
            import Regex
            """
                |> expectErrors [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (1)" <|
        \() ->
            testRule """
            import Regex
            import Regex exposing (a)
            """
                |> expectErrors [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (2)" <|
        \() ->
            testRule """
            import Regex
            import Regex exposing (..)
            """
                |> expectErrors [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (3)" <|
        \() ->
            testRule """
            import Regex exposing (..)
            import Regex exposing (a)
            """
                |> expectErrors [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (4)" <|
        \() ->
            testRule """
            import Regex exposing (a)
            import Regex exposing (b)
            """
                |> expectErrors [ error "Regex was imported several times" ]
    , test "should report duplicated submodule imports" <|
        \() ->
            testRule """
            import Html.App
            import Html.App
            """
                |> expectErrors [ error "Html.App was imported several times" ]
    , test "should not report the import of a module and its submodule" <|
        \() ->
            testRule """
            import Html
            import Html.App
            """
                |> expectErrors []
    ]


all : Test
all =
    describe "NoDuplicateImports" tests
