port module NoDuplicateImportsTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import Lint.Rules.NoDuplicateImports exposing (rule)
import Lint.Types exposing (LintRule, Error)


error : String -> Error
error =
    Error "NoDuplicateImports"


tests : List Test
tests =
    [ test "should not report imports that are called only once" <|
        \() ->
            rule """
            import Regex
            import Http
            import Html exposing (..)
            import Lib exposing (a)
            """
                |> Expect.equal []
    , test "should report duplicated imports" <|
        \() ->
            rule """
            import Regex
            import Regex
            import Http
            """
                |> Expect.equal [ error "Regex was imported several times" ]
    , test "should report duplicated imports when several modules are imported twice" <|
        \() ->
            rule """
            import Regex
            import Regex
            import Http
            import Http
            """
                |> Expect.equal
                    [ error "Http was imported several times"
                    , error "Regex was imported several times"
                    ]
    , test "should only report duplicated imports once" <|
        \() ->
            rule """
            import Regex
            import Regex
            import Regex
            """
                |> Expect.equal [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (1)" <|
        \() ->
            rule """
            import Regex
            import Regex exposing (a)
            """
                |> Expect.equal [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (2)" <|
        \() ->
            rule """
            import Regex
            import Regex exposing (..)
            """
                |> Expect.equal [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (3)" <|
        \() ->
            rule """
            import Regex exposing (..)
            import Regex exposing (a)
            """
                |> Expect.equal [ error "Regex was imported several times" ]
    , test "should report duplicated imports even if they do not import the same thing (4)" <|
        \() ->
            rule """
            import Regex exposing (a)
            import Regex exposing (b)
            """
                |> Expect.equal [ error "Regex was imported several times" ]
    , test "should report duplicated submodule imports" <|
        \() ->
            rule """
            import Html.App
            import Html.App
            """
                |> Expect.equal [ error "Html.App was imported several times" ]
    , test "should not report the import of a module and its submodule" <|
        \() ->
            rule """
            import Html
            import Html.App
            """
                |> Expect.equal []
    ]


all : Test
all =
    describe "NoDuplicatedImports" tests
