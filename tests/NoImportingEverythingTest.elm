module NoImportingEverythingTest exposing (all)

import Lint.Rule.NoImportingEverything exposing (Configuration, rule)
import Lint.Test2 exposing (LintResult)
import Test exposing (Test, describe, test)


testRule : Configuration -> String -> LintResult
testRule options =
    Lint.Test2.run (rule options)


tests : List Test
tests =
    [ test "should not report imports that do not expose anything" <|
        \() ->
            """module A exposing (..)
import Html
import Http"""
                |> testRule { exceptions = [] }
                |> Lint.Test2.expectNoErrors
    , test "should not report imports that expose functions by name" <|
        \() ->
            """module A exposing (..)
import Html exposing (a)
import Http exposing (a, b)"""
                |> testRule { exceptions = [] }
                |> Lint.Test2.expectNoErrors
    , test "should report imports that expose everything" <|
        \() ->
            """module A exposing (..)
import Html exposing (..)"""
                |> testRule { exceptions = [] }
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = "Do not expose everything from Html"
                        , under = ".."
                        }
                        |> Lint.Test2.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 25 } }
                    ]
    , test "should report imports from sub-modules" <|
        \() ->
            """module A exposing (a)
import Html.App exposing (..)"""
                |> testRule { exceptions = [] }
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = "Do not expose everything from Html.App"
                        , under = ".."
                        }
                    ]
    , test "should report imports from sub-modules (multiple dots)" <|
        \() ->
            """module A exposing (a)
import Html.Foo.Bar exposing (..)"""
                |> testRule { exceptions = [] }
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = "Do not expose everything from Html.Foo.Bar"
                        , under = ".."
                        }
                    ]
    , test "should not report imports that expose everything that are in the exception list" <|
        \() ->
            """module A exposing (a)
import Html exposing (..)"""
                |> testRule { exceptions = [ "Html" ] }
                |> Lint.Test2.expectNoErrors
    , test "should not report imports from sub-modules that are in the exception list" <|
        \() ->
            """module A exposing (a)
import Html.App exposing (..)"""
                |> testRule { exceptions = [ "Html.App" ] }
                |> Lint.Test2.expectNoErrors
    , test "should not report imports from sub-modules (multiple dots)" <|
        \() ->
            """module A exposing (a)
import Html.Foo.Bar exposing (..)"""
                |> testRule { exceptions = [ "Html.Foo.Bar" ] }
                |> Lint.Test2.expectNoErrors
    , test "should report imports whose parent is ignored" <|
        \() ->
            """module A exposing (a)
import Html.Foo.Bar exposing (..)"""
                |> testRule { exceptions = [ "Html" ] }
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = "Do not expose everything from Html.Foo.Bar"
                        , under = ".."
                        }
                    ]
    , test "should report imports whose sub-module is ignored" <|
        \() ->
            """module A exposing (a)
import Html exposing (..)"""
                |> testRule { exceptions = [ "Html.App" ] }
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = "Do not expose everything from Html"
                        , under = ".."
                        }
                    ]
    ]


all : Test
all =
    describe "NoImportingEverything" tests
