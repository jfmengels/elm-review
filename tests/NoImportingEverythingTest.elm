module NoImportingEverythingTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint exposing (Rule)
import Lint.Error as Error exposing (Error, LintResult)
import Lint.Rule.NoImportingEverything exposing (Configuration, rule)
import Test exposing (Test, describe, test)
import TestUtil


testRule : Configuration -> String -> LintResult
testRule options =
    TestUtil.ruleTester (rule options)


error : String -> Error
error message =
    TestUtil.errorWithoutRange message


tests : List Test
tests =
    [ test "should not report imports that do not expose anything" <|
        \() ->
            """module A exposing (..)
import Html
import Http
"""
                |> testRule { exceptions = [] }
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report imports that expose functions by name" <|
        \() ->
            """module A exposing (..)
import Html exposing (a)
import Http exposing (a, b)
"""
                |> testRule { exceptions = [] }
                |> TestUtil.expectErrorsWithoutRange []
    , test "should report imports that expose everything" <|
        \() ->
            """module A exposing (..)
import Html exposing (..)
"""
                |> testRule { exceptions = [] }
                |> TestUtil.expectErrorsWithoutRange
                    [ error "Do not expose everything from Html" ]
    , test "should report imports from sub-modules" <|
        \() ->
            """module A exposing (..)
import Html.App exposing (..)
"""
                |> testRule { exceptions = [] }
                |> TestUtil.expectErrorsWithoutRange [ error "Do not expose everything from Html.App" ]
    , test "should report imports from sub-modules (multiple dots)" <|
        \() ->
            """module A exposing (..)
import Html.Foo.Bar exposing (..)
"""
                |> testRule { exceptions = [] }
                |> TestUtil.expectErrorsWithoutRange [ error "Do not expose everything from Html.Foo.Bar" ]
    , test "should not report imports that expose everything that are in the exception list" <|
        \() ->
            """module A exposing (..)
import Html exposing (..)
"""
                |> testRule { exceptions = [ "Html" ] }
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report imports from sub-modules that are in the exception list" <|
        \() ->
            """module A exposing (..)
import Html.App exposing (..)
"""
                |> testRule { exceptions = [ "Html.App" ] }
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report imports from sub-modules (multiple dots)" <|
        \() ->
            """module A exposing (..)
import Html.Foo.Bar exposing (..)
"""
                |> testRule { exceptions = [ "Html.Foo.Bar" ] }
                |> TestUtil.expectErrorsWithoutRange []
    , test "should report imports whose parent is ignored" <|
        \() ->
            """module A exposing (..)
import Html.Foo.Bar exposing (..)
"""
                |> testRule { exceptions = [ "Html" ] }
                |> TestUtil.expectErrorsWithoutRange [ error "Do not expose everything from Html.Foo.Bar" ]
    , test "should report imports whose sub-module is ignored" <|
        \() ->
            """module A exposing (..)
import Html exposing (..)
"""
                |> testRule { exceptions = [ "Html.App" ] }
                |> TestUtil.expectErrorsWithoutRange [ error "Do not expose everything from Html" ]
    ]


all : Test
all =
    describe "NoImportingEverything" tests
