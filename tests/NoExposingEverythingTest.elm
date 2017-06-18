module NoExposingEverythingTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoExposingEverything exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> LintError
error =
    LintError "NoExposingEverything"


tests : List Test
tests =
    [ test "should not report modules that do not have a module declaration" <|
        \() ->
            testRule "bar = 2"
                |> expectErrors []
    , test "should not report import statements that expose everything" <|
        \() ->
            testRule """module Foo exposing (foo)
            import Html exposing (..)
            """
                |> expectErrors []
    , test "should not report modules that expose discrete items (single item)" <|
        \() ->
            testRule "module Foo exposing (foo)"
                |> expectErrors []
    , test "should not report modules that expose discrete items (multiple items)" <|
        \() ->
            testRule "module Foo exposing (foo, bar)"
                |> expectErrors []
    , test "should not report port modules that expose discrete items (single item)" <|
        \() ->
            testRule "port module Foo exposing (foo)"
                |> expectErrors []
    , test "should not report port modules that expose discrete items (multiple items)" <|
        \() ->
            testRule "port module Foo exposing (foo, bar)"
                |> expectErrors []
    , test "should report modules that expose everything" <|
        \() ->
            testRule "module Foo exposing (..)"
                |> expectErrors [ error "Do not expose everything from module Foo using (..)" ]
    , test "should report port modules that expose everything" <|
        \() ->
            testRule "port module Foo exposing (..)"
                |> expectErrors [ error "Do not expose everything from module Foo using (..)" ]
    , test "should report modules with dotted names that expose everything" <|
        \() ->
            testRule "module Foo.Bar.Baz exposing (..)"
                |> expectErrors [ error "Do not expose everything from module Foo.Bar.Baz using (..)" ]
    , test "should report port modules with dotted names that expose everything" <|
        \() ->
            testRule "port module Foo.Bar.Baz exposing (..)"
                |> expectErrors [ error "Do not expose everything from module Foo.Bar.Baz using (..)" ]
    ]


all : Test
all =
    describe "NoExposingEverything" tests
