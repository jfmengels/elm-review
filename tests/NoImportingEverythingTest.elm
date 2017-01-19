port module NoImportingEverythingTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import NoImportingEverything exposing (rule)
import Types exposing (Error)


error : String -> Error
error =
    Error "NoImportingEverything"


tests : List Test
tests =
    [ test "should not report imports that do not expose anything" <|
        \() ->
            rule """
            import Html
            import Http
            """
                |> Expect.equal []
    , test "should not report imports that expose functions by name" <|
        \() ->
            rule """
            import Html exposing (a)
            import Http exposing (a, b)
            """
                |> Expect.equal []
    , test "should report imports that expose everything" <|
        \() ->
            rule """
            import Html exposing (..)
            """
                |> Expect.equal
                    [ error "Do not expose everything from Html"
                    ]
    , test "should report imports from sub-modules" <|
        \() ->
            rule """
            import Html.App exposing (..)
            """
                |> Expect.equal
                    [ error "Do not expose everything from Html.App"
                    ]
    , test "should report imports from sub-modules (multiple dots)" <|
        \() ->
            rule """
            import Html.Foo.Bar exposing (..)
            """
                |> Expect.equal
                    [ error "Do not expose everything from Html.Foo.Bar"
                    ]
    , test "should not report when declaring a module that exposes everything" <|
        \() ->
            rule "module Main exposing (..)"
                |> Expect.equal []
    ]


all : Test
all =
    describe "NoDuplicatedImports" tests
