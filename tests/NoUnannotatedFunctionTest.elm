port module NoUnannotatedFunctionTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import TestUtil exposing (ruleTester)
import Lint.Rules.NoUnannotatedFunction exposing (rule)
import Lint.Types exposing (LintRule, Error)


error : String -> Error
error =
    Error "NoUnannotatedFunction"


testRule =
    ruleTester rule


tests : List Test
tests =
    [ test "should not report constants that are annotated" <|
        \() ->
            testRule """
              f : Int"
              f = 2
            """
                |> Expect.equal []
    , test "should not report functions that are annotated" <|
        \() ->
            testRule """
              f : Int -> Int"
              f n = 2
            """
                |> Expect.equal []
    , test "should report constants that are not annotated" <|
        \() ->
            testRule "f = 2"
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated" <|
        \() ->
            testRule "f n = 2"
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated" <|
        \() ->
            testRule "f n = 2"
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated when there are annotations" <|
        \() ->
            testRule """
              f : Int -> Int
              g n = 3
            """
                |> Expect.equal [ error "`g` does not have a type declaration" ]
    , test "should report functions that are not annotated when there are other annotated functions" <|
        \() ->
            testRule """
              f : Int -> Int
              f n = 2

              g n = 3
            """
                |> Expect.equal [ error "`g` does not have a type declaration" ]
    , test "should not functions declared in a `let` body" <|
        \() ->
            testRule """
              f : Int -> Int
              f n =
                let
                  a = 2
                in
                  a
            """
                |> Expect.equal []
    ]


all : Test
all =
    describe "NoUnannotatedFunction" tests
