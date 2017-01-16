port module NoUnannotatedFunctionTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import NoUnannotatedFunction exposing (rule)
import Types exposing (Error)


error : String -> Error
error =
    Error "NoUnannotatedFunction"


tests : List Test
tests =
    [ test "should not report constants that are annotated" <|
        \() ->
            rule """
            f : Int"
            f = 2
            """
                |> Expect.equal []
    , test "should not report functions that are annotated" <|
        \() ->
            rule """
            f : Int -> Int"
            f n = 2
            """
                |> Expect.equal []
    , test "should report constants that are not annotated" <|
        \() ->
            rule "f = 2"
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated" <|
        \() ->
            rule "f n = 2"
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated" <|
        \() ->
            rule "f n = 2"
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated when there are annotations" <|
        \() ->
            rule """
            f : Int -> Int
            g n = 3
            """
                |> Expect.equal [ error "`g` does not have a type declaration" ]
    , test "should report functions that are not annotated when there are other annotated functions" <|
        \() ->
            rule """
            f : Int -> Int
            f n = 2

            g n = 3
            """
                |> Expect.equal [ error "`g` does not have a type declaration" ]
    , test "should not functions declared in a `let` body" <|
        \() ->
            rule """
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
