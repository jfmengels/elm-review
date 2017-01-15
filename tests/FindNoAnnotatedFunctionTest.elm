port module FindNoAnnotatedFunctionTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import FindNoAnnotatedFunction exposing (rule)
import Types exposing (Error)


error : String -> Error
error =
    Error "FindNoAnnotatedFunction"


tests : List Test
tests =
    [ test "should not report constants that are annotated" <|
        \() ->
            """
            f : Int"
            f = 2
            """
                |> rule
                |> Expect.equal []
    , test "should not report functions that are annotated" <|
        \() ->
            """
            f : Int -> Int"
            f n = 2
            """
                |> rule
                |> Expect.equal []
    , test "should report constants that are not annotated" <|
        \() ->
            "f = 2"
                |> rule
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated" <|
        \() ->
            "f n = 2"
                |> rule
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated" <|
        \() ->
            "f n = 2"
                |> rule
                |> Expect.equal [ error "`f` does not have a type declaration" ]
    , test "should report functions that are not annotated when there are annotations" <|
        \() ->
            """
            f : Int -> Int
            g n = 3
            """
                |> rule
                |> Expect.equal [ error "`g` does not have a type declaration" ]
    , test "should report functions that are not annotated when there are other annotated functions" <|
        \() ->
            """
            f : Int -> Int
            f n = 2

            g n = 3
            """
                |> rule
                |> Expect.equal [ error "`g` does not have a type declaration" ]
    , test "should not functions declared in a `let` body" <|
        \() ->
            """
            f : Int -> Int
            f n =
              let
                a = 2
              in
                a
            """
                |> rule
                |> Expect.equal []
    ]


all : Test
all =
    describe "FindNoAnnotatedFunction" tests
