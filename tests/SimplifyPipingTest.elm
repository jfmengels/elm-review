port module SimplifyPipingTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import Lint.Rules.SimplifyPiping exposing (rule)
import Lint.Types exposing (Error)


error : String -> String -> Error
error op fn =
    Error "SimplifyPiping" ("Instead of `" ++ fn ++ " f " ++ op ++ " List.map g`, try " ++ fn ++ " (f " ++ op ++ " g)")


tests : List Test
tests =
    [ test "should not report piping of the result of different functions of the same module" <|
        \() ->
            rule "a = b |> List.map f |> List.filter g"
                |> Expect.equal []
    , test "should not report piping of the result of similarly named functions of different modules" <|
        \() ->
            rule "a = b |> List.map f |> Set.map g"
                |> Expect.equal []
    , test "should report piping of List.map" <|
        \() ->
            rule "a = b |> List.map f |> List.map g"
                |> Expect.equal [ error ">>" "List.map" ]
    , test "should report piping of Set.map" <|
        \() ->
            rule "a = b |> Set.map f |> Set.map g"
                |> Expect.equal [ error ">>" "Set.map" ]
    , test "should report piping of Array.map" <|
        \() ->
            rule "a = b |> Array.map f |> Array.map g"
                |> Expect.equal [ error ">>" "Array.map" ]
    , test "should report piping of the result of Array.indexedMap" <|
        \() ->
            rule "a = b |> Array.indexedMap f |> Array.indexedMap g"
                |> Expect.equal [ error ">>" "Array.indexedMap" ]
    , test "should not report piping of the incomplete simplifiable function call" <|
        \() ->
            rule "a = List.map f |> List.map g"
                |> Expect.equal []
    , test "should not report piping the function in itself" <|
        \() ->
            rule "a = List.map f |> List.map g"
                |> Expect.equal []
    , test "should report piping the complete call into the same function" <|
        \() ->
            rule "a = List.map f data |> List.map g"
                |> Expect.equal [ error ">>" "List.map" ]
    , test "should report piping the complete call into the same function with an additional pipe at the end" <|
        \() ->
            rule "a = List.map f data |> List.map g |> foo"
                |> Expect.equal [ error ">>" "List.map" ]
    , test "should not report the use of a single simplifiable function" <|
        \() ->
            rule "a = List.map f"
                |> Expect.equal []
    , test "should not report the use of a single simplifiable function with piped functions as the argument" <|
        \() ->
            rule "a = List.map (f >> g)"
                |> Expect.equal []
    , test "should not report any piping of similar methods" <|
        \() ->
            rule "a = List.foo fn |> List.foo fn2"
                |> Expect.equal []
    , test "should not report direct piping of List.map and friends" <|
        \() ->
            rule """
            a = List.map |> List.map
            b = List.map >> List.map
            c = Set.map |> Set.map
            d = Set.map >> Set.map
            """
                |> Expect.equal []
    , test "should report piping right to left" <|
        \() ->
            rule "a = List.map f <| List.map g <| b"
                |> Expect.equal [ error "<<" "List.map" ]
    , test "should report piping right to left with complete call at the right side" <|
        \() ->
            rule "a = List.map f <| List.map g data"
                |> Expect.equal [ error "<<" "List.map" ]
    , test "should report piping right to left with complete call at the right side and additional pipe on the left" <|
        \() ->
            rule "a = a <| List.map f <| List.map g data"
                |> Expect.equal [ error "<<" "List.map" ]
    , test "should report piping the functions directly" <|
        \() ->
            rule "a = List.map f >> List.map g"
                |> Expect.equal [ error ">>" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the left" <|
        \() ->
            rule "a = foo >> List.map f >> List.map g"
                |> Expect.equal [ error ">>" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the right" <|
        \() ->
            rule "a = List.map f >> List.map g >> bar"
                |> Expect.equal [ error ">>" "List.map" ]
    , test "should report piping the functions directly with additional pipe on both sides" <|
        \() ->
            rule "a = foo >> List.map f >> List.map g >> bar"
                |> Expect.equal [ error ">>" "List.map" ]
    , test "should report piping the functions directly, right to left" <|
        \() ->
            rule "a = List.map f << List.map g"
                |> Expect.equal [ error "<<" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the left, right to left" <|
        \() ->
            rule "a = foo << List.map f << List.map g"
                |> Expect.equal [ error "<<" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the right, right to left" <|
        \() ->
            rule "a = List.map f << List.map g << bar"
                |> Expect.equal [ error "<<" "List.map" ]
    , test "should report piping the functions directly with additional pipe on both sides, right to left" <|
        \() ->
            rule "a = foo << List.map f << List.map g << bar"
                |> Expect.equal [ error "<<" "List.map" ]
    ]


all : Test
all =
    describe "SimplifyPiping" tests
