module SimplifyPipingTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.SimplifyPiping exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> String -> LintError
error op fn =
    LintError "SimplifyPiping" ("Instead of `" ++ fn ++ " f " ++ op ++ " List.map g`, try " ++ fn ++ " (f " ++ op ++ " g)")


tests : List Test
tests =
    [ test "should not report piping of the result of different functions of the same module" <|
        \() ->
            testRule "a = b |> List.map f |> List.filter g"
                |> expectErrors []
    , test "should not report piping of the result of similarly named functions of different modules" <|
        \() ->
            testRule "a = b |> List.map f |> Set.map g"
                |> expectErrors []
    , test "should report piping of List.map" <|
        \() ->
            testRule "a = b |> List.map f |> List.map g"
                |> expectErrors [ error ">>" "List.map" ]
    , test "should report piping of Set.map" <|
        \() ->
            testRule "a = b |> Set.map f |> Set.map g"
                |> expectErrors [ error ">>" "Set.map" ]
    , test "should report piping of Array.map" <|
        \() ->
            testRule "a = b |> Array.map f |> Array.map g"
                |> expectErrors [ error ">>" "Array.map" ]
    , test "should report piping of the result of Array.indexedMap" <|
        \() ->
            testRule "a = b |> Array.indexedMap f |> Array.indexedMap g"
                |> expectErrors [ error ">>" "Array.indexedMap" ]
    , test "should not report piping of the incomplete simplifiable function call" <|
        \() ->
            testRule "a = List.map f |> List.map g"
                |> expectErrors []
    , test "should report piping the complete call into the same function" <|
        \() ->
            testRule "a = List.map f data |> List.map g"
                |> expectErrors [ error ">>" "List.map" ]
    , test "should report piping the complete call into the same function with an additional pipe at the end" <|
        \() ->
            testRule "a = List.map f data |> List.map g |> foo"
                |> expectErrors [ error ">>" "List.map" ]
    , test "should not report the use of a single simplifiable function" <|
        \() ->
            testRule "a = List.map f"
                |> expectErrors []
    , test "should not report the use of a single simplifiable function with piped functions as the argument" <|
        \() ->
            testRule "a = List.map (f >> g)"
                |> expectErrors []
    , test "should not report any piping of similar methods" <|
        \() ->
            testRule "a = List.foo fn |> List.foo fn2"
                |> expectErrors []
    , test "should not report direct piping of List.map and friends" <|
        \() ->
            testRule """
            a = List.map |> List.map
            b = List.map >> List.map
            c = Set.map |> Set.map
            d = Set.map >> Set.map
            """
                |> expectErrors []
    , test "should report piping right to left" <|
        \() ->
            testRule "a = List.map f <| List.map g <| b"
                |> expectErrors [ error "<<" "List.map" ]
    , test "should report piping right to left with complete call at the right side" <|
        \() ->
            testRule "a = List.map f <| List.map g data"
                |> expectErrors [ error "<<" "List.map" ]
    , test "should report piping right to left with complete call at the right side and additional pipe on the left" <|
        \() ->
            testRule "a = a <| List.map f <| List.map g data"
                |> expectErrors [ error "<<" "List.map" ]
    , test "should report piping the functions directly" <|
        \() ->
            testRule "a = List.map f >> List.map g"
                |> expectErrors [ error ">>" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the left" <|
        \() ->
            testRule "a = foo >> List.map f >> List.map g"
                |> expectErrors [ error ">>" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the right" <|
        \() ->
            testRule "a = List.map f >> List.map g >> bar"
                |> expectErrors [ error ">>" "List.map" ]
    , test "should report piping the functions directly with additional pipe on both sides" <|
        \() ->
            testRule "a = foo >> List.map f >> List.map g >> bar"
                |> expectErrors [ error ">>" "List.map" ]
    , test "should report piping the functions directly, right to left" <|
        \() ->
            testRule "a = List.map f << List.map g"
                |> expectErrors [ error "<<" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the left, right to left" <|
        \() ->
            testRule "a = foo << List.map f << List.map g"
                |> expectErrors [ error "<<" "List.map" ]
    , test "should report piping the functions directly with additional pipe on the right, right to left" <|
        \() ->
            testRule "a = List.map f << List.map g << bar"
                |> expectErrors [ error "<<" "List.map" ]
    , test "should report piping the functions directly with additional pipe on both sides, right to left" <|
        \() ->
            testRule "a = foo << List.map f << List.map g << bar"
                |> expectErrors [ error "<<" "List.map" ]
    ]


all : Test
all =
    describe "SimplifyPiping" tests
