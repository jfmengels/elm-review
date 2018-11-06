module NoDebugTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint.Rules.NoDebug exposing (rule)
import Lint.Types exposing (LintError, LintResult, LintRule)
import Test exposing (Test, describe, only, test)
import TestUtil exposing (expectErrors, ruleTester)


testRule : String -> LintResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> ruleTester rule


error : String -> Range -> LintError
error =
    LintError "NoDebug"


location : Int -> Int -> Int -> Range
location row columnStart columnEnd =
    { start = { row = row, column = columnStart }
    , end = { row = row, column = columnEnd }
    }


tests : List Test
tests =
    [ test "should not report normal function calls" <|
        \() ->
            testRule """
a = foo n
b = bar.foo n
c = debug
e = debug.log n
d = debug.log n
            """
                |> expectErrors []
    , test "should report Debug.log use" <|
        \() ->
            testRule "a = Debug.log"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 5 14) ]
    , test "should report Debug.log calls" <|
        \() ->
            testRule "a = Debug.log z"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 5 14) ]
    , test "should report multiple Debug.log calls" <|
        \() ->
            testRule """
a = Debug.log z
b = Debug.log z
            """
                |> expectErrors
                    [ error "Forbidden use of Debug" (location 4 5 14)
                    , error "Forbidden use of Debug" (location 5 5 14)
                    ]
    , test "should report Debug.crash calls" <|
        \() ->
            testRule "a = Debug.crash 1"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 5 16) ]
    , test "should report any Debug method" <|
        \() ->
            testRule "a = Debug.foo 1"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 5 14) ]
    , test "should report Debug in a binary expression" <|
        \() ->
            testRule "a = (Debug.log z) + 2"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 6 15) ]
    , test "should report Debug in a << binary expression" <|
        \() ->
            testRule "a = fn << Debug.log"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 11 20) ]
    , test "should report Debug in a pipe expression" <|
        \() ->
            testRule "a = fn |> Debug.log z"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 11 20) ]
    , test "should report Debug in an list expression" <|
        \() ->
            testRule "a = [Debug.log z y]"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 6 15) ]
    , test "should report Debug in an record expression" <|
        \() ->
            testRule "a = { foo = Debug.log z y }"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 13 22) ]
    , test "should report Debug in an record update expression" <|
        \() ->
            testRule "a = { model | foo = Debug.log z y }"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 21 30) ]
    , test "should report Debug in an lambda expression" <|
        \() ->
            testRule "a = (\\foo -> Debug.log z foo)"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 14 23) ]
    , test "should report Debug in an if expression condition" <|
        \() ->
            testRule "a = if Debug.log a b then True else False"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 8 17) ]
    , test "should report Debug in an if expression then branch" <|
        \() ->
            testRule "a = if True then Debug.log a b else False"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 18 27) ]
    , test "should report Debug in an if expression else branch" <|
        \() ->
            testRule "a = if True then True else Debug.log a b"
                |> expectErrors [ error "Forbidden use of Debug" (location 3 28 37) ]
    , test "should report Debug in a case value" <|
        \() ->
            testRule """
a = case Debug.log a b of
  _ -> []
            """
                |> expectErrors [ error "Forbidden use of Debug" (location 4 10 19) ]
    , test "should report Debug in a case body" <|
        \() ->
            testRule """
a = case a of
  _ -> Debug.log a b
            """
                |> expectErrors [ error "Forbidden use of Debug" (location 5 8 17) ]
    , test "should report Debug in let declaration section" <|
        \() ->
            testRule """
a = let b = Debug.log a b
    in b
            """
                |> expectErrors [ error "Forbidden use of Debug" (location 4 13 22) ]
    , test "should report Debug in let body" <|
        \() ->
            testRule """
a = let b = c
    in Debug.log a b
            """
                |> expectErrors [ error "Forbidden use of Debug" (location 5 8 17) ]
    ]


all : Test
all =
    describe "NoDebug" tests
