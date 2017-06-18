module NoDebugTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoDebug exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> LintError
error =
    LintError "NoDebug"


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
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug.log calls" <|
        \() ->
            testRule "a = Debug.log z"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report multiple Debug.log calls" <|
        \() ->
            testRule """
            a = Debug.log z
            b = Debug.log z
            """
                |> expectErrors
                    [ error "Forbidden use of Debug"
                    , error "Forbidden use of Debug"
                    ]
    , test "should report Debug.crash calls" <|
        \() ->
            testRule "a = Debug.crash 1"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report any Debug method" <|
        \() ->
            testRule "a = Debug.foo 1"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in a binary expression" <|
        \() ->
            testRule "a = (Debug.log z) + 2"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in a << binary expression" <|
        \() ->
            testRule "a = fn << Debug.log"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in a pipe expression" <|
        \() ->
            testRule "a = fn |> Debug.log z"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in an list expression" <|
        \() ->
            testRule "a = [Debug.log z y]"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in an record expression" <|
        \() ->
            testRule "a = { foo = Debug.log z y }"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in an record update expression" <|
        \() ->
            testRule "a = { model | foo = Debug.log z y }"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in an lambda expression" <|
        \() ->
            testRule "a = (\\foo -> Debug.log z foo)"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression condition" <|
        \() ->
            testRule "a = if Debug.log a b then True else False"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression then" <|
        \() ->
            testRule "a = if True then Debug.log a b else False"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression else" <|
        \() ->
            testRule "a = if True then True else Debug.log a b"
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case value" <|
        \() ->
            testRule """
            a = case Debug.log a b of
              _ -> []
            """
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case pattern" <|
        \() ->
            testRule """
            a = case a of
              Debug.log a b -> []
            """
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case body" <|
        \() ->
            testRule """
            a = case a of
              _ -> Debug.log a b
            """
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in let declaration section" <|
        \() ->
            testRule """
            a = let b = Debug.log a b
                in b
            """
                |> expectErrors [ error "Forbidden use of Debug" ]
    , test "should report Debug in let body" <|
        \() ->
            testRule """
            a = let b = c
                in Debug.log a b
            """
                |> expectErrors [ error "Forbidden use of Debug" ]
    ]


all : Test
all =
    describe "NoDebug" tests
