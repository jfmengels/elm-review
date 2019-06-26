module NoDebugTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint.Error as Error exposing (Error)
import Lint.Rule exposing (Rule)
import Lint.Rule.NoDebug exposing (rule)
import Lint.Test exposing (LintResult)
import Test exposing (Test, describe, test)


testRule : String -> LintResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Lint.Test.ruleTester rule


error : Range -> Error
error range =
    Error.create "Forbidden use of Debug" range


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
                |> Lint.Test.expectErrors []
    , test "should report Debug.log use" <|
        \() ->
            testRule "a = Debug.log"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 5 ) ( 3, 14 )) ]
    , test "should report Debug.log calls" <|
        \() ->
            testRule "a = Debug.log z"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 5 ) ( 3, 14 )) ]
    , test "should report multiple Debug.log calls" <|
        \() ->
            testRule """
a = Debug.log z
b = Debug.log z
            """
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 4, 5 ) ( 4, 14 ))
                    , error (Lint.Test.location ( 5, 5 ) ( 5, 14 ))
                    ]
    , test "should report Debug.crash calls" <|
        \() ->
            testRule "a = Debug.crash 1"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 5 ) ( 3, 16 )) ]
    , test "should report any Debug method" <|
        \() ->
            testRule "a = Debug.foo 1"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 5 ) ( 3, 14 )) ]
    , test "should report Debug in a binary expression" <|
        \() ->
            testRule "a = (Debug.log z) + 2"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 6 ) ( 3, 15 )) ]
    , test "should report Debug in a << binary expression" <|
        \() ->
            testRule "a = fn << Debug.log"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 11 ) ( 3, 20 )) ]
    , test "should report Debug in a pipe expression" <|
        \() ->
            testRule "a = fn |> Debug.log z"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 11 ) ( 3, 20 )) ]
    , test "should report Debug in an list expression" <|
        \() ->
            testRule "a = [Debug.log z y]"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 6 ) ( 3, 15 )) ]
    , test "should report Debug in an record expression" <|
        \() ->
            testRule "a = { foo = Debug.log z y }"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 13 ) ( 3, 22 )) ]
    , test "should report Debug in an record update expression" <|
        \() ->
            testRule "a = { model | foo = Debug.log z y }"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 21 ) ( 3, 30 )) ]
    , test "should report Debug in an lambda expression" <|
        \() ->
            testRule "a = (\\foo -> Debug.log z foo)"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 14 ) ( 3, 23 )) ]
    , test "should report Debug in an if expression condition" <|
        \() ->
            testRule "a = if Debug.log a b then True else False"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 8 ) ( 3, 17 )) ]
    , test "should report Debug in an if expression then branch" <|
        \() ->
            testRule "a = if True then Debug.log a b else False"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 18 ) ( 3, 27 )) ]
    , test "should report Debug in an if expression else branch" <|
        \() ->
            testRule "a = if True then True else Debug.log a b"
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 3, 28 ) ( 3, 37 )) ]
    , test "should report Debug in a case value" <|
        \() ->
            testRule """
a = case Debug.log a b of
  _ -> []
            """
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 4, 10 ) ( 4, 19 )) ]
    , test "should report Debug in a case body" <|
        \() ->
            testRule """
a = case a of
  _ -> Debug.log a b
            """
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 5, 8 ) ( 5, 17 )) ]
    , test "should report Debug in let declaration section" <|
        \() ->
            testRule """
a = let b = Debug.log a b
    in b
            """
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 4, 13 ) ( 4, 22 )) ]
    , test "should report Debug in let body" <|
        \() ->
            testRule """
a = let b = c
    in Debug.log a b
            """
                |> Lint.Test.expectErrors
                    [ error (Lint.Test.location ( 5, 8 ) ( 5, 17 )) ]
    ]


all : Test
all =
    describe "NoDebug" tests
