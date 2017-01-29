port module NoDebugTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import Lint.Rules.NoDebug exposing (rule)
import Lint.Types exposing (Error)


error : String -> Error
error =
    Error "NoDebug"


tests : List Test
tests =
    [ test "should not report normal function calls" <|
        \() ->
            rule """
            a = foo n
            b = bar.foo n
            c = debug
            e = debug.log n
            d = debug.log n
            """
                |> Expect.equal []
    , test "should report Debug.log use" <|
        \() ->
            rule "a = Debug.log"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug.log calls" <|
        \() ->
            rule "a = Debug.log z"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report multiple Debug.log calls" <|
        \() ->
            rule """
            a = Debug.log z
            b = Debug.log z
            """
                |> Expect.equal
                    [ error "Forbidden use of Debug"
                    , error "Forbidden use of Debug"
                    ]
    , test "should report Debug.crash calls" <|
        \() ->
            rule "a = Debug.crash 1"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report any Debug method" <|
        \() ->
            rule "a = Debug.foo 1"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a binary expression" <|
        \() ->
            rule "a = (Debug.log z) + 2"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a << binary expression" <|
        \() ->
            rule "a = fn << Debug.log"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a pipe expression" <|
        \() ->
            rule "a = fn |> Debug.log z"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an list expression" <|
        \() ->
            rule "a = [Debug.log z y]"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an record expression" <|
        \() ->
            rule "a = { foo = Debug.log z y }"
                |> Expect.equal [ error "Forbidden use of Debug" ]
      -- elm-ast doesn't handle record update expressions
      -- , test "should report Debug in an record update expression" <|
      --     \() ->
      --         rule "a = { model | foo = Debug.log z y }"
      --             |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an lambda expression" <|
        \() ->
            rule "a = (\\foo -> Debug.log z foo)"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression condition" <|
        \() ->
            rule "a = if Debug.log a b then True else False"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression then" <|
        \() ->
            rule "a = if True then Debug.log a b else False"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression else" <|
        \() ->
            rule "a = if True then True else Debug.log a b"
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case value" <|
        \() ->
            rule """
            a = case Debug.log a b of
              _ -> []
            """
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case pattern" <|
        \() ->
            rule """
            a = case a of
              Debug.log a b -> []
            """
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case body" <|
        \() ->
            rule """
            a = case a of
              _ -> Debug.log a b
            """
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in let declaration section" <|
        \() ->
            rule """
            a = let b = Debug.log a b
                in b
            """
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in let body" <|
        \() ->
            rule """
            a = let b = c
                in Debug.log a b
            """
                |> Expect.equal [ error "Forbidden use of Debug" ]
    ]


all : Test
all =
    describe "NoDebug" tests
