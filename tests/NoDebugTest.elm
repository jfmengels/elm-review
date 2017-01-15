port module NoDebugTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import NoDebug exposing (rule)
import Types exposing (Error)


error : String -> Error
error =
    Error "NoDebug"


tests : List Test
tests =
    [ test "should not report normal function calls" <|
        \() ->
            """
            a = foo n
            b = bar.foo n
            c = debug
            e = debug.log n
            d = debug.log n
            """
                |> rule
                |> Expect.equal []
    , test "should report Debug.log use" <|
        \() ->
            "a = Debug.log"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug.log calls" <|
        \() ->
            "a = Debug.log z"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report multiple Debug.log calls" <|
        \() ->
            """
            a = Debug.log z
            b = Debug.log z
            """
                |> rule
                |> Expect.equal
                    [ error "Forbidden use of Debug"
                    , error "Forbidden use of Debug"
                    ]
    , test "should report Debug.crash calls" <|
        \() ->
            "a = Debug.crash 1"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report any Debug method" <|
        \() ->
            "a = Debug.foo 1"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a binary expression" <|
        \() ->
            "a = (Debug.log z) + 2"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a << binary expression" <|
        \() ->
            "a = fn << Debug.log"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a pipe expression" <|
        \() ->
            "a = fn |> Debug.log z"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an list expression" <|
        \() ->
            "a = [Debug.log z y]"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an record expression" <|
        \() ->
            "a = { foo = Debug.log z y }"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
      -- elm-ast doesn't handle record update expressions
      -- , test "should report Debug in an record update expression" <|
      --     \() ->
      --         "a = { model | foo = Debug.log z y }"
      --             |> rule
      --             |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an lambda expression" <|
        \() ->
            "a = (\\foo -> Debug.log z foo)"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression condition" <|
        \() ->
            "a = if Debug.log a b then True else False"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression then" <|
        \() ->
            "a = if True then Debug.log a b else False"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in an if expression else" <|
        \() ->
            "a = if True then True else Debug.log a b"
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case value" <|
        \() ->
            """
            a = case Debug.log a b of
              _ -> []
            """
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case pattern" <|
        \() ->
            """
            a = case a of
              Debug.log a b -> []
            """
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in a case body" <|
        \() ->
            """
            a = case a of
              _ -> Debug.log a b
            """
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in let declaration section" <|
        \() ->
            """
            a = let b = Debug.log a b
                in b
            """
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    , test "should report Debug in let body" <|
        \() ->
            """
            a = let b = c
                in Debug.log a b
            """
                |> rule
                |> Expect.equal [ error "Forbidden use of Debug" ]
    ]


all : Test
all =
    describe "NoDebug" tests
