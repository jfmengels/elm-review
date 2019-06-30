module NoDebugTest exposing (all)

import Lint.Rule.NoDebug exposing (rule)
import Lint.Test2 exposing (LintResult)
import Test exposing (Test, describe, test)


testRule : String -> LintResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Lint.Test2.run rule


message : String
message =
    "Forbidden use of Debug"


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
                |> Lint.Test2.expectNoErrors
    , test "should report Debug.log use" <|
        \() ->
            testRule "a = Debug.log"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug.log calls" <|
        \() ->
            testRule "a = Debug.log z"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report multiple Debug.log calls" <|
        \() ->
            testRule """
a = Debug.log z
b = Debug.log z
            """
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                        |> Lint.Test2.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
                    , Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                        |> Lint.Test2.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                    ]
    , test "should report Debug.crash calls" <|
        \() ->
            testRule "a = Debug.crash 1"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.crash"
                        }
                    ]
    , test "should report any Debug method" <|
        \() ->
            testRule "a = Debug.foo 1"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.foo"
                        }
                    ]
    , test "should report Debug in a binary expression" <|
        \() ->
            testRule "a = ( Debug.log z ) + 2"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a << binary expression" <|
        \() ->
            testRule "a = fn << Debug.log"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a pipe expression" <|
        \() ->
            testRule "a = fn |> Debug.log z"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an list expression" <|
        \() ->
            testRule "a = [ Debug.log z y ]"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an record expression" <|
        \() ->
            testRule "a = { foo = Debug.log z y }"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an record update expression" <|
        \() ->
            testRule "a = { model | foo = Debug.log z y }"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an lambda expression" <|
        \() ->
            testRule "a = (\\foo -> Debug.log z foo)"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an if expression condition" <|
        \() ->
            testRule "a = if Debug.log a b then True else False"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an if expression then branch" <|
        \() ->
            testRule "a = if True then Debug.log a b else False"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an if expression else branch" <|
        \() ->
            testRule "a = if True then True else Debug.log a b"
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a case value" <|
        \() ->
            testRule """
a = case Debug.log a b of
  _ -> []
            """
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a case body" <|
        \() ->
            testRule """
a = case a of
  _ -> Debug.log a b
            """
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in let declaration section" <|
        \() ->
            testRule """
a = let b = Debug.log a b
    in b
            """
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in let body" <|
        \() ->
            testRule """
a = let b = c
    in Debug.log a b
            """
                |> Lint.Test2.expectErrors
                    [ Lint.Test2.error
                        { message = message
                        , under = "Debug.log"
                        }
                    ]
    ]


all : Test
all =
    describe "NoDebug" tests
