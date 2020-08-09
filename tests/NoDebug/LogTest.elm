module NoDebug.LogTest exposing (all)

import NoDebug.Log exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


message : String
message =
    "Remove the use of `Debug.log` before shipping to production"


details : List String
details =
    [ "`Debug.log` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
    ]


all : Test
all =
    describe "NoDebug.Log"
        [ test "should not report normal function calls" <|
            \() ->
                testRule """
a = foo n
b = bar.foo n
c = debug
e = debug.log n
d = debug.log n
            """
                    |> Review.Test.expectNoErrors
        , test "should not report Debug.todo or Debug.toString calls" <|
            \() ->
                testRule """
a = Debug.toString n
b = Debug.todo ""
"""
                    |> Review.Test.expectNoErrors
        , test "should report Debug.log use" <|
            \() ->
                testRule "a = Debug.log"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log calls" <|
            \() ->
                testRule "a = Debug.log z"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report multiple Debug.log calls" <|
            \() ->
                testRule """
a = Debug.log z
b = Debug.log z
            """
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
                        , Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                        ]
        , test "should report Debug.log in a binary expression" <|
            \() ->
                testRule "a = ( Debug.log z ) + 2"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in a << binary expression" <|
            \() ->
                testRule "a = fn << Debug.log"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in a pipe expression" <|
            \() ->
                testRule "a = fn |> Debug.log z"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in an list expression" <|
            \() ->
                testRule "a = [ Debug.log z y ]"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in an record expression" <|
            \() ->
                testRule "a = { foo = Debug.log z y }"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in an record update expression" <|
            \() ->
                testRule "a = { model | foo = Debug.log z y }"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in an lambda expression" <|
            \() ->
                testRule "a = (\\foo -> Debug.log z foo)"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in an if expression condition" <|
            \() ->
                testRule "a = if Debug.log a b then True else False"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in an if expression then branch" <|
            \() ->
                testRule "a = if True then Debug.log a b else False"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in an if expression else branch" <|
            \() ->
                testRule "a = if True then True else Debug.log a b"
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in a case value" <|
            \() ->
                testRule """
a = case Debug.log a b of
  _ -> []
            """
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in a case body" <|
            \() ->
                testRule """
a = case a of
  _ -> Debug.log a b
            """
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in let declaration section" <|
            \() ->
                testRule """
a = let b = Debug.log a b
    in b
            """
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should report Debug.log in let body" <|
            \() ->
                testRule """
a = let b = c
    in Debug.log a b
            """
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Debug.log"
                            }
                        ]
        , test "should not report calls from a module containing Debug but that is not Debug" <|
            \() ->
                testRule """
a = Foo.Debug.log 1
b = Debug.Foo.log 1
            """
                    |> Review.Test.expectNoErrors
        , test "should not report the import of the Debug module" <|
            \() ->
                testRule "import Debug"
                    |> Review.Test.expectNoErrors
        , test "should report the use of `log` when it has been explicitly imported" <|
            \() ->
                testRule """
import Debug exposing (log)
a = log "" 1
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "log"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } }
                        ]
        , test "should report the use of `log` when it has been implicitly imported" <|
            \() ->
                testRule """
import Debug exposing (..)
a = log "" 1
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "log"
                            }
                        ]
        , test "should not report the use of `log` when it has not been imported" <|
            \() ->
                testRule """
import Debug exposing (todo)
a = log "" 1
"""
                    |> Review.Test.expectNoErrors
        ]
