module NoDebug.LogTest exposing (all)

import NoDebug.Log exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


errorDetails : { message : String, details : List String, under : String }
errorDetails =
    { message = message
    , details = details
    , under = "Debug.log"
    }


whenFixed : String -> Review.Test.ExpectedError -> Review.Test.ExpectedError
whenFixed string =
    Review.Test.whenFixed ("module A exposing (..)\n\n" ++ string)


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
                        [ Review.Test.error errorDetails
                        ]
        , test "should report Debug.log calls" <|
            \() ->
                testRule "a = Debug.log z"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                        ]
        , test "should report multiple Debug.log calls" <|
            \() ->
                testRule """
a = Debug.log z
b = Debug.log z
            """
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
                        , Review.Test.error errorDetails
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                        ]
        , test "should report Debug.log in a binary expression" <|
            \() ->
                testRule "a = ( Debug.log z ) + 2"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                        ]
        , test "should report Debug.log in a << binary expression (on the right)" <|
            \() ->
                testRule "a = fn << Debug.log b"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = fn"
                        ]
        , test "should report Debug.log in a << binary expression (on the left)" <|
            \() ->
                testRule "a = Debug.log b << fn"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = fn"
                        ]
        , test "should report Debug.log in a >> binary expression (on the right)" <|
            \() ->
                testRule "a = fn >> Debug.log b"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = fn"
                        ]
        , test "should report Debug.log in a >> binary expression (on the left)" <|
            \() ->
                testRule "a = Debug.log b >> fn"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = fn"
                        ]
        , test "should report Debug.log in a |> pipe expression" <|
            \() ->
                testRule "a = fn |> Debug.log z"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = fn"
                        ]
        , test "should report Debug.log in a large |> pipe expression (pipes on each side)" <|
            \() ->
                testRule "a = fn |> Debug.log z |> fn2"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = fn |> fn2"
                        ]
        , test "should report Debug.log multiple times in a pipeline expression with multiple Debug.log calls" <|
            \() ->
                testRule "a = fn |> Debug.log z |> fn2 |> Debug.log y"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> Review.Test.atExactly { start = { row = 3, column = 11 }, end = { row = 3, column = 20 } }
                            |> whenFixed "a = fn |> fn2 |> Debug.log y"
                        , Review.Test.error errorDetails
                            |> Review.Test.atExactly { start = { row = 3, column = 33 }, end = { row = 3, column = 42 } }
                            |> whenFixed "a = fn |> Debug.log z |> fn2"
                        ]
        , test "should report Debug.log in a <| pipe expression" <|
            \() ->
                testRule "a = Debug.log z <| fn"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = fn"
                        ]
        , test "should report Debug.log in a large <| pipe expression" <|
            \() ->
                testRule "a = foo <| Debug.log z <| fn"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = foo <| fn"
                        ]
        , test "should report Debug.log in an list expression" <|
            \() ->
                testRule "a = [ Debug.log z y ]"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = [ y ]"
                        ]
        , test "should report Debug.log in a record expression" <|
            \() ->
                testRule "a = { foo = Debug.log z y }"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = { foo = y }"
                        ]
        , test "should report Debug.log in a record update expression" <|
            \() ->
                testRule "a = { model | foo = Debug.log z y }"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = { model | foo = y }"
                        ]
        , test "should report Debug.log in an lambda expression" <|
            \() ->
                testRule "a = (\\foo -> Debug.log z foo)"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = (\\foo -> foo)"
                        ]
        , test "should report Debug.log in an if expression condition" <|
            \() ->
                testRule "a = if Debug.log a b then True else False"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = if b then True else False"
                        ]
        , test "should report Debug.log in an if expression then branch" <|
            \() ->
                testRule "a = if True then Debug.log a b else False"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = if True then b else False"
                        ]
        , test "should report Debug.log in an if expression else branch" <|
            \() ->
                testRule "a = if True then True else Debug.log a b"
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed "a = if True then True else b"
                        ]
        , test "should report Debug.log in a case value" <|
            \() ->
                testRule """
a = case Debug.log a b of
  _ -> []"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed """
a = case b of
  _ -> []"""
                        ]
        , test "should report Debug.log in a case body" <|
            \() ->
                testRule """
a = case a of
  _ -> Debug.log a b"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed """
a = case a of
  _ -> b"""
                        ]
        , test "should report Debug.log in let declaration section" <|
            \() ->
                testRule """
a = let b = Debug.log a c
    in b"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed """
a = let b = c
    in b"""
                        ]
        , test "should report Debug.log in let body" <|
            \() ->
                testRule """
a = let b = c
    in Debug.log a b"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error errorDetails
                            |> whenFixed """
a = let b = c
    in b"""
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
                            |> whenFixed """
import Debug exposing (log)
a = 1
"""
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
                            |> whenFixed """
import Debug exposing (..)
a = 1
"""
                        ]
        , test "should not report the use of `log` when it has not been imported" <|
            \() ->
                testRule """
import Debug exposing (todo)
a = log "" 1
"""
                    |> Review.Test.expectNoErrors
        ]
