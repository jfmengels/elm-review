module NoLeftPizzaTest exposing (..)

import NoLeftPizza
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "NoLeftPizza"
        [ test "Simple pizza" <|
            \_ ->
                """module A exposing (..)

a = foo <| bar"""
                    |> Review.Test.run NoLeftPizza.rule
                    |> Review.Test.expectErrors
                        [ makeError "foo <| bar"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo bar"""
                        ]
        , test "Nested pizza" <|
            \_ ->
                """module A exposing (..)

a = foo <| bar <| baz"""
                    |> Review.Test.run NoLeftPizza.rule
                    |> Review.Test.expectErrors
                        [ makeError
                            "foo <| bar <| baz"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo (bar <| baz)"""
                        , makeError "bar <| baz"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo <| bar baz"""
                        ]
        , test "Fixes operator precedence" <|
            \_ ->
                """module A exposing (..)

a = foo <| 1 + 1"""
                    |> Review.Test.run NoLeftPizza.rule
                    |> Review.Test.expectErrors
                        [ makeError
                            "foo <| 1 + 1"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo (1 + 1)"""
                        ]
        , test "Fixes more operator precedence" <|
            \_ ->
                """module A exposing (..)

a = foo <| 1 + 1 / 2"""
                    |> Review.Test.run NoLeftPizza.rule
                    |> Review.Test.expectErrors
                        [ makeError
                            "foo <| 1 + 1 / 2"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo (1 + 1 / 2)"""
                        ]
        , test "Why isn't this fixed?" <|
            \_ ->
                """module A exposing (..)

f =
    List.map .x <| y
"""
                    |> Review.Test.run NoLeftPizza.rule
                    |> Review.Test.expectErrors
                        [ makeError
                            "List.map .x <| y"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

f =
    List.map .x y
"""
                        ]
        , test "Why isn't _this_ fixed, pt2?" <|
            \_ ->
                """module A exposing (..)
f =
    String.join " " <|
        List.map x y
"""
                    |> Review.Test.run NoLeftPizza.rule
                    |> Review.Test.expectErrors
                        [ makeError
                            """String.join " " <|
        List.map x y"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)
f =
    String.join " " (List.map x y)
"""
                        ]
        ]


makeError : String -> Review.Test.ExpectedError
makeError under =
    Review.Test.error
        { message = "That's a left pizza (<|) operator application there!"
        , details =
            [ "We prefer using either parenthesized function application like `Html.text (context.translate Foo.Bar)` or right pizza's like `foo |> bar`."
            , "The proposed fix rewrites the expression to a simple parenthesized expression, however, this may not always be what you want. Use your best judgement!"
            ]
        , under = under
        }
