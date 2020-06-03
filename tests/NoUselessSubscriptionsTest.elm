module NoUselessSubscriptionsTest exposing (all)

import NoUselessSubscriptions exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUselessSubscriptions"
        [ test "should report an error when subscriptions returns Sub.none" <|
            \_ ->
                """
module Main exposing (..)
subscriptions _ = Sub.none
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The `subscription` function never returns any subscriptions"
                            , details = [ "The `subscription` function never returns any subscriptions. You might as well remove it." ]
                            , under = "Sub.none"
                            }
                        ]
        , test "should not report an error if it returns anything else" <|
            \_ ->
                """
module Main exposing (..)
subscriptions _ = Time.every 5000 SomeMsg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when subscriptions returns `Sub.batch []`" <|
            \_ ->
                """
module Main exposing (..)
subscriptions _ = Sub.batch []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The `subscription` function never returns any subscriptions"
                            , details = [ "The `subscription` function never returns any subscriptions. You might as well remove it." ]
                            , under = "Sub.batch []"
                            }
                        ]
        , test "should report an error when subscriptions returns `always Sub.none`" <|
            \_ ->
                """
module Main exposing (..)
subscriptions = always Sub.none
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The `subscription` function never returns any subscriptions"
                            , details = [ "The `subscription` function never returns any subscriptions. You might as well remove it." ]
                            , under = "always Sub.none"
                            }
                        ]
        , test "should report an error when subscriptions returns `Basics.always Sub.none`" <|
            \_ ->
                """
module Main exposing (..)
subscriptions = Basics.always Sub.none
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The `subscription` function never returns any subscriptions"
                            , details = [ "The `subscription` function never returns any subscriptions. You might as well remove it." ]
                            , under = "Basics.always Sub.none"
                            }
                        ]
        ]
