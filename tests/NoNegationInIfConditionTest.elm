module NoNegationInIfConditionTest exposing (all)

import NoNegationInIfCondition exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoNegationInIfCondition"
        [ test "should not report if condition without a not call" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2 
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report if condition without a not call" <|
            \() ->
                """module A exposing (..)
a = if not condition then 1 else 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Don't use if expressions with negated conditions"
                            , details = [ "We at fruits.com think that if expressions are more readable when the condition is not negated." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if  condition then 2 else 1
"""
                        ]
        , test "should report multi-line if condition without a not call" <|
            \() ->
                """module A exposing (..)
a =
    if not condition then
        1
    else
        2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Don't use if expressions with negated conditions"
                            , details = [ "We at fruits.com think that if expressions are more readable when the condition is not negated." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    if  condition then
        2
    else
        1
"""
                        ]
        ]
