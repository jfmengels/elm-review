module Simplify.IfTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "if expressions"
        [ test "should remove the else branch when a condition is True" <|
            \() ->
                """module A exposing (..)
a = if True then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should remove the if branch when a condition is False" <|
            \() ->
                """module A exposing (..)
a = if False then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should not remove anything if the condition is not statically knowable" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should only keep the condition if then is True and else is False" <|
            \() ->
                """module A exposing (..)
a = if condition then True else False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the same as the condition"
                            , details = [ "The expression can be replaced by the condition." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = condition
"""
                        ]
        , test "should only keep the negated condition if then is False and else is True" <|
            \() ->
                """module A exposing (..)
a = if condition then False else True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the inverse of the condition"
                            , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not condition
"""
                        ]
        , test "should replace the expression by the branch if both branches have the same value" <|
            \() ->
                """module A exposing (..)
a = if condition then x else x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The values in both branches is the same."
                            , details = [ "The expression can be replaced by the contents of either branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace the expression by the branch if both branches are known to be equal" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2 - 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The values in both branches is the same."
                            , details = [ "The expression can be replaced by the contents of either branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        ]
