module MiscRules.NoBooleanCaseTest exposing (all)

import MiscRules.NoBooleanCase as NoBooleanCase
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "noBooleanCase"
        [ test "Simple one" <|
            \_ ->
                """module A exposing (..)
a =
    case expr of
        True -> True
        False -> False
"""
                    |> Review.Test.run NoBooleanCase.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Matching boolean values in a case .. of expression"
                            , details =
                                [ "It's quite silly"
                                ]
                            , under = """case expr of
        True -> True
        False -> False"""
                            }
                        ]
        ]
