module Simplify.LetTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Let declarations"
        [ test "should merge two adjacent let declarations" <|
            \() ->
                """module A exposing (..)
a =
    let
        b =
            1
    in
    let
        c =
            1
    in
    b + c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Let blocks can be joined together"
                            , details = [ "Let blocks can contain multiple declarations, and there is no advantage to having multiple chained let expressions rather than one longer let expression." ]
                            , under = "let"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let
        b =
            1

        c =
            1
    in
    b + c
"""
                        ]
        , test "should merge two adjacent let declarations with multiple declarations" <|
            \() ->
                """module A exposing (..)
a =
    let
        b =
            1

        c =
            1
    in
    let
        d =
            1

        e =
            1
    in
    b + c + d + e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Let blocks can be joined together"
                            , details = [ "Let blocks can contain multiple declarations, and there is no advantage to having multiple chained let expressions rather than one longer let expression." ]
                            , under = "let"
                            }
                            |> Review.Test.atExactly { start = { row = 10, column = 5 }, end = { row = 10, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let
        b =
            1

        c =
            1

        d =
            1

        e =
            1
    in
    b + c + d + e
"""
                        ]
        ]
