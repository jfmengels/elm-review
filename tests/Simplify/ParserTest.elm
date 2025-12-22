module Simplify.ParserTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Parser.oneOf"
        [ test "should not report Parser.oneOf used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Parser
import Parser.Advanced
a = Parser.oneOf x
b = Parser.oneOf [ y, z ]
c = Parser.Advanced.oneOf x
d = Parser.Advanced.oneOf [ y, z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Parser.oneOf [ x ] by x" <|
            \() ->
                """module A exposing (..)
import Parser
a = Parser.oneOf [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Parser.oneOf on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Parser.oneOf"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Parser
a = x
"""
                        ]
        , test "should replace Parser.Advanced.oneOf [ x ] by x" <|
            \() ->
                """module A exposing (..)
import Parser.Advanced
a = Parser.Advanced.oneOf [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Parser.Advanced.oneOf on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Parser.Advanced.oneOf"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Parser.Advanced
a = x
"""
                        ]
        ]
