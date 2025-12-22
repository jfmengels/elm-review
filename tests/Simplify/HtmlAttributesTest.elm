module Simplify.HtmlAttributesTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Html.Attributes"
        [ htmlAttributesClassListTests
        ]


htmlAttributesClassListTests : Test
htmlAttributesClassListTests =
    describe "Html.Attributes.classList"
        [ test "should not report Html.Attributes.classList used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList
b = Html.Attributes.classList x
c = Html.Attributes.classList [ y, z ]
c = Html.Attributes.classList [ y, ( z, True ) ]
d = Html.Attributes.classList (x :: y :: z)
d = Html.Attributes.classList (( y, True ) :: z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Html.Attributes.classList [ ( x, True ) ] by Html.Attributes.class x" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ ( x, True ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class x
"""
                        ]
        , test "should replace Html.Attributes.classList [ ( f x, True ) ] by Html.Attributes.class (f x)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ ( f x, True ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class (f x)
"""
                        ]
        , test "should replace Html.Attributes.classList (List.singleton ( x, True )) by Html.Attributes.class x" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (List.singleton ( x, True ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class x
"""
                        ]
        , test "should replace Html.Attributes.classList (List.singleton ( f x, True )) by Html.Attributes.class (f x)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (List.singleton ( f x, True ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class (f x)
"""
                        ]
        , test "should replace Html.Attributes.classList (List.singleton ( x, False )) by Html.Attributes.classList []" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (List.singleton ( x, False ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList []
"""
                        ]
        , test "should replace Html.Attributes.classList [ x, ( y, False ), z ] by Html.Attributes.classList [ x, y ]" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x, ( y, False ), z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x, z ]
"""
                        ]
        , test "should replace Html.Attributes.classList [ x, ( y, False ) ] by Html.Attributes.classList [ x ]" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x, ( y, False ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x ]
"""
                        ]
        , test "should replace Html.Attributes.classList [ ( x, False ), y ] by Html.Attributes.classList [ y ]" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ ( x, False ), y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ y ]
"""
                        ]
        , test "should replace Html.Attributes.classList [( x, False )] by Html.Attributes.classList []" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [( x, False )]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList []
"""
                        ]
        , test "should replace Html.Attributes.classList (( x, False ) :: tail) by Html.Attributes.classList (tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (( x, False ) :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (( x, False ) :: f tail) by Html.Attributes.classList (f tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (( x, False ) :: f tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (f tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (x :: ( y, False ) :: tail) by Html.Attributes.classList (x :: tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: ( y, False ) :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (x :: ( y, False ) :: z :: tail) by Html.Attributes.classList (x :: tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: ( y, False ) :: z :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: z :: tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (( x, False ) :: y :: tail) by Html.Attributes.classList (x :: tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (( x, False ) :: y :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (y :: tail)
"""
                        ]
        ]
