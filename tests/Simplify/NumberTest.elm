module Simplify.NumberTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


all : Test
all =
    describe "Number tests"
        [ plusTests
        , minusTests
        , multiplyTests
        , divisionTests
        , intDivideTests
        , negationTest
        , basicsNegateTests
        ]


plusTests : Test
plusTests =
    describe "(+)"
        [ test "should not simplify (+) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b + 1
b = 2 + 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n + 0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary adding 0"
                            , details = [ "You can replace this operation by the left number you added 0 to." ]
                            , under = "+ 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n + 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary adding 0"
                            , details = [ "You can replace this operation by the left number you added 0 to." ]
                            , under = "+ 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 + n to n" <|
            \() ->
                """module A exposing (..)
a = 0 + n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary adding 0"
                            , details = [ "You can replace this operation by the right number you added 0 to." ]
                            , under = "0 +"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n + (-n) to 0" <|
            \() ->
                """module A exposing (..)
a = n + (-n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Adding opposite numbers will result in 0"
                            , details = [ "Adding two numbers with an equal absolute value and an opposite sign will cancel each other out. You can replace this operation by 0." ]
                            , under = "n + (-n)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify -n + n to 0" <|
            \() ->
                """module A exposing (..)
a = -n + n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Adding opposite numbers will result in 0"
                            , details = [ "Adding two numbers with an equal absolute value and an opposite sign will cancel each other out. You can replace this operation by 0." ]
                            , under = "-n + n"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should not simplify n + (-n) to 0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n + (-n) to 0
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        ]


minusTests : Test
minusTests =
    describe "(-)"
        [ test "should not simplify (-) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b - 1
b = 2 - 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n - 0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtracting 0"
                            , details = [ "You can replace this operation by the left number you subtracted 0 from." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n - 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtracting 0"
                            , details = [ "You can replace this operation by the left number you subtracted 0 from." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 - n to -n" <|
            \() ->
                """module A exposing (..)
a = 0 - n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Subtracting from 0 is the same as negating"
                            , details = [ "You can replace this operation by the negated right number you subtracted from 0, like `-n`." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -n
"""
                        ]
        , test "should simplify 0 - List.length list to -(List.length list)" <|
            \() ->
                """module A exposing (..)
a = 0 - List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Subtracting from 0 is the same as negating"
                            , details = [ "You can replace this operation by the negated right number you subtracted from 0, like `-n`." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -(List.length list)
"""
                        ]
        , test "should simplify n - n to 0" <|
            \() ->
                """module A exposing (..)
a = n - n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Subtracting equal numbers will result in 0"
                            , details = [ "You can replace this operation by 0." ]
                            , under = "n - n"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should not simplify n - n to 0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n - n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        ]


multiplyTests : Test
multiplyTests =
    describe "(*)"
        [ test "should not simplify (*) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b * 2
b = 2 * 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n * 1 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary * 1"
                            , details = [ "You can replace this operation by the left number." ]
                            , under = "* 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n * 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary * 1"
                            , details = [ "You can replace this operation by the left number." ]
                            , under = "* 1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 1 * n to n" <|
            \() ->
                """module A exposing (..)
a = 1 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary * 1"
                            , details = [ "You can replace this operation by the right number." ]
                            , under = "1 *"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n * 0 by 0" <|
            \() ->
                """module A exposing (..)
a = n * 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should report but not fix n * 0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n * 0
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0"
                            }
                        ]
        , test "should simplify n * 0.0 to 0" <|
            \() ->
                """module A exposing (..)
a = n * 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should report but not fix n * 0.0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n * 0.0
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0.0"
                            }
                        ]
        , test "should simplify 0 * n to 0" <|
            \() ->
                """module A exposing (..)
a = 0 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "0 *"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify 0.0 * n to 0" <|
            \() ->
                """module A exposing (..)
a = 0.0 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "0.0 *"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]


divisionTests : Test
divisionTests =
    describe "(/)"
        [ test "should not simplify (/) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = 1 / 2
b = 2 / 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n / 1 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left number you divided by 1." ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n / 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left number you divided by 1." ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 / n to 0" <|
            \() ->
                """module A exposing (..)
a = 0 / n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing 0 will result in 0"
                            , details =
                                [ "Dividing 0 by anything, even infinite numbers, gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify 0.0 / n to 0.0" <|
            \() ->
                """module A exposing (..)
a = 0.0 / n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing 0 will result in 0"
                            , details =
                                [ "Dividing 0 by anything, even infinite numbers, gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0.0
"""
                        ]
        , test "should report but not fix 0 / 0" <|
            \() ->
                """module A exposing (..)
a = 0 / 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        , test "should report but not fix 0 / 0.0" <|
            \() ->
                """module A exposing (..)
a = 0 / 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        , test "should report but not fix 0.0 / 0" <|
            \() ->
                """module A exposing (..)
a = 0.0 / 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        , test "should report but not fix 0.0 / 0.0" <|
            \() ->
                """module A exposing (..)
a = 0.0 / 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        ]


intDivideTests : Test
intDivideTests =
    describe "(//)"
        [ test "should not simplify (//) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = 1 // 2
b = 2 // 3
c = n // n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n // 1 to n" <|
            \() ->
                """module A exposing (..)
a = n // 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left integer you divided by 1." ]
                            , under = "//"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n // m // 1 to n // m" <|
            \() ->
                """module A exposing (..)
a = n // m // 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left integer you divided by 1." ]
                            , under = "//"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n // m
"""
                        ]
        , test "should simplify 0 // n to 0" <|
            \() ->
                """module A exposing (..)
a = 0 // n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing 0 will result in 0"
                            , details =
                                [ "Dividing 0 by anything using (//), even 0, gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "//"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify n // 0 to 0" <|
            \() ->
                """module A exposing (..)
a = n // 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing by 0 will result in 0"
                            , details =
                                [ "Dividing anything by 0 using (//) gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing by 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "//"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]


negationTest : Test
negationTest =
    describe "Unary negation"
        [ test "should not report negation used in okay situations" <|
            \() ->
                """module A exposing (..)
a = -1
a = -(-1 + 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify -(-n) to n" <|
            \() ->
                """module A exposing (..)
a = -(-n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            , under = "-(-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify -(-(f n)) to (f n)" <|
            \() ->
                """module A exposing (..)
a = -(-(f n))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            , under = "-(-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        ]


basicsNegateTests : Test
basicsNegateTests =
    describe "Basics.negate"
        [ test "should simplify negate >> negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate >> negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 15 }, end = { row = 2, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> negate >> negate to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> negate >> negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 26 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify negate >> negate >> a to a" <|
            \() ->
                """module A exposing (..)
a = negate >> negate >> a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 15 }, end = { row = 2, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify negate << negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify negate << negate << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = negate << negate << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << negate << negate to a" <|
            \() ->
                """module A exposing (..)
a = a << negate << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 10 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify (negate >> a) << negate to a" <|
            \() ->
                """module A exposing (..)
a = (negate >> a) << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should negate simplify (negate << a) << negate" <|
            \() ->
                """module A exposing (..)
a = (negate << a) << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'negate <| negate x' to x" <|
            \() ->
                """module A exposing (..)
a = negate <| negate x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.negate." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]
