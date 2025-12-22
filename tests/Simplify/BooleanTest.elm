module Simplify.BooleanTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Booleans"
        [ orTests
        , andTests
        , notTests
        ]


orTests : Test
orTests =
    describe "||"
        [ test "should not report unsimplifiable condition" <|
            \() ->
                """module A exposing (..)
a = x || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'True || x' to True" <|
            \() ->
                """module A exposing (..)
a = True || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(||) with any side being True will result in True"
                            , details =
                                [ "You can replace this operation by True."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "True || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'x || True' to True" <|
            \() ->
                """module A exposing (..)
a = x || True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(||) with any side being True will result in True"
                            , details =
                                [ "You can replace this operation by True."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "x || True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'False || x' to x" <|
            \() ->
                """module A exposing (..)
a = False || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary || False"
                            , details = [ "You can replace this operation by the right bool." ]
                            , under = "False ||"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x || False' to x" <|
            \() ->
                """module A exposing (..)
a = x || False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary || False"
                            , details = [ "You can replace this operation by the left bool." ]
                            , under = "|| False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around False" <|
            \() ->
                """module A exposing (..)
a = x || (False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary || False"
                            , details = [ "You can replace this operation by the left bool." ]
                            , under = "|| (False)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around True" <|
            \() ->
                """module A exposing (..)
a = (True) || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(||) with any side being True will result in True"
                            , details =
                                [ "You can replace this operation by True."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "(True) || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (True)
"""
                        ]
        , test "should simply x || x to x" <|
            \() ->
                """module A exposing (..)
a = x || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simply x || y || x to x || y" <|
            \() ->
                """module A exposing (..)
a = x || y || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || y
"""
                        ]
        , test "should simply x || x || y to x || y" <|
            \() ->
                """module A exposing (..)
a = x || x || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || y
"""
                        ]
        , test "should simply x || (x) || y to x || False || y" <|
            \() ->
                """module A exposing (..)
a = x || (x) || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || (False) || y
"""
                        ]
        , test "should simply x || (x || y) to x || (False || y)" <|
            \() ->
                """module A exposing (..)
a = x || (x || y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || (False || y)
"""
                        ]
        , test "should simply (x || y) || (z || x) to (x || y) || (z)" <|
            \() ->
                """module A exposing (..)
a = (x || y) || (z || x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x || y) || (z)
"""
                        ]
        , test "should simply x && x to x" <|
            \() ->
                """module A exposing (..)
a = x && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (&&) operator, therefore one of them can be removed."
                                ]
                            , under = " && x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simply x && (x && y) to x && (True && y)" <|
            \() ->
                """module A exposing (..)
a = x && (x && y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (&&) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x && (True && y)
"""
                        ]
        ]


andTests : Test
andTests =
    describe "&&"
        [ test "should not report unsimplifiable condition" <|
            \() ->
                """module A exposing (..)
a = x && y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'True && x' to x" <|
            \() ->
                """module A exposing (..)
a = True && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary && True"
                            , details = [ "You can replace this operation by the right bool." ]
                            , under = "True &&"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x && True' to x" <|
            \() ->
                """module A exposing (..)
a = x && True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary && True"
                            , details = [ "You can replace this operation by the left bool." ]
                            , under = "&& True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'False && x' to False" <|
            \() ->
                """module A exposing (..)
a = False && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(&&) with any side being False will result in False"
                            , details =
                                [ "You can replace this operation by False."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "False && x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'x && False' to False" <|
            \() ->
                """module A exposing (..)
a = x && False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(&&) with any side being False will result in False"
                            , details =
                                [ "You can replace this operation by False."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "x && False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


notTests : Test
notTests =
    describe "not calls"
        [ test "should simplify 'not True' to False" <|
            \() ->
                """module A exposing (..)
a = not True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not False' to True" <|
            \() ->
                """module A exposing (..)
a = not False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be False can be replaced by True"
                            , details = [ "You can replace this call by True." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'not (True)' to False" <|
            \() ->
                """module A exposing (..)
a = not (True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not <| True' to False" <|
            \() ->
                """module A exposing (..)
a = not <| True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'True |> not' to False" <|
            \() ->
                """module A exposing (..)
a = True |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify not >> not to identity" <|
            \() ->
                """module A exposing (..)
a = not >> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> not >> not to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> not >> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 17 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify not >> not >> a to a" <|
            \() ->
                """module A exposing (..)
a = not >> not >> a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify not << not to identity" <|
            \() ->
                """module A exposing (..)
a = not << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify not << not << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = not << not << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << not << not to a" <|
            \() ->
                """module A exposing (..)
a = a << not << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 10 }, end = { row = 2, column = 13 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify (not >> a) << not to a" <|
            \() ->
                """module A exposing (..)
a = (not >> a) << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should not simplify (not << a) << not" <|
            \() ->
                """module A exposing (..)
a = (not << a) << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'not (not x)' to x" <|
            \() ->
                """module A exposing (..)
a = not (not x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x |> not |> not' to x" <|
            \() ->
                """module A exposing (..)
a = x |> not |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 17 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify '(x |> not) |> not' to x" <|
            \() ->
                """module A exposing (..)
a = (x |> not) |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 19 }, end = { row = 2, column = 22 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify '(not <| x) |> not' to x" <|
            \() ->
                """module A exposing (..)
a = (not <| x) |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 19 }, end = { row = 2, column = 22 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'not x |> not' to x" <|
            \() ->
                """module A exposing (..)
a = not x |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 14 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'not <| not x' to x" <|
            \() ->
                """module A exposing (..)
a = not <| not x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'not (b < c) to (b >= c)" <|
            \() ->
                """module A exposing (..)
a = not (b < c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `>=` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b >= c)
"""
                        ]
        , test "should simplify 'not (b <= c) to (b > c)" <|
            \() ->
                """module A exposing (..)
a = not (b <= c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `>` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b > c)
"""
                        ]
        , test "should simplify 'not (b > c) to (b <= c)" <|
            \() ->
                """module A exposing (..)
a = not (b > c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `<=` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b <= c)
"""
                        ]
        , test "should simplify 'not (b >= c) to (b < c)" <|
            \() ->
                """module A exposing (..)
a = not (b >= c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `<` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b < c)
"""
                        ]
        , test "should simplify 'not (b == c) to (b /= c)" <|
            \() ->
                """module A exposing (..)
a = not (b == c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `/=` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b /= c)
"""
                        ]
        , test "should simplify 'not (b /= c) to (b == c)" <|
            \() ->
                """module A exposing (..)
a = not (b /= c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `==` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b == c)
"""
                        ]
        ]
