module Simplify.EqualTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


all : Test
all =
    describe "(==)"
        [ test "should not simplify values that can't be determined" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x == True to x" <|
            \() ->
                """module A exposing (..)
a = x == True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "== True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify x == False" <|
            \() ->
                """module A exposing (..)
a = x == False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify True == x to x" <|
            \() ->
                """module A exposing (..)
a = True == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "True =="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify False == x" <|
            \() ->
                """module A exposing (..)
a = False == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify x /= True" <|
            \() ->
                """module A exposing (..)
a = x /= True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x /= False to x" <|
            \() ->
                """module A exposing (..)
a = x /= False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "/= False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify True /= x" <|
            \() ->
                """module A exposing (..)
a = True /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify False /= x to x" <|
            \() ->
                """module A exposing (..)
a = False /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "False /="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify not x == not y to x == y" <|
            \() ->
                """module A exposing (..)
a = not x == not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (==)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "=="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x == y
"""
                        ]
        , test "should simplify (x |> f |> not) == (y |> g |> not) to (x |> f) == (y |> g)" <|
            \() ->
                """module A exposing (..)
a = (x |> f |> not) == (y |> g |> not)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (==)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "=="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> f) == (y |> g)
"""
                        ]
        , test "should simplify not x /= not y to x /= y" <|
            \() ->
                """module A exposing (..)
a = not x /= not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (/=)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "/="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x /= y
"""
                        ]
        , test "should simplify x == x to True" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "x == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify x == x when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should simplify x == (x) to True" <|
            \() ->
                """module A exposing (..)
a = x == (x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "x == (x)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify x /= x to False" <|
            \() ->
                """module A exposing (..)
a = x /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "x /= x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify more complex calls (function call and lambda)" <|
            \() ->
                """module A exposing (..)
a = List.map (\\a -> a.value) things == List.map (\\a -> a.value) things
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "List.map (\\a -> a.value) things == List.map (\\a -> a.value) things"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (f <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(f b) == (f <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (f b <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(f b c) == (f b <| c)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (b |> f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(f b) == (b |> f)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> f b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(f b c) == (c |> f b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> (b |> f))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(f b c) == (c |> (b |> f))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in let expression" <|
            \() ->
                """module A exposing (..)
a = (let x = 1 in f b c) == (c |> (let x = 1 in f b))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(let x = 1 in f b c) == (c |> (let x = 1 in f b))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in if expression" <|
            \() ->
                """module A exposing (..)
a = (if cond then f b c else g d c) == (c |> (if cond then f b else g d))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(if cond then f b c else g d c) == (c |> (if cond then f b else g d))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in case expression" <|
            \() ->
                """module A exposing (..)
a = (case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = """(case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(b.c) == (.c b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison using pipeline" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(b.c) == (.c <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different literals to False" <|
            \() ->
                """module A exposing (..)
a = "a" == "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "\"a\" == \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different char literals to False" <|
            \() ->
                """module A exposing (..)
a = 'a' == 'b'
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "'a' == 'b'"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify inequality of different literal comparisons to True" <|
            \() ->
                """module A exposing (..)
a = "a" /= "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "\"a\" /= \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different number literal comparisons to False" <|
            \() ->
                """module A exposing (..)
a = 1 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "1 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (integer left)" <|
            \() ->
                """module A exposing (..)
a = 1 == 2.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "1 == 2.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (float left)" <|
            \() ->
                """module A exposing (..)
a = 1.0 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "1.0 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (hex left)" <|
            \() ->
                """module A exposing (..)
a = 0x10 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "0x10 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (addition left)" <|
            \() ->
                """module A exposing (..)
a = 1 + 3 == 2 + 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "1 + 3 == 2 + 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (subtraction left)" <|
            \() ->
                """module A exposing (..)
a = 1 - 3 == 2 - 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "1 - 3 == 2 - 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (multiplication left)" <|
            \() ->
                """module A exposing (..)
a = 2 * 3 == 2 * 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "2 * 3 == 2 * 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (division left)" <|
            \() ->
                """module A exposing (..)
a = 1 / 3 == 2 / 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "1 / 3 == 2 / 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of same value (() left)" <|
            \() ->
                """module A exposing (..)
a = () == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "() == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of same value (() right)" <|
            \() ->
                """module A exposing (..)
a = x == ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "x == ()"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of lists (different lengths)" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "[ 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same lengths but different values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "[ 1, 2 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 - 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "[ 1, 2 - 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different integers comparisons to False (wrapped in parens)" <|
            \() ->
                """module A exposing (..)
a = (1) == (2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "(1) == (2)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of tuples" <|
            \() ->
                """module A exposing (..)
a = ( 1, 2 ) == ( 1, 1 )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "( 1, 2 ) == ( 1, 1 )"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of records" <|
            \() ->
                """module A exposing (..)
a = { a = 1, b = 2 } == { b = 1, a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "{ a = 1, b = 2 } == { b = 1, a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and different field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "{ x | a = 1 } == { x | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "{ x | a = 1 } == { x | a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify equality of record updates with same base values and different fields" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify equality of record updates (different base values)" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "{ x | a = 1 } == { y | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should not simplify equality of record updates with same field values but different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify equality of record updates with non-corresponding fields but otherwise similar field values and different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1, b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify comparison of values for which we don't know if they're equal" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should normalize module names" <|
            \() ->
                [ """module A exposing (..)
import B exposing (b)
a = B.b == b
""", """module Other exposing (..)
b = 1
""" ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "(==) comparison will result in True"
                                , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                                , under = "B.b == b"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B exposing (b)
a = True
"""
                            ]
                          )
                        ]
        , test "should simplify function calls with the same function and similar arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == map fn (2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "List.map fn 1 == map fn (2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import List exposing (map)
a = True
"""
                        ]
        , test "should not simplify function calls of the same function but with different arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == List.map fn 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify if expressions that look like each other" <|
            \() ->
                """module A exposing (..)
a = (if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify if expressions with conditions that look different but branches that look like each other when switched around" <|
            \() ->
                """module A exposing (..)
a = (if c then 2 else 3) == (if Basics.not c then 3 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(if c then 2 else 3) == (if Basics.not c then 3 else 2)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify if expressions that have conditions that are like each other but on-True and on-False branches that look different" <|
            \() ->
                """module A exposing (..)
a = (if c then 2 else 3) == (if c then 1 else 4)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "(if c then 2 else 3) == (if c then 1 else 4)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should not simplify if expressions that have conditions that are like each other but on-False branches that look different whereas on-True branches look like each other" <|
            \() ->
                """module A exposing (..)
a = (if c then 1 else 3) == (if c then 1 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify if expressions that don't look like each other in condition" <|
            \() ->
                """module A exposing (..)
a = (if c then 1 else 2) == (if x then 1 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify if expressions that look like each other in condition and on-False branches but on-True branches don't look similar" <|
            \() ->
                """module A exposing (..)
a = (if c then 1 else 2) == (if c then x + 1 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify if expressions that look like each other in condition and on-True branches but on-False branches don't look similar" <|
            \() ->
                """module A exposing (..)
a = (if c then 1 else 2 + x) == (if c then 1 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify if expressions that look different in condition but switched branches don't look similar" <|
            \() ->
                """module A exposing (..)
a = (if c then 1 + x else 2 + x) == (if Basics.not c then 2 else 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify negations that look like each other" <|
            \() ->
                """module A exposing (..)
a = -1 == -(2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "-1 == -(2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record accesses that look like each other" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 2 - 1 }).a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "({ a = 1 }).a == ({ a = 2 - 1 }).a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        , Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 37 }, end = { row = 2, column = 39 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ({ a = 1 }).a == (2 - 1)
"""
                        , Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 16 }, end = { row = 2, column = 18 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 2 - 1 }).a
"""
                        ]
        , test "should simplify operator expressions" <|
            \() ->
                """module A exposing (..)
a = (1 |> fn) == (2 - 1 |> fn)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "(1 |> fn) == (2 - 1 |> fn)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify with different fields" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 1 }).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 1 }).b
"""
                        ]
        , listIsEmptyTests
        , setIsEmptyTests
        , dictIsEmptyTests
        , arrayIsEmptyTests
        ]


listIsEmptyTests : Test
listIsEmptyTests =
    describe "List.length should be `List.isEmpty`"
        [ test "should replace List.length l == 0 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = List.length l == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace 0 == List.length l with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 0 == List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace (l |> List.length) == 0 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = (l |> List.length) == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (l |> List.isEmpty)
"""
                        ]
        , test "should replace 0 == ([] |> List.length) with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 0 == (l |> List.length)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (l |> List.isEmpty)
"""
                        ]
        , test "should replace List.length l /= 0 with not (List.isEmpty l)" <|
            \() ->
                """module A exposing (..)
a = List.length l /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty` and `not`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace 0 /= List.length l with not (List.isEmpty l)" <|
            \() ->
                """module A exposing (..)
a = 0 /= List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty` and `not`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (l |> List.length) /= 0 with (l |> List.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
a = (l |> List.length) /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty` and `not`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (l |> List.isEmpty |> not)
"""
                        ]
        , test "should replace 0 /= (l |> List.length) with (l |> List.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
a = 0 /= (l |> List.length)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty` and `not`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (l |> List.isEmpty |> not)
"""
                        ]
        , test "should replace List.length l /= 0 with Basics.not (List.isEmpty l) when not needs to be qualified" <|
            \() ->
                """module A exposing (..)
a = List.length l /= 0
not = False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `List.isEmpty` and `not`"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the List, List.isEmpty runs in constant time."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Basics.not (List.isEmpty l)
not = False
"""
                        ]
        ]


setIsEmptyTests : Test
setIsEmptyTests =
    describe "Set.size should be `Set.isEmpty`"
        [ test "should replace Set.size s == 0 with Set.isEmpty s" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size s == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.isEmpty s
"""
                        ]
        , test "should replace CoreSet.size s == 0 with isEmpty s" <|
            \() ->
                """module A exposing (..)
import Set as CoreSet
a = CoreSet.size s == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty`"
                            , details =
                                [ "Whereas CoreSet.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "CoreSet.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as CoreSet
a = CoreSet.isEmpty s
"""
                        ]
        , test "should replace size s == 0 with isEmpty s" <|
            \() ->
                """module A exposing (..)
import Set as CoreSet exposing (..)
a = size s == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty`"
                            , details =
                                [ "Whereas size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as CoreSet exposing (..)
a = isEmpty s
"""
                        ]
        , test "should replace 0 == Set.size s with Set.isEmpty s" <|
            \() ->
                """module A exposing (..)
import Set
a = 0 == Set.size s
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.isEmpty s
"""
                        ]
        , test "should replace (s |> Set.size) == 0 with Set.isEmpty s" <|
            \() ->
                """module A exposing (..)
import Set
a = (s |> Set.size) == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (s |> Set.isEmpty)
"""
                        ]
        , test "should replace 0 == ([] |> Set.size) with Set.isEmpty s" <|
            \() ->
                """module A exposing (..)
import Set
a = 0 == (s |> Set.size)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (s |> Set.isEmpty)
"""
                        ]
        , test "should replace Set.size s /= 0 with not (Set.isEmpty s)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size s /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty` and `not`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = not (Set.isEmpty s)
"""
                        ]
        , test "should replace 0 /= Set.size s with not (Set.isEmpty s)" <|
            \() ->
                """module A exposing (..)
import Set
a = 0 /= Set.size s
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty` and `not`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = not (Set.isEmpty s)
"""
                        ]
        , test "should replace (s |> Set.size) /= 0 with (s |> Set.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
import Set
a = (s |> Set.size) /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty` and `not`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (s |> Set.isEmpty |> not)
"""
                        ]
        , test "should replace 0 /= (s |> Set.size) with (s |> Set.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
import Set
a = 0 /= (s |> Set.size)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Set.isEmpty` and `not`"
                            , details =
                                [ "Whereas Set.size takes as long to run as the number of elements in the Set, Set.isEmpty runs in constant time."
                                ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (s |> Set.isEmpty |> not)
"""
                        ]
        ]


dictIsEmptyTests : Test
dictIsEmptyTests =
    describe "Dict.size should be `Dict.isEmpty`"
        [ test "should replace Dict.size d == 0 with Dict.isEmpty d" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size d == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.isEmpty d
"""
                        ]
        , test "should replace size d == 0 with isEmpty d" <|
            \() ->
                """module A exposing (..)
import Dict as CoreDict exposing (..)
a = size d == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty`"
                            , details =
                                [ "Whereas size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict as CoreDict exposing (..)
a = isEmpty d
"""
                        ]
        , test "should replace 0 == Dict.size d with Dict.isEmpty d" <|
            \() ->
                """module A exposing (..)
import Dict
a = 0 == Dict.size d
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.isEmpty d
"""
                        ]
        , test "should replace (d |> Dict.size) == 0 with Dict.isEmpty d" <|
            \() ->
                """module A exposing (..)
import Dict
a = (d |> Dict.size) == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (d |> Dict.isEmpty)
"""
                        ]
        , test "should replace 0 == ([] |> Dict.size) with Dict.isEmpty d" <|
            \() ->
                """module A exposing (..)
import Dict
a = 0 == (d |> Dict.size)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (d |> Dict.isEmpty)
"""
                        ]
        , test "should replace Dict.size d /= 0 with not (Dict.isEmpty d)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size d /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty` and `not`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = not (Dict.isEmpty d)
"""
                        ]
        , test "should replace 0 /= Dict.size d with not (Dict.isEmpty d)" <|
            \() ->
                """module A exposing (..)
import Dict
a = 0 /= Dict.size d
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty` and `not`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = not (Dict.isEmpty d)
"""
                        ]
        , test "should replace (d |> Dict.size) /= 0 with (d |> Dict.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
import Dict
a = (d |> Dict.size) /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty` and `not`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (d |> Dict.isEmpty |> not)
"""
                        ]
        , test "should replace 0 /= (d |> Dict.size) with (d |> Dict.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
import Dict
a = 0 /= (d |> Dict.size)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Dict.isEmpty` and `not`"
                            , details =
                                [ "Whereas Dict.size takes as long to run as the number of elements in the Dict, Dict.isEmpty runs in constant time."
                                ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (d |> Dict.isEmpty |> not)
"""
                        ]
        ]


arrayIsEmptyTests : Test
arrayIsEmptyTests =
    describe "Array.length should be `Array.isEmpty`"
        [ test "should replace Array.length array == 0 with Array.isEmpty array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length array == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.isEmpty array
"""
                        ]
        , test "should replace length array == 0 with isEmpty array" <|
            \() ->
                """module A exposing (..)
import Array as SomethingElse exposing (..)
a = length array == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty`"
                            , details =
                                [ "Whereas length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array as SomethingElse exposing (..)
a = isEmpty array
"""
                        ]
        , test "should replace 0 == Array.length array with Array.isEmpty array" <|
            \() ->
                """module A exposing (..)
import Array
a = 0 == Array.length array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.isEmpty array
"""
                        ]
        , test "should replace (array |> Array.length) == 0 with array |> Array.isEmpty" <|
            \() ->
                """module A exposing (..)
import Array
a = (array |> Array.length) == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (array |> Array.isEmpty)
"""
                        ]
        , test "should replace 0 == ([] |> Array.length) with Array.isEmpty array" <|
            \() ->
                """module A exposing (..)
import Array
a = 0 == (array |> Array.length)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (array |> Array.isEmpty)
"""
                        ]
        , test "should replace Array.length array /= 0 with not (Array.isEmpty array)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length array /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty` and `not`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = not (Array.isEmpty array)
"""
                        ]
        , test "should replace 0 /= Array.length array with not (Array.isEmpty array)" <|
            \() ->
                """module A exposing (..)
import Array
a = 0 /= Array.length array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty` and `not`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = not (Array.isEmpty array)
"""
                        ]
        , test "should replace (array |> Array.length) /= 0 with (array |> Array.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
import Array
a = (array |> Array.length) /= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty` and `not`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (array |> Array.isEmpty |> not)
"""
                        ]
        , test "should replace 0 /= (array |> Array.length) with (array |> Array.isEmpty |> not)" <|
            \() ->
                """module A exposing (..)
import Array
a = 0 /= (array |> Array.length)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "This can be replaced with a call to `Array.isEmpty` and `not`"
                            , details =
                                [ "Whereas Array.length takes as long to run as the number of elements in the Array, Array.isEmpty runs in constant time."
                                ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (array |> Array.isEmpty |> not)
"""
                        ]
        ]
