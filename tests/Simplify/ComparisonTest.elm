module Simplify.ComparisonTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults, whenNotExpectingNaN)


all : Test
all =
    describe "comparison tests"
        [ lessThanTests
        , lessThanOrEqualToTests
        , greaterThanOrEqualToTests
        , greaterThanTests
        , interactionWithComplexCollectionSizesTests
        ]


lessThanTests : Test
lessThanTests =
    describe "<"
        [ test "should simplify n < n to False, expect NaN enabled" <|
            \() ->
                """module A exposing (..)
a = n < n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with two equal operands results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify n > n to False" <|
            \() ->
                """module A exposing (..)
a = n > n
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "(>) with two equal operands results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify n >= n to True" <|
            \() ->
                """module A exposing (..)
a = n >= n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) with two equal operands results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify n <= n to True" <|
            \() ->
                """module A exposing (..)
a = n <= n
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "(<=) with two equal operands results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report > with okay operands" <|
            \() ->
                """module A exposing (..)
a = x > y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report >= with okay operands" <|
            \() ->
                """module A exposing (..)
a = x >= y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report < with okay operands" <|
            \() ->
                """module A exposing (..)
a = x < y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report <= with okay operands" <|
            \() ->
                """module A exposing (..)
a = x <= y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 1 < 2 to True" <|
            \() ->
                """module A exposing (..)
a = 1 < 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 1.1 < 2.2 to True" <|
            \() ->
                """module A exposing (..)
a = 1.1 < 2.2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'a' < 'b' to True" <|
            \() ->
                """module A exposing (..)
a = 'a' < 'b'
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify \"a\" < \"b\" to True" <|
            \() ->
                """module A exposing (..)
a = "a" < "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify (1.1,2) < (2.2,1) to True" <|
            \() ->
                """module A exposing (..)
a = (1.1,2) < (2.2,1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify (0,1.1) < (0,2.2) to True" <|
            \() ->
                """module A exposing (..)
a = (0,1.1) < (0,2.2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [1.1,2] < [2.2,1] to True" <|
            \() ->
                """module A exposing (..)
a = [1.1,2] < [2.2,1]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [1.1,2] < [2.2] to True" <|
            \() ->
                """module A exposing (..)
a = [1.1,2] < [2.2]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [0,1.1] < ([0,(2.2)]) to True" <|
            \() ->
                """module A exposing (..)
a = [0,1.1] < ([0,(2.2)])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [0] < [0,2.2] to True" <|
            \() ->
                """module A exposing (..)
a = [0] < [0,2.2]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 1 < 2 + 3 to False" <|
            \() ->
                """module A exposing (..)
a = 1 < 2 + 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 2 < 1 to False" <|
            \() ->
                """module A exposing (..)
a = 2 < 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value greater than the right results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 > 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 > 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) with a left value less than the right results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 >= 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) with a left value less than the right results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 <= 2 to True" <|
            \() ->
                """module A exposing (..)
a = 1 <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length l < 1 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = List.length l < 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length < 1 can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace 0 < List.length l with not (List.isEmpty l)" <|
            \() ->
                """module A exposing (..)
a = 0 < List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 < List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (<) 0 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (<) 0 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 < List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by not on List.isEmpty."
                                ]
                            , under = "(<)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not << List.isEmpty
"""
                        ]
        , test "should not report List.length l < min 0 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l < min 0 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should not report x < List.length list" <|
            \() ->
                """module A exposing (..)
a = x < List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length < x" <|
            \() ->
                """module A exposing (..)
a = List.length length < x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length < String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length < String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.length l < 0 by False" <|
            \() ->
                """module A exposing (..)
a = List.length l < 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly 0."
                                ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace min -1 n < 0 by True" <|
            \() ->
                """module A exposing (..)
a = min -1 n < 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at most -1 and the right number was determined to be exactly 0."
                                ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report min 0 n < 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = min 0 n < 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


greaterThanOrEqualToTests : Test
greaterThanOrEqualToTests =
    describe ">="
        [ test "should replace 0 >= List.length l with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 0 >= List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 >= List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace List.length l >= 1 with not (List.isEmpty l)" <|
            \() ->
                """module A exposing (..)
a = List.length l >= 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length >= 1 can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (>=) 0 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (>=) 0 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 >= List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by List.isEmpty."
                                ]
                            , under = "(>=)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty
"""
                        ]
        , test "should not replace List.length l >= min -1 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l >= min -1 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should not replace min 0 n >= 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = min 0 n >= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace min -1 n >= 0 by False" <|
            \() ->
                """module A exposing (..)
a = min -1 n >= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at most -1 and the right number was determined to be exactly 0."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.length l >= 0 by True" <|
            \() ->
                """module A exposing (..)
a = List.length l >= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly 0."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length l >= -1 by True" <|
            \() ->
                """module A exposing (..)
a = List.length l >= -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly -1."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report x >= List.length list" <|
            \() ->
                """module A exposing (..)
a = x >= List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length >= x" <|
            \() ->
                """module A exposing (..)
a = List.length length >= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length >= String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length >= String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x >= y" <|
            \() ->
                """module A exposing (..)
a = x >= y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


lessThanOrEqualToTests : Test
lessThanOrEqualToTests =
    describe "<="
        [ test "should replace List.length l <= 0 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = List.length l <= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length <= 0 can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace 1 <= List.length l with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 1 <= List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 <= List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (<=) 1 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (<=) 1 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 <= List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by not on List.isEmpty."
                                ]
                            , under = "(<=)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not << List.isEmpty
"""
                        ]
        , test "should not replace List.length l <= min -1 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l <= min -1 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace List.length l <= -1 by False" <|
            \() ->
                """module A exposing (..)
a = List.length l <= -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly -1."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace min 0 n <= 0 by True" <|
            \() ->
                """module A exposing (..)
a = min 0 n <= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at most 0 and the right number was determined to be exactly 0."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report max 0 n <= 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = max 0 n <= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x <= List.length list" <|
            \() ->
                """module A exposing (..)
a = x <= List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length <= x" <|
            \() ->
                """module A exposing (..)
a = List.length length <= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length <= String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length <= String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace (let n = m in min 0 n) <= 0 by True" <|
            \() ->
                """module A exposing (..)
a = (let n = m in min 0 n) <= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at most 0 and the right number was determined to be exactly 0."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace (if c then 0 else 1) <= 1 by True" <|
            \() ->
                """module A exposing (..)
a = (if c then 0 else 1) <= 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 1 inclusive and the right number was determined to be exactly 1."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace (case n of 0 -> 0 ; _ -> 1) <= 1 by True" <|
            \() ->
                """module A exposing (..)
a = (case n of 0 -> 0
               _ -> 1) <= 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 1 inclusive and the right number was determined to be exactly 1."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        ]


greaterThanTests : Test
greaterThanTests =
    describe ">"
        [ test "should replace 1 > List.length l with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 1 > List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 > List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace List.length l > 0 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = List.length l > 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length > 0 can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (>) 1 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (>) 1 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 > List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by List.isEmpty."
                                ]
                            , under = "(>)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty
"""
                        ]
        , test "should not replace List.length l > max -1 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l > max -1 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace List.length l > -1 by True" <|
            \() ->
                """module A exposing (..)
a = List.length l > -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly -1."
                                ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace min 0 n > 0 by False" <|
            \() ->
                """module A exposing (..)
a = min 0 n > 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at most 0 and the right number was determined to be exactly 0."
                                ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should not report max 0 n > 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = max 0 n > 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x > List.length list" <|
            \() ->
                """module A exposing (..)
a = x > List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length > x" <|
            \() ->
                """module A exposing (..)
a = List.length length > x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length > String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length > String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x > y" <|
            \() ->
                """module A exposing (..)
a = x > y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


interactionWithComplexCollectionSizesTests : Test
interactionWithComplexCollectionSizesTests =
    describe "interaction with complex collection sizes"
        [ test "should replace String.length (String.fromChar c) >= 3 by False" <|
            \() ->
                """module A exposing (..)
a = String.length (String.fromChar c) >= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace Set.size (Set.fromList [ first, second ]) >= 3 by False" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [ first, second ]) >= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        ]
        , test "should replace Dict.size (Dict.fromList ((0,()) :: (1,()) :: tail)) < 2 by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList ((0,()) :: (1,()) :: tail)) < 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace List.length (List.drop 2 (e0 :: e1 :: e2 :: e3 :: e4Up)) >= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (List.drop 2 (e0 :: e1 :: e2 :: e3 :: e4Up)) >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (List.take 3 (e0 :: e1 :: el2Up)) >= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (List.take 3 (e0 :: e1 :: el2Up)) >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 3 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.length (String.left (clamp 1 5 n) str) <= 5 by True" <|
            \() ->
                """module A exposing (..)
a = String.length (String.left (clamp 1 5 n) str) <= 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 5 inclusive and the right number was determined to be exactly 5."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.length (String.dropRight (clamp 1 5 n) \"123456789\") <= 8 by True" <|
            \() ->
                """module A exposing (..)
a = String.length (String.dropRight (clamp 1 5 n) "123456789") <= 8
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 4 and 8 inclusive and the right number was determined to be exactly 8."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace Set.size (Set.insert k set) + 1 >= 2 by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.insert k set) + 1 >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Dict.size (Dict.insert k dict) + 1 >= 2 by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.insert k dict) + 1 >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Set.size (Set.remove k (Set.fromList [ e0, e1 ])) <= 2 by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.remove k (Set.fromList [ e0, e1 ])) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Dict.size (Dict.remove k (Dict.fromList [ e0, e1 ])) <= 2 by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.remove k (Dict.fromList [ e0, e1 ])) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace List.length (\"\" :: String.words str) >= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length ("" :: String.words str) >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (\"\" :: String.lines str) >= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length ("" :: String.lines str) >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (List.intersperse 0 (el0 :: el1 :: el2Up)) >= 3 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (List.intersperse 0 (el0 :: el1 :: el2Up)) >= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 3 and the right number was determined to be exactly 3."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (List.map3 f (List.repeat 3 0 ++ xs) (List.repeat 2 0) (List.repeat 4 0)) >= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (List.map3 f (List.repeat 3 0) (List.repeat 2 0 ++ xs) (List.repeat 4 0)) >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 3 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (List.map2 f (List.repeat 3 0) xs) <= 3 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (List.map2 f (List.repeat 3 0) xs) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (List.map4 f (List.take 3 zs) (List.repeat 5 0 ++ xs) (List.repeat 4 0) (List.take 2 ys)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (List.map4 f (List.take 3 zs) (List.repeat 5 0 ++ xs) (List.repeat 4 0) (List.take 2 ys)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace Dict.size (Dict.union (Dict.insert k0 dict0) (Dict.fromList [ (0,v0), (1,v1) ])) >= 2 by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.union (Dict.insert k0 dict0) (Dict.fromList [ (0,v0), (1,v1) ])) >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Dict.size (Dict.union (Dict.fromList [ e ]) (Dict.singleton k v)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.union (Dict.fromList [ e ]) (Dict.singleton k v)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Set.size (Set.union (Set.fromList (List.take 2 xs)) (Set.fromList (List.take 1 ys))) <= 3 by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.union (Set.fromList (List.take 2 xs)) (Set.fromList (List.take 1 ys))) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Set.size (Set.intersect (Set.fromList (List.take 2 xs)) (Set.fromList (List.take 3 ys))) <= 2 by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.intersect (Set.fromList (List.take 2 xs)) (Set.fromList (List.take 3 ys))) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Dict.size (Dict.intersect (Dict.fromList (List.take 2 xs)) (Dict.fromList (List.take 3 ys))) <= 2 by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.intersect (Dict.fromList (List.take 2 xs)) (Dict.fromList (List.take 3 ys))) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace List.length (x :: List.reverse (List.take 1 list)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (x :: List.reverse (List.take 1 list)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (x :: List.sort (List.take 1 list)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (x :: List.sort (List.take 1 list)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (x :: List.sortBy f (List.take 1 list)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (x :: List.sortBy f (List.take 1 list)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (x :: List.sortWith f (List.take 1 list)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (x :: List.sortWith f (List.take 1 list)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (x :: List.map f (List.take 1 list)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (x :: List.map f (List.take 1 list)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (x :: List.indexedMap f (List.take 1 list)) <= 2 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (x :: List.indexedMap f (List.take 1 list)) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (List.take 1 xs ++ Array.toList (Array.initialize 2 f)) <= 3 by True" <|
            \() ->
                """module A exposing (..)
import Array
a = List.length (List.take 1 xs ++ Array.toList (Array.initialize 2 f)) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = True
"""
                        ]
        , test "should replace List.length (List.take 1 xs ++ Array.toIndexedList (Array.initialize 2 f)) <= 3 by True" <|
            \() ->
                """module A exposing (..)
import Array
a = List.length (List.take 1 xs ++ Array.toIndexedList (Array.initialize 2 f)) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = True
"""
                        ]
        , test "should replace List.length (List.take 1 xs ++ Set.toList (Set.fromList [ e0, e1 ])) <= 3 by True" <|
            \() ->
                """module A exposing (..)
import Set
a = List.length (List.take 1 xs ++ Set.toList (Set.fromList [ e0, e1 ])) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace List.length (List.take 1 xs ++ Dict.toList (Dict.fromList [ e0, e1 ])) <= 3 by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.length (List.take 1 xs ++ Dict.toList (Dict.fromList [ e0, e1 ])) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace List.length (List.take 1 xs ++ String.toList (String.repeat 2 \"👀\")) <= 5 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (List.take 1 xs ++ String.toList (String.repeat 2 "👀")) <= 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 5 inclusive and the right number was determined to be exactly 5."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.length (\"x\" ++ String.reverse (String.repeat (clamp 1 2 n) \"👀\")) <= 5 by True" <|
            \() ->
                """module A exposing (..)
a = String.length ("x" ++ String.reverse (String.repeat (clamp 1 2 n) "👀")) <= 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 3 and 5 inclusive and the right number was determined to be exactly 5."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.length (\"x\" ++ String.map f (String.repeat (clamp 1 2 n) \"x\")) <= 5 by True" <|
            \() ->
                """module A exposing (..)
a = String.length ("x" ++ String.map f (String.repeat (clamp 1 2 n) "x")) <= 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 5 inclusive and the right number was determined to be exactly 5."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace Dict.size (Dict.insert k (Dict.map f (Dict.fromList ((0,v0) :: (1,v1) :: e2Up)))) >= 2 by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.insert k (Dict.map f (Dict.fromList ((0,v0) :: (1,v1) :: e2Up)))) >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 2 and the right number was determined to be exactly 2."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Set.size (Set.map f (Set.fromList [ 1, 2 ])) <= 2 by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.map f (Set.fromList [ 1, 2 ])) <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 1 and 2 inclusive and the right number was determined to be exactly 2."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Array.length (Array.push x (Array.map m (Array.initialize (clamp 1 2 n) f))) <= 3 by True" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.push x (Array.map m (Array.initialize (clamp 1 2 n) f))) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = True
"""
                        ]
        , test "should replace Array.length (Array.push x (Array.indexedMap m (Array.initialize (clamp 1 2 n) f))) <= 3 by True" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.push x (Array.indexedMap m (Array.initialize (clamp 1 2 n) f))) <= 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 2 and 3 inclusive and the right number was determined to be exactly 3."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = True
"""
                        ]
        , test "should replace List.length (let c = d in List.take 1 c) <= 1 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (let c = d in List.take 1 c) <= 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 1 inclusive and the right number was determined to be exactly 1."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (if c then [] else [ 0 ]) <= 1 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (if c then [] else [ 0 ]) <= 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 1 inclusive and the right number was determined to be exactly 1."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length (case n of 0 -> [] ; _ -> [ 0 ]) <= 1 by True" <|
            \() ->
                """module A exposing (..)
a = List.length (case n of 0 -> []
                           _ -> [ 0 ]) <= 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be between 0 and 1 inclusive and the right number was determined to be exactly 1."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        ]
