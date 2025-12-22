module Simplify.ComparisonTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


all : Test
all =
    describe "comparison tests"
        [ lessThanTests
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
        , test "should simplify n > n to False, expect NaN not enabled" <|
            \() ->
                """module A exposing (..)
a = n > n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) with two equal operands results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify n >= n to True, expect NaN enabled" <|
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
        , test "should simplify n <= n to True, expect NaN not enabled" <|
            \() ->
                """module A exposing (..)
a = n <= n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) with two equal operands results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report n > n when expect NaN is enabled" <|
            \() ->
                """module A exposing (..)
a = n > n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should not report n <= n when expect NaN is enabled" <|
            \() ->
                """module A exposing (..)
a = n <= n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
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
        ]
