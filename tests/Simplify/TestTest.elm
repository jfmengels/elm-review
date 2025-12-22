module Simplify.TestTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Test"
        [ testConcatTests
        ]


testConcatTests : Test
testConcatTests =
    describe "Test.concat"
        [ test "should not report Test.concat with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = Test.concat
a1 = Test.concat testList
a2 = Test.concat [ test0, test1 ]
a3 = Test.concat [ Test.describe "test0" [ test0, test1 ], test2 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Test.concat [ test ] by test" <|
            \() ->
                """module A exposing (..)
a = Test.concat [ test ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Test.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Test.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = test
"""
                        ]
        , test "should replace Test.concat [ test0, Test.concat [ test1, test2 ], test3, Test.concat [ test4, test5 ] ] by Test.concat [ test0, test1, test2, test3, test4, test5 ]" <|
            \() ->
                """module A exposing (..)
a = Test.concat [ test0, Test.concat [ test1, test2 ], test3, Test.concat [ test4, test5 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Nested Test.concat calls can be spread"
                            , details = [ "You can move the elements from the inner Test.concat calls to inside this outer Test.concat call." ]
                            , under = "Test.concat"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Test.concat [ test0,  test1, test2 , test3,  test4, test5  ]
"""
                        ]
        ]
