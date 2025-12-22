module Simplify.SubTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Sub"
        [ subBatchTests
        , subMapTests
        ]


subBatchTests : Test
subBatchTests =
    describe "Sub.batch"
        [ test "should not report Sub.batch used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Sub.batch
a = Sub.batch b
a = Sub.batch [ b, x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Sub.batch [] by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.batch []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on [] will result in Sub.none"
                            , details = [ "You can replace this call by Sub.none." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        , test "should replace Sub.batch [ a, Sub.none, b ] by Sub.batch [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ a, Sub.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on a list containing an irrelevant Sub.none"
                            , details = [ "Including Sub.none in the list does not change the result of this call. You can remove the Sub.none element." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ a, b ]
"""
                        ]
        , test "should replace Sub.batch [ Sub.none ] by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ Sub.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        , test "should replace Sub.batch [ b ] by b" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should replace Sub.batch [ f n ] by (f n)" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ f n ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        , test "should replace Sub.batch << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = Sub.batch << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Sub.batch [ b, Sub.none ] by Sub.batch []" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ b, Sub.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on a list containing an irrelevant Sub.none"
                            , details = [ "Including Sub.none in the list does not change the result of this call. You can remove the Sub.none element." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ b ]
"""
                        ]
        , test "should replace Sub.batch [ Sub.none, b ] by Sub.batch [ b ]" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ Sub.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sub.batch on a list containing an irrelevant Sub.none"
                            , details = [ "Including Sub.none in the list does not change the result of this call. You can remove the Sub.none element." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ b ]
"""
                        ]
        , test "should replace Sub.batch [ subscription0, Sub.batch [ subscription1, subscription2 ], subscription3, Sub.batch [ subscription4, subscription5 ] ] by Sub.batch [ subscription0, subscription1, subscription2, subscription3, subscription4, subscription5 ]" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ subscription0, Sub.batch [ subscription1, subscription2 ], subscription3, Sub.batch [ subscription4, subscription5 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Nested Sub.batch calls can be spread"
                            , details = [ "You can move the elements from the inner Sub.batch calls to inside this outer Sub.batch call." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ subscription0,  subscription1, subscription2 , subscription3,  subscription4, subscription5  ]
"""
                        ]
        ]


subMapTests : Test
subMapTests =
    describe "Sub.map"
        [ test "should replace Sub.map identity sub by sub" <|
            \() ->
                """module A exposing (..)
a = Sub.map identity sub
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Sub.map with an identity function will always return the same given subscription"
                            , details = [ "You can replace this call by the subscription itself." ]
                            , under = "Sub.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = sub
"""
                        ]
        , test "should replace Sub.map f Sub.none by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.map f Sub.none
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Sub.map on Sub.none will result in Sub.none"
                            , details = [ "You can replace this call by Sub.none." ]
                            , under = "Sub.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        ]
