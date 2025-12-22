module Simplify.RecordUpdateTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Record update"
        [ test "should not simplify when assigning a different field or a value" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = b.c, e = c.e, f = g b.f, g = b.g.h }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify when assigning a field to itself applied to an extra argument" <|
            \() ->
                """module A exposing (..)
a = { b | d = .d b extraArgument }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify when assigning a field in a non-update record assignment" <|
            \() ->
                """module A exposing (..)
a = { d = b.d, c = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove the updates that assigns the previous value of a field to itself (first)" <|
            \() ->
                """module A exposing (..)
a = { b | d = b.d, c = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1 }
"""
                        ]
        , test "should remove the update record syntax when it assigns the previous value of a field to itself and it is the only assignment" <|
            \() ->
                """module A exposing (..)
a = { b | d = b.d }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (not first)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = b.d }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (using parens)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = (b.d) }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        , test "should remove the update record syntax when it assigns the previous value of a field to itself and it is the only assignment (using access function application)" <|
            \() ->
                """module A exposing (..)
a = { b | d = .d b }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = ".d b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should remove the update record syntax when it assigns the previous value of a field to itself and it is the only assignment (using access function <|)" <|
            \() ->
                """module A exposing (..)
a = { b | d = .d <| b }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = ".d <| b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should remove the update record syntax when it assigns the previous value of a field to itself and it is the only assignment (using access function |>)" <|
            \() ->
                """module A exposing (..)
a = { b | d = b |> .d }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b |> .d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (not first and using access function application)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = .d b }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = ".d b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (using parens and access function application)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = (.d b) }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = ".d b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        ]
