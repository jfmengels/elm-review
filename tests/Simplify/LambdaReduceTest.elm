module Simplify.LambdaReduceTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "lambda reduce"
        [ test "should detect (\\error -> Err error) as Err function" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << (\\error -> Err error)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> error |> Err) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> error |> Err)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> Err <| error) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> Err <| error)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error sorry -> (error |> Err) <| sorry) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> \\sorry -> (error |> Err) <| sorry)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> \\sorry -> (error |> Err) <| sorry) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> \\sorry -> (error |> Err) <| sorry)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\result -> Result.mapError f result) as Result.mapError f call" <|
            \() ->
                """module A exposing (..)
a = (\\result -> Result.mapError f result) << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\result -> \\sorry -> (result |> Result.mapError f) <| sorry) as Result.mapError f call" <|
            \() ->
                """module A exposing (..)
a = (\\result -> \\sorry -> (result |> Result.mapError f) <| sorry) << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        ]
