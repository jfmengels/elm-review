module Simplify.AppliedLambdaTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Applied lambda functions"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = f ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace (\\() -> x) () by x" <|
            \() ->
                """module A exposing (..)
a = (\\() -> x) ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace (\\_ -> x) a by x" <|
            \() ->
                """module A exposing (..)
a = (\\_ -> x) a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace (\\x -> x) y by y" <|
            \() ->
                """module A exposing (..)
a = (\\x -> x) y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary identity function"
                            , details = [ "This function returns the argument it is given without any changes. Calling it with an argument is the same thing as writing the argument on its own." ]
                            , under = "\\x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace (\\x -> x) <| f <| y by f <| y" <|
            \() ->
                """module A exposing (..)
a = (\\x -> x) <| f <| y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary identity function"
                            , details = [ "This function returns the argument it is given without any changes. Calling it with an argument is the same thing as writing the argument on its own." ]
                            , under = "\\x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f <| y
"""
                        ]
        , test "should replace y |> f |> (\\x -> x) by y |> f" <|
            \() ->
                """module A exposing (..)
a = y |> f |> (\\x -> x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary identity function"
                            , details = [ "This function returns the argument it is given without any changes. Calling it with an argument is the same thing as writing the argument on its own." ]
                            , under = "\\x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y |> f
"""
                        ]
        , test "should replace f >> (\\x -> x) by f" <|
            \() ->
                """module A exposing (..)
a = f >> (\\x -> x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The identity function should be removed"
                            , details = [ "Composing a function with the identity function is the same as simplify referencing the function." ]
                            , under = "(\\x -> x)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace f >> (\\_ -> x) by (\\_ -> x)" <|
            \() ->
                """module A exposing (..)
a = f >> (\\_ -> x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with lambda will be ignored"
                            , details = [ "The lambda function ignores the first argument, meaning it will swallow the function composed into it." ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ -> x)
"""
                        ]
        , test "should replace (\\() y -> x) () by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\() y -> x) ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x)
"""
                        ]
        , test "should replace (\\() y -> x) () b by (\\y -> x) b" <|
            \() ->
                """module A exposing (..)
a = (\\() y -> x) () b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x) b
"""
                        ]
        , test "should replace (\\_ y -> x) a by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\_ y -> x) a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x)
"""
                        ]
        , test "should replace (\\_ -> x) b c by x c" <|
            \() ->
                """module A exposing (..)
a = (\\_ -> x) b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x c
"""
                        ]
        , test "should replace (\\_ -> x |> f) b c by (x |> f) c" <|
            \() ->
                """module A exposing (..)
a = (\\_ -> x |> f) b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> f) c
"""
                        ]
        , test "should replace a |> (\\_ y -> x) by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = b |> (\\_ y -> x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x)
"""
                        ]
        , test "should replace (\\_ y -> x) a b by (\\y -> x) b" <|
            \() ->
                """module A exposing (..)
a = (\\_ y -> x) a b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x) b
"""
                        ]
        , test "should replace a |> f |> (\\_ y -> x) by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = b |> f |> (\\_ y -> x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x)
"""
                        ]
        , test "should replace (\\_ y -> x) <| f <| a by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\_ y -> x) <| f <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x)
"""
                        ]
        , test "should replace (\\y _ -> x) b c by (\\y -> x) b" <|
            \() ->
                """module A exposing (..)
a = (\\y _ -> x) b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x) b
"""
                        ]
        , test "should replace (\\y _ z -> x) b c by (\\y z -> x) b" <|
            \() ->
                """module A exposing (..)
a = (\\y _ z -> x) b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y z -> x) b
"""
                        ]
        , test "should replace (\\y _ -> x) b c d by (\\y -> x) b d" <|
            \() ->
                """module A exposing (..)
a = (\\y _ -> x) b c d
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x) b d
"""
                        ]
        , test "should replace (\\y _ -> x) b <| c by (\\y -> x) b" <|
            \() ->
                """module A exposing (..)
a = (\\y _ -> x) b <| c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x) b
"""
                        ]
        , test "should replace c |> (\\y _ -> x) b by (\\y -> x) b" <|
            \() ->
                """module A exposing (..)
a = x |> (\\y _ -> x) b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x) b
"""
                        ]
        , test "should not report non-simplifiable lambdas that are directly called with an argument" <|
            \() ->
                """module A exposing (..)
a = (\\x y -> x + y) n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report non-simplifiable lambdas that are directly called in a |> pipeline" <|
            \() ->
                """module A exposing (..)
a = n |> (\\x y -> x + y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report lambdas that are directly called in a |> pipeline if the argument is a pipeline itself" <|
            \() ->
                """module A exposing (..)
a = n |> f |> (\\x y -> x + y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report non-simplifiable lambdas that are directly called in a  <| pipeline" <|
            \() ->
                """module A exposing (..)
a = (\\x y -> x + y) <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report lambdas non-simplifiable lambdas that are directly called in a <| pipeline if the argument is a pipeline itself" <|
            \() ->
                """module A exposing (..)
a = (\\x y -> x + y) <| f <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]
