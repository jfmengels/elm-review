module NoConfusingPrefixOperatorTest exposing (all)

import NoConfusingPrefixOperator exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoConfusingPrefixOperator"
        [ test "should not report fully-applied operators" <|
            \() ->
                """module A exposing (..)
a = a < 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report operators not used in an application" <|
            \() ->
                """module A exposing (..)
a = (<)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report commutative operators (+) (*) (==) (/=) (&&) (||)" <|
            \() ->
                """module A exposing (..)
a = (+) 1
b = (*) 1
c = (==) 1
d = (/=) 1
e = (&&) True
f = (||) True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report non-commutative operator (-)" <|
            \() ->
                """module A exposing (..)
a = (-) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a - b` is easy to understand, it is not as obvious to a reader that `(-) b` is the same as `\\a -> b - a`."
                                , "Prefer using the form `\\b -> a - b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(-)"
                            }
                        ]
        , test "should report non-commutative operator (/)" <|
            \() ->
                """module A exposing (..)
a = (/) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a / b` is easy to understand, it is not as obvious to a reader that `(/) b` is the same as `\\a -> b / a`."
                                , "Prefer using the form `\\b -> a / b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(/)"
                            }
                        ]
        , test "should report non-commutative operator (//)" <|
            \() ->
                """module A exposing (..)
a = (//) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a // b` is easy to understand, it is not as obvious to a reader that `(//) b` is the same as `\\a -> b // a`."
                                , "Prefer using the form `\\b -> a // b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(//)"
                            }
                        ]
        , test "should report non-commutative operator (^)" <|
            \() ->
                """module A exposing (..)
a = (^) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a ^ b` is easy to understand, it is not as obvious to a reader that `(^) b` is the same as `\\a -> b ^ a`."
                                , "Prefer using the form `\\b -> a ^ b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(^)"
                            }
                        ]
        , test "should report non-commutative operator (<)" <|
            \() ->
                """module A exposing (..)
a = (<) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a < b` is easy to understand, it is not as obvious to a reader that `(<) b` is the same as `\\a -> b < a`."
                                , "Prefer using the form `\\b -> a < b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(<)"
                            }
                        ]
        , test "should report non-commutative operator (>)" <|
            \() ->
                """module A exposing (..)
a = (>) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a > b` is easy to understand, it is not as obvious to a reader that `(>) b` is the same as `\\a -> b > a`."
                                , "Prefer using the form `\\b -> a > b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(>)"
                            }
                        ]
        , test "should report non-commutative operator (<=)" <|
            \() ->
                """module A exposing (..)
a = (<=) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a <= b` is easy to understand, it is not as obvious to a reader that `(<=) b` is the same as `\\a -> b <= a`."
                                , "Prefer using the form `\\b -> a <= b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(<=)"
                            }
                        ]
        , test "should report non-commutative operator (>=)" <|
            \() ->
                """module A exposing (..)
a = (>=) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a >= b` is easy to understand, it is not as obvious to a reader that `(>=) b` is the same as `\\a -> b >= a`."
                                , "Prefer using the form `\\b -> a >= b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(>=)"
                            }
                        ]
        , test "should report non-commutative operator (++)" <|
            \() ->
                """module A exposing (..)
a = (++) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a ++ b` is easy to understand, it is not as obvious to a reader that `(++) b` is the same as `\\a -> b ++ a`."
                                , "Prefer using the form `\\b -> a ++ b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(++)"
                            }
                        ]
        , test "should report non-commutative operator (|>)" <|
            \() ->
                """module A exposing (..)
a = (|>) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a |> b` is easy to understand, it is not as obvious to a reader that `(|>) b` is the same as `\\a -> b |> a`."
                                , "Prefer using the form `\\b -> a |> b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(|>)"
                            }
                        ]
        , test "should report non-commutative operator (<|)" <|
            \() ->
                """module A exposing (..)
a = (<|) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a <| b` is easy to understand, it is not as obvious to a reader that `(<|) b` is the same as `\\a -> b <| a`."
                                , "Prefer using the form `\\b -> a <| b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(<|)"
                            }
                        ]
        , test "should report non-commutative operator (>>)" <|
            \() ->
                """module A exposing (..)
a = (>>) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a >> b` is easy to understand, it is not as obvious to a reader that `(>>) b` is the same as `\\a -> b >> a`."
                                , "Prefer using the form `\\b -> a >> b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(>>)"
                            }
                        ]
        , test "should report non-commutative operator (<<)" <|
            \() ->
                """module A exposing (..)
a = (<<) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a << b` is easy to understand, it is not as obvious to a reader that `(<<) b` is the same as `\\a -> b << a`."
                                , "Prefer using the form `\\b -> a << b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(<<)"
                            }
                        ]
        , test "should report non-commutative operator (|.)" <|
            \() ->
                """module A exposing (..)
a = (|.) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a |. b` is easy to understand, it is not as obvious to a reader that `(|.) b` is the same as `\\a -> b |. a`."
                                , "Prefer using the form `\\b -> a |. b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(|.)"
                            }
                        ]
        , test "should report non-commutative operator (|=)" <|
            \() ->
                """module A exposing (..)
a = (|=) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a |= b` is easy to understand, it is not as obvious to a reader that `(|=) b` is the same as `\\a -> b |= a`."
                                , "Prefer using the form `\\b -> a |= b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(|=)"
                            }
                        ]
        , test "should report non-commutative operator (</>)" <|
            \() ->
                """module A exposing (..)
a = (</>) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a </> b` is easy to understand, it is not as obvious to a reader that `(</>) b` is the same as `\\a -> b </> a`."
                                , "Prefer using the form `\\b -> a </> b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(</>)"
                            }
                        ]
        , test "should report non-commutative operator (<?>)" <|
            \() ->
                """module A exposing (..)
a = (<?>) value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found a confusing usage of prefix operator"
                            , details =
                                [ "Prefix operators for operators like this one are very error-prone. While `a <?> b` is easy to understand, it is not as obvious to a reader that `(<?>) b` is the same as `\\a -> b <?> a`."
                                , "Prefer using the form `\\b -> a <?> b` which will be a lot easier to understand and to get right."
                                ]
                            , under = "(<?>)"
                            }
                        ]
        ]
