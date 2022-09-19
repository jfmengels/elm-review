module NoConfusingPrefixOperatorTest exposing (all)

import NoConfusingPrefixOperator exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoConfusingPrefixOperator" (baseTests ++ nonCommutativeOperatorTests)


baseTests : List Test
baseTests =
    [ test "should not report fully-applied operators" <|
        \() ->
            """module A exposing (..)
a = a < 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report operators not used in an application" <|
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
    ]


nonCommutativeOperatorTests : List Test
nonCommutativeOperatorTests =
    List.map
        (\( operator, details ) ->
            test ("should report non-commutative operators " ++ operator) <|
                \() ->
                    ("""module A exposing (..)
a = """ ++ operator ++ """ value
""")
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Found a confusing usage of prefix operator"
                                , details = details
                                , under = operator
                                }
                            ]
        )
        [ ( "(-)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a - b` is easy to understand, it is not as obvious to a reader that `(-) b` is the same as `\\a -> b - a`."
            , "Prefer using the form `\\b -> a - b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(/)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a / b` is easy to understand, it is not as obvious to a reader that `(/) b` is the same as `\\a -> b / a`."
            , "Prefer using the form `\\b -> a / b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(//)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a // b` is easy to understand, it is not as obvious to a reader that `(//) b` is the same as `\\a -> b // a`."
            , "Prefer using the form `\\b -> a // b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(^)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a ^ b` is easy to understand, it is not as obvious to a reader that `(^) b` is the same as `\\a -> b ^ a`."
            , "Prefer using the form `\\b -> a ^ b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(<)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a < b` is easy to understand, it is not as obvious to a reader that `(<) b` is the same as `\\a -> b < a`."
            , "Prefer using the form `\\b -> a < b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(>)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a > b` is easy to understand, it is not as obvious to a reader that `(>) b` is the same as `\\a -> b > a`."
            , "Prefer using the form `\\b -> a > b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(<=)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a <= b` is easy to understand, it is not as obvious to a reader that `(<=) b` is the same as `\\a -> b <= a`."
            , "Prefer using the form `\\b -> a <= b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(>=)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a >= b` is easy to understand, it is not as obvious to a reader that `(>=) b` is the same as `\\a -> b >= a`."
            , "Prefer using the form `\\b -> a >= b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(++)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a ++ b` is easy to understand, it is not as obvious to a reader that `(++) b` is the same as `\\a -> b ++ a`."
            , "Prefer using the form `\\b -> a ++ b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(|>)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a |> b` is easy to understand, it is not as obvious to a reader that `(|>) b` is the same as `\\a -> b |> a`."
            , "Prefer using the form `\\b -> a |> b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(<|)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a <| b` is easy to understand, it is not as obvious to a reader that `(<|) b` is the same as `\\a -> b <| a`."
            , "Prefer using the form `\\b -> a <| b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(>>)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a >> b` is easy to understand, it is not as obvious to a reader that `(>>) b` is the same as `\\a -> b >> a`."
            , "Prefer using the form `\\b -> a >> b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(<<)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a << b` is easy to understand, it is not as obvious to a reader that `(<<) b` is the same as `\\a -> b << a`."
            , "Prefer using the form `\\b -> a << b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(|.)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a |. b` is easy to understand, it is not as obvious to a reader that `(|.) b` is the same as `\\a -> b |. a`."
            , "Prefer using the form `\\b -> a |. b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(|=)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a |= b` is easy to understand, it is not as obvious to a reader that `(|=) b` is the same as `\\a -> b |= a`."
            , "Prefer using the form `\\b -> a |= b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(</>)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a </> b` is easy to understand, it is not as obvious to a reader that `(</>) b` is the same as `\\a -> b </> a`."
            , "Prefer using the form `\\b -> a </> b` which will be a lot easier to understand and to get right."
            ]
          )
        , ( "(<?>)"
          , [ "Prefix operators for operators like this one are very error-prone. While `a <?> b` is easy to understand, it is not as obvious to a reader that `(<?>) b` is the same as `\\a -> b <?> a`."
            , "Prefer using the form `\\b -> a <?> b` which will be a lot easier to understand and to get right."
            ]
          )
        ]
