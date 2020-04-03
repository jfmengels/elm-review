module NoExtraBooleanComparisonTest exposing (all)

import NoExtraBooleanComparison exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


tests : List Test
tests =
    [ test "should not report condition without an operator" <|
        \() ->
            testRule "a = if n then 1 else 2"
                |> Review.Test.expectNoErrors
    , test "should not report condition with integer operators" <|
        \() ->
            testRule """
a = if n < 1 then 1 else 2
b = if n <= 1 then 1 else 2
c = if n > 1 then 1 else 2
d = if n >= 1 then 1 else 2
"""
                |> Review.Test.expectNoErrors
    , test "should not report condition using `not`" <|
        \() ->
            testRule "a = if not n then 1 else 2"
                |> Review.Test.expectNoErrors
    , test "should report condition with `expr == True`" <|
        \() ->
            testRule "a = if b == True then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `True`"
                        , details = [ "You can simplify this expression by removing the `==` operator and the value `True`." ]
                        , under = "b == True"
                        }
                    ]
    , test "should report condition with `True == expr`" <|
        \() ->
            testRule "a = if True == b then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `True`"
                        , details = [ "You can simplify this expression by removing the `==` operator and the value `True`." ]
                        , under = "True == b"
                        }
                    ]
    , test "should report condition with `expr == False`" <|
        \() ->
            testRule "a = if b == False then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `False`"
                        , details = [ "You can simplify this expression by removing the `==` operator and the value `False`." ]
                        , under = "b == False"
                        }
                    ]
    , test "should report condition with `False == expr`" <|
        \() ->
            testRule "a = if False == b then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `False`"
                        , details = [ "You can simplify this expression by removing the `==` operator and the value `False`." ]
                        , under = "False == b"
                        }
                    ]
    , test "should report condition with `expr /= True`" <|
        \() ->
            testRule "a = if b /= True then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `True`"
                        , details = [ "You can simplify this expression by removing the `/=` operator and the value `True`." ]
                        , under = "b /= True"
                        }
                    ]
    , test "should report condition with `True /= expr`" <|
        \() ->
            testRule "a = if True /= b then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `True`"
                        , details = [ "You can simplify this expression by removing the `/=` operator and the value `True`." ]
                        , under = "True /= b"
                        }
                    ]
    , test "should report condition with `expr /= False`" <|
        \() ->
            testRule "a = if b /= False then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `False`"
                        , details = [ "You can simplify this expression by removing the `/=` operator and the value `False`." ]
                        , under = "b /= False"
                        }
                    ]
    , test "should report condition with `False /= expr`" <|
        \() ->
            testRule "a = if False /= b then 1 else 2"
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unnecessary comparison with `False`"
                        , details = [ "You can simplify this expression by removing the `/=` operator and the value `False`." ]
                        , under = "False /= b"
                        }
                    ]
    ]


all : Test
all =
    describe "NoExtraBooleanComparison" tests
