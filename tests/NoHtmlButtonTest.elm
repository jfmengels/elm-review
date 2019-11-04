module NoHtmlButtonTest exposing (all)

import NoHtmlButton exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


message : String
message =
    "Do not use `Html.button` directly"


details : List String
details =
    [ "At fruits.com, we've built a nice `Button` module that suits our needs better. Using this module instead of `Html.button` ensures we have a consistent button experience across the website."
    ]


tests : List Test
tests =
    [ test "should not report the use of non-`Html.button` function or values" <|
        \() ->
            testRule """
a = bar 1
b = Foo.bar 1
c = baz
"""
                |> Review.Test.expectNoErrors
    , test "should report the use of `Html.button` as an expression" <|
        \() ->
            testRule """
import Html
a = Html.button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "Html.button"
                        }
                    ]
    , test "should report the use of `Html.button` even if it is not imported" <|
        \() ->
            testRule """
a = Html.button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "Html.button"
                        }
                    ]
    , test "should not report the use of `H.button` when H is not an alias for Html" <|
        \() ->
            testRule """
a = H.button
"""
                |> Review.Test.expectNoErrors
    , test "should report the use of `H.button` when H is an alias for Html" <|
        \() ->
            testRule """
import Html as H
a = H.button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "H.button"
                        }
                    ]
    ]


all : Test
all =
    describe "NoHtmlButton" tests
