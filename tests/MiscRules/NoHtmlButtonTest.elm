module MiscRules.NoHtmlButtonTest exposing (all)

import Fixtures.Dependencies as Dependencies
import MiscRules.NoHtmlButton exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


projectWithHtmlDependency : Project
projectWithHtmlDependency =
    Project.new
        |> Project.addDependency Dependencies.elmHtml


testRuleWithHtmlDependency : String -> ReviewResult
testRuleWithHtmlDependency string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.runWithProjectData projectWithHtmlDependency rule


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
    , test "should report the use of `button` when it has been imported explicitly" <|
        \() ->
            testRule """
import Html exposing (button)
a = button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "button"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 11 } }
                    ]
    , test "should not report the use of `button` when it has been imported using `exposing (..)` and the dependency is not known" <|
        \() ->
            testRule """
import Html exposing (..)
a = button
"""
                |> Review.Test.expectNoErrors
    , test "should report the use of `button` when it has been imported using `exposing (..)` and the dependency is known" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a = button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "button"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 11 } }
                    ]
    , test "should not report the use of `button` when it has been imported using `exposing (..)` and the dependency is known, but it has been redefined at the top-level as a function" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a = button
button = 1
"""
                |> Review.Test.expectNoErrors
    , test "should not report the use of `button` when it has been imported using `exposing (..)` and the dependency is known, but it has been redefined in an accessible let..in declaration" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a = let
  button = 1
  in button
"""
                |> Review.Test.expectNoErrors
    , test "should not report the use of `button` when it has been imported using `exposing (..)` and the dependency is known, but it has been redefined at the top-level as a port" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a = button
port button : (() -> msg) -> Sub msg
"""
                |> Review.Test.expectNoErrors
    , test "should not report the use of `button` when it has been imported using `exposing (button)`, but it has been redefined in an accessible let..in declaration" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a = let
  button = 1
  in button
"""
                |> Review.Test.expectNoErrors
    , test "should report the use of `button` when it has been imported using `exposing (..)` and the dependency is known, and it has been redefined in an out-of-scope let..in declaration" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a = let
  button = 1
  in 2
b = button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "button"
                        }
                        |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 11 } }
                    ]
    , test "should not report the use of `button` if it is shadowed by an accessible parameter" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a button =
  button
"""
                |> Review.Test.expectNoErrors
    , test "should report the use of `button` even if it was defined as a parameter somewhere else" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a button =
  button
b =
  button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "button"
                        }
                        |> Review.Test.atExactly { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                    ]
    , test "should not report the use of `button` if it is shadowed by a name while pattern matching" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a =
  case c of
    Foo button -> button
"""
                |> Review.Test.expectNoErrors
    , test "should report the use of `button` even if a different pattern matching has a pattern that shadows it" <|
        \() ->
            testRuleWithHtmlDependency """
import Html exposing (..)
a =
  case c of
    Foo button -> button
    Bar -> button
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "button"
                        }
                        |> Review.Test.atExactly { start = { row = 8, column = 12 }, end = { row = 8, column = 18 } }
                    ]
    ]


all : Test
all =
    describe "NoHtmlButton" tests
