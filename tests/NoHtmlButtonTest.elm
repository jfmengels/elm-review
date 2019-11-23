module NoHtmlButtonTest exposing (all)

import Elm.Type as Type
import NoHtmlButton exposing (rule)
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
        |> Project.withDependency
            { packageName = "elm/html"
            , modules =
                [ { name = "Html"
                  , comment = ""
                  , unions = []
                  , aliases = []
                  , values =
                        [ { name = "button"
                          , comment = ""
                          , tipe =
                                -- "List.List (Html.Attribute msg) -> List.List (Html.Html msg) -> Html.Html msg"
                                Type.Lambda (Type.Type "List.List" [ Type.Type "Html.Attribute" [ Type.Var "msg" ] ])
                                    (Type.Lambda (Type.Type "List.List" [ Type.Type "Html.Html" [ Type.Var "msg" ] ]) (Type.Type "Html.Html" [ Type.Var "msg" ]))
                          }
                        ]
                  , binops = []
                  }
                ]
            }


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
    , test "should not report the use of `button` when it has been imported using `exposing (..)` and the dependency is known, but it has been redefined at the top-level" <|
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
    ]


all : Test
all =
    describe "NoHtmlButton" tests
