module NoUnknownCssClassesTest exposing (all)

import NoUnknownCssClasses exposing (defaults, rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnknownCssClasses"
        [ test "should not report an error when strings don't seem to be CSS classes" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [] [ Html.text "ok" ]
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering an unknown CSS class through Html.Attributes.class" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class "unknown" ] []
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "\"unknown\""
                            }
                        ]
        ]
