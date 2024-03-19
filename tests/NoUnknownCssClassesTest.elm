module NoUnknownCssClassesTest exposing (all)

import NoUnknownCssClasses exposing (defaults, rule, withCssFiles, withHardcodedKnownClasses)
import Review.Project as Project exposing (Project)
import Review.Test
import Review.Test.Dependencies
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
                            { message = "Unknown CSS class \"unknown\""
                            , details = [ "I could not find this class in CSS files. Have you made a typo? Here are similarly-named classes: TODO" ]
                            , under = "unknown"
                            }
                        ]
        , test "should not report an error when encountering an CSS class specified in the configuration" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class "known" ] []
"""
                    |> Review.Test.run (defaults |> withHardcodedKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering an unknown CSS class through Html.Attributes.class in <| pipe" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class <| "unknown" ] []
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unknown CSS class \"unknown\""
                            , details = [ "I could not find this class in CSS files. Have you made a typo? Here are similarly-named classes: TODO" ]
                            , under = "unknown"
                            }
                        ]
        , test "should report an error when encountering an unknown CSS class through Html.Attributes.class in |> pipe" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ "unknown" |> Attr.class ] []
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unknown CSS class \"unknown\""
                            , details = [ "I could not find this class in CSS files. Have you made a typo? Here are similarly-named classes: TODO" ]
                            , under = "unknown"
                            }
                        ]
        , test "should not report an error when encountering CSS classes found in specified files" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ "known red" |> Attr.class ] []
"""
                    |> Review.Test.runWithProjectData projectWithCssClasses (defaults |> withCssFiles [ "*.css" ] |> rule)
                    |> Review.Test.expectNoErrors
        ]


projectWithCssClasses : Project
projectWithCssClasses =
    Project.addExtraFiles
        [ { path = "some-file.css"
          , content = """
.known {
    color: blue;
}
.red {
    color: red;
}
"""
          }
        ]
        Review.Test.Dependencies.projectWithElmCore
