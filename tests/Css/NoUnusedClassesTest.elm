module Css.NoUnusedClassesTest exposing (all)

import Css.ClassFunction as ClassFunction exposing (CssArgument, fromLiteral)
import Css.NoUnusedClasses exposing (cssFiles, dontReport, rule, withCssUsingFunctions)
import Dict
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Review.FilePattern as FilePattern exposing (FilePattern)
import Review.Project as Project exposing (Project)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Css.NoUnusedClasses"
        [ test "should not report an error when strings don't seem to be CSS classes" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [] [ Html.text "ok" ]
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should report an error when a CSS class is found in the CSS files but is unused" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class "unknown" ] []
"""
                    |> Review.Test.runWithProjectData projectWithCssClasses
                        (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectErrorsForExtraFile "some-file.css"
                        [ Review.Test.error
                            { message = "Found unused CSS classes"
                            , details =
                                [ "This file declared the usage of some CSS classes for which I could not any usage in the Elm codebase. Please check that no typo was made in the name of the classes, and remove them if they still seem unused."
                                , "Here are the classes that seem unused: unused"
                                ]
                            , under = "-- First line"
                            }
                        ]
        ]


classFromAttrFunction : ClassFunction.Arguments -> List CssArgument
classFromAttrFunction { firstArgument } =
    [ fromLiteral firstArgument ]


projectWithCssClasses : Project
projectWithCssClasses =
    Project.addExtraFiles
        (Dict.fromList
            [ ( "some-file.css"
              , """-- First line
.unused {
    color: blue;
}
"""
              )
            ]
        )
        Review.Test.Dependencies.projectWithElmCore
