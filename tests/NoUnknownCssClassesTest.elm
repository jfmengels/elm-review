module NoUnknownCssClassesTest exposing (all)

import Dict
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import NoUnknownCssClasses exposing (CssArgument, defaults, fromLiteral, rule, withCssFiles, withCssUsingFunctions, withHardcodedKnownClasses)
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
    Html.span [ "known red-faint under_score" |> Attr.class ] []
"""
                    |> Review.Test.runWithProjectData projectWithCssClasses (defaults |> withCssFiles [ "*.css" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering a non-literal argument for Html.Attributes.class" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.class model.class
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "model.class"
                            }
                        ]
        , test "should report an error when encountering a non-literal argument for Html.Attributes.classList" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.classList model.classList
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "model.classList"
                            }
                        ]
        , test "should report an error when encountering non-literal CSS classes for Html.Attributes.classList" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.classList
        [ ( "known", model.a )
        , ( variable, model.b )
        ]
"""
                    |> Review.Test.run (defaults |> withHardcodedKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "variable"
                            }
                        ]
        , test "should not report an error when encountering a literal CSS class with a custom CSS function" <|
            \() ->
                """module A exposing (..)
import Class

view model =
    Class.fromString "known"
"""
                    |> Review.Test.run
                        (defaults
                            |> withHardcodedKnownClasses [ "known" ]
                            |> withCssUsingFunctions (Dict.fromList [ ( ( [ "Class" ], "fromString" ), classFromAttrFunction ) ])
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering a non-literal CSS class with a custom CSS function" <|
            \() ->
                """module A exposing (..)
import Class

view model =
    Class.fromString model.a
"""
                    |> Review.Test.run
                        (defaults
                            |> withHardcodedKnownClasses [ "known" ]
                            |> withCssUsingFunctions (Dict.fromList [ ( ( [ "Class" ], "fromString" ), classFromAttrFunction ) ])
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "model.a"
                            }
                        ]
        , test "should report an error when being unable to parse a CSS file" <|
            \() ->
                """module A exposing (..)
import Class

view model =
    Class.fromString model.a
"""
                    |> Review.Test.runWithProjectData projectWithUnparsableCssClasses (defaults |> withCssFiles [ "*.css" ] |> rule)
                    |> Review.Test.expectErrorsForModules
                        [ ( "some-file.css"
                          , [ Review.Test.error
                                { message = "Unable to parse CSS file `some-file.css`"
                                , details = [ "Please check that this file is syntactically correct. It is possible that I'm mistaken as my CSS parser is still very naive. Contributions are welcome to solve the issue." ]
                                , under = "-- First line"
                                }
                            ]
                          )
                        ]
        ]


classFromAttrFunction : { firstArgument : Node Expression, restOfArguments : List (Node Expression) } -> List CssArgument
classFromAttrFunction { firstArgument } =
    [ fromLiteral firstArgument ]


projectWithCssClasses : Project
projectWithCssClasses =
    Project.addExtraFiles
        [ { path = "some-file.css"
          , content = """-- First line
.known {
    color: blue;
}
.red-faint {
    color: red;
}
.under_score {
    color: green;
}
"""
          }
        ]
        Review.Test.Dependencies.projectWithElmCore


projectWithUnparsableCssClasses : Project
projectWithUnparsableCssClasses =
    Project.addExtraFiles
        [ { path = "some-file.css"
          , content = """-- First line
.known {
    color: blue;
}
.red-faint {
    color: red;
-- missing closing curly brace
"""
          }
        ]
        Review.Test.Dependencies.projectWithElmCore
