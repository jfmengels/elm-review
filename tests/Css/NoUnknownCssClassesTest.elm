module Css.NoUnknownCssClassesTest exposing (all)

import Css.ClassFunction as ClassFunction exposing (CssArgument, fromLiteral)
import Css.NoUnknownCssClasses exposing (addKnownClasses, cssFiles, rule, withCssUsingFunctions)
import Dict
import Review.FilePattern as FilePattern
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
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering an unknown CSS class through Html.Attributes.class" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class "unknown" ] []
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known", "bar", "unknown2" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unknown CSS class \"unknown\""
                            , details =
                                [ "I could not find this class in CSS files. Have you made a typo?"
                                , "Here are similarly-named classes:\n - unknown2\n - known"
                                ]
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
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the class argument is empty" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class "" ] []
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the class argument is only made out of spaces" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class "  " ] []
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering an unknown CSS class through Html.Attributes.class in <| pipe" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Html.span [ Attr.class <| "unknown" ] []
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unknown CSS class \"unknown\""
                            , details = [ "I could not find this class in CSS files. Have you made a typo?" ]
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
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unknown CSS class \"unknown\""
                            , details = [ "I could not find this class in CSS files. Have you made a typo?" ]
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
                    |> Review.Test.runWithProjectData projectWithCssClasses (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering a non-literal argument for Html.Attributes.class" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.class model.class
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
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
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
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
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "variable"
                            }
                        ]
        , test "should report an error when encountering a non-literal argument to Html.Attributes.attribute \"class\"" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.attribute "class" model.class
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "model.class"
                            }
                        ]
        , test "should not report an error when encountering a known class argument to Html.Attributes.attribute \"class\"" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.attribute "class" "known"
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when Html.Attributes.attribute is used with something else than \"class\"" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.attribute "id" model.id
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when Html.Attributes.attribute is used with a non-literal attribute name" <|
            \() ->
                """module A exposing (..)
import Html
import Html.Attributes as Attr

view model =
    Attr.attribute name model.name
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> rule)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when encountering a literal CSS class with a custom CSS function" <|
            \() ->
                """module A exposing (..)
import Class

view model =
    Class.fromString "known"
"""
                    |> Review.Test.run
                        (cssFiles [ FilePattern.include "*.css" ]
                            |> addKnownClasses [ "known" ]
                            |> withCssUsingFunctions [ ( "Class.fromString", classFromAttrFunction ) ]
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
                        (cssFiles [ FilePattern.include "*.css" ]
                            |> addKnownClasses [ "known" ]
                            |> withCssUsingFunctions [ ( "Class.fromString", classFromAttrFunction ) ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "model.a"
                            }
                        ]
        , test "should report an error when encountering a reference to class function outside of a function call" <|
            \() ->
                """module A exposing (..)
import Html.Attributes

classListWithoutErrorsBeingReported =
    Html.Attributes.classList
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Class using function is used without arguments"
                            , details = [ "Having the function used without arguments confuses me and will prevent me from figuring out whether the classes passed to this function will be known or unknown. Please pass in all the arguments at the location." ]
                            , under = "Html.Attributes.classList"
                            }
                        ]
        , test "should report an error when encountering a class function application with less arguments than where their class arguments are" <|
            \() ->
                """module A exposing (..)
import Html.Attributes

classFunctionWithoutErrorsBeingReported =
    Html.Attributes.attribute "class"
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Class using function is used without all of its CSS class arguments"
                            , details = [ "Having the function used without all of its arguments confuses me and will prevent me from figuring out whether the classes passed to this function will be known or unknown. Please pass in all the arguments at the location." ]
                            , under = "Html.Attributes.attribute"
                            }
                        ]
        , test "should report an error when being unable to parse a CSS file" <|
            \() ->
                """module A exposing (..)
import Class

view model =
    Class.fromString model.a
"""
                    |> Review.Test.runWithProjectData projectWithUnparsableCssClasses (cssFiles [ FilePattern.include "*.css" ] |> rule)
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
        , test "should report an error when encountering an if expression as an argument to Html.Attributes.class" <|
            \() ->
                """module A exposing (..)
import Html.Attributes as Attr

view model =
    Attr.class <| if model.condition then "a" else "b"
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "if model.condition then \"a\" else \"b\""
                            }
                        ]
        , test "should understand if expressions as an argument to Html.Attributes.class" <|
            \() ->
                """module A exposing (..)
import Html.Attributes as Attr

view model =
    Attr.class <| if model.condition then "known" else nonLiteral
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> withCssUsingFunctions [ ( "Html.Attributes.class", ClassFunction.smartFirstArgumentIsClass ) ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "nonLiteral"
                            }
                        ]
        , test "should understand case expressions as an argument to Html.Attributes.class" <|
            \() ->
                """module A exposing (..)
import Html.Attributes as Attr

view model =
    Attr.class <|
        case model.thing of
            A -> "known"
            B -> nonLiteral
"""
                    |> Review.Test.run (cssFiles [ FilePattern.include "*.css" ] |> addKnownClasses [ "known" ] |> withCssUsingFunctions [ ( "Html.Attributes.class", ClassFunction.smartFirstArgumentIsClass ) ] |> rule)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            , under = "nonLiteral"
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
              )
            ]
        )
        Review.Test.Dependencies.projectWithElmCore


projectWithUnparsableCssClasses : Project
projectWithUnparsableCssClasses =
    Project.addExtraFiles
        (Dict.fromList
            [ ( "some-file.css"
              , """-- First line
.known {
    color: blue;
}
.red-faint {
    color: red;
-- missing closing curly brace
"""
              )
            ]
        )
        Review.Test.Dependencies.projectWithElmCore
