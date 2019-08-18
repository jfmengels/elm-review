module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, p, text, textarea)
import Html.Attributes as Attr
import Html.Events as Events
import Lint exposing (LintError, lintSource)
import Lint.Rule exposing (Rule)
import Lint.Rule.NoDebug
import Lint.Rule.NoExtraBooleanComparison
import Lint.Rule.NoImportingEverything
import Lint.Rule.NoUnusedTypeConstructors
import Lint.Rule.NoUnusedVariables
import Reporter



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { sourceCode : String
    , lintErrors : List LintError
    , noDebugEnabled : Bool
    , noUnusedVariablesEnabled : Bool
    , noImportingEverythingEnabled : Bool
    , noImportingEverythingExceptions : List String
    , noExtraBooleanComparisonEnabled : Bool
    , noUnusedTypeConstructorsEnabled : Bool
    , showConfigurationAsText : Bool
    }


init : Model
init =
    let
        sourceCode : String
        sourceCode =
            """module Main exposing (f)

import Html.Events exposing (..)
import Html exposing (..)
import NotUsed
import SomeModule exposing (notUsed)

f : Int -> Int
f x = x Debug.log 1

g n = n + 1
"""
    in
    { sourceCode = sourceCode
    , lintErrors = []
    , noDebugEnabled = True
    , noUnusedVariablesEnabled = True
    , noImportingEverythingEnabled = True
    , noImportingEverythingExceptions = [ "Html", "Html.Attributes" ]
    , noExtraBooleanComparisonEnabled = True
    , noUnusedTypeConstructorsEnabled = True
    , showConfigurationAsText = False
    }
        |> runLinting



-- LINT CONFIGURATION


config : Model -> List Rule
config model =
    [ ( model.noDebugEnabled, Lint.Rule.NoDebug.rule )
    , ( model.noUnusedVariablesEnabled, Lint.Rule.NoUnusedVariables.rule )
    , ( model.noImportingEverythingEnabled, Lint.Rule.NoImportingEverything.rule { exceptions = [ "Html" ] } )
    , ( model.noExtraBooleanComparisonEnabled, Lint.Rule.NoExtraBooleanComparison.rule )
    , ( model.noUnusedTypeConstructorsEnabled, Lint.Rule.NoUnusedTypeConstructors.rule )

    -- , Lint.Rule.NoConstantCondition.rule
    -- , Lint.Rule.NoDuplicateImports.rule
    -- , Lint.Rule.NoExposingEverything.rule
    -- , Lint.Rule.NoNestedLet.rule
    -- , Lint.Rule.NoUnannotatedFunction.rule
    -- , Lint.Rule.NoUselessIf.rule
    -- , Lint.Rule.NoUselessPatternMatching.rule
    -- , Lint.Rule.NoWarningComments.rule
    -- , Lint.Rule.SimplifyPiping.rule
    -- , Lint.Rule.SimplifyPropertyAccess.rule
    -- , Lint.Rule.ElmTest.NoDuplicateTestBodies.rule
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second



-- UPDATE


type Msg
    = UserEditedSourceCode String
    | UserToggledNoDebugRule
    | UserToggledNoUnusedVariablesRule
    | UserToggledNoImportingEverythingRule
    | UserToggledNoExtraBooleanComparisonRule
    | UserToggledNoUnusedTypeConstructorsRule
    | UserToggledConfigurationAsText


update : Msg -> Model -> Model
update action model =
    case action of
        UserEditedSourceCode sourceCode ->
            { model | sourceCode = sourceCode }
                |> runLinting

        UserToggledNoDebugRule ->
            { model | noDebugEnabled = not model.noDebugEnabled }
                |> runLinting

        UserToggledNoUnusedVariablesRule ->
            { model | noUnusedVariablesEnabled = not model.noUnusedVariablesEnabled }
                |> runLinting

        UserToggledNoImportingEverythingRule ->
            { model | noImportingEverythingEnabled = not model.noImportingEverythingEnabled }
                |> runLinting

        UserToggledNoExtraBooleanComparisonRule ->
            { model | noExtraBooleanComparisonEnabled = not model.noExtraBooleanComparisonEnabled }
                |> runLinting

        UserToggledNoUnusedTypeConstructorsRule ->
            { model | noUnusedTypeConstructorsEnabled = not model.noUnusedTypeConstructorsEnabled }
                |> runLinting

        UserToggledConfigurationAsText ->
            { model | showConfigurationAsText = not model.showConfigurationAsText }


runLinting : Model -> Model
runLinting model =
    { model | lintErrors = lintSource (config model) (file model.sourceCode) }



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attr.id "wrapper" ]
        [ div [ Attr.id "left" ]
            [ p [ Attr.class "title" ] [ text "Source code" ]
            , div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "row"
                ]
                [ div
                    [ Attr.style "width" "60%"
                    ]
                    [ textarea
                        [ Attr.id "input"
                        , Events.onInput UserEditedSourceCode
                        , Attr.style "width" "100%"
                        , Attr.style "height" "500px"
                        ]
                        [ text model.sourceCode ]
                    , div
                        [ Attr.style "border-radius" "4px"
                        , Attr.style "padding" "12px"
                        , Attr.style "max-width" "100%"
                        , Attr.style "width" "calc(100vw - 24px)"
                        , Attr.style "overflow-x" "auto"
                        , Attr.style "white-space" "pre"
                        , Attr.style "color" "white"
                        , Attr.style "font-family" "'Source Code Pro', monospace"
                        , Attr.style "font-size" "12px"
                        , Attr.style "background-color" "black"
                        ]
                        [ viewLintErrors model
                        ]
                    ]
                , div
                    [ Attr.style "margin-left" "2rem"
                    , Attr.style "width" "40%"
                    ]
                    [ viewConfigurationPanel model
                    , viewConfigurationAsText model
                    , p [ Attr.class "title" ] [ text "Linting errors" ]
                    ]
                ]
            ]
        ]


viewConfigurationPanel : Model -> Html Msg
viewConfigurationPanel model =
    div []
        [ p [ Attr.class "title" ] [ text "Configuration" ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            ]
            [ viewCheckbox UserToggledNoDebugRule "NoDebug" model.noDebugEnabled
            , viewCheckbox UserToggledNoUnusedVariablesRule "NoUnusedVariables" model.noUnusedVariablesEnabled
            , viewCheckbox UserToggledNoImportingEverythingRule "NoImportingEverything" model.noImportingEverythingEnabled
            , viewCheckbox UserToggledNoExtraBooleanComparisonRule "NoExtraBooleanComparison" model.noExtraBooleanComparisonEnabled
            , viewCheckbox UserToggledNoUnusedTypeConstructorsRule "NoUnusedTypeConstructors" model.noUnusedTypeConstructorsEnabled
            ]
        ]


viewConfigurationAsText : Model -> Html Msg
viewConfigurationAsText model =
    if model.showConfigurationAsText then
        div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "width" "100%"
            ]
            [ button
                [ Attr.style "margin-top" "2rem"
                , Events.onClick UserToggledConfigurationAsText
                ]
                [ text "Hide configuration as Elm code" ]
            , textarea
                [ Events.onInput UserEditedSourceCode
                , Attr.style "height" "300px"
                , Attr.style "width" "100%"
                ]
                [ text <| configurationAsText model ]
            ]

    else
        button
            [ Attr.style "margin-top" "2rem"
            , Events.onClick UserToggledConfigurationAsText
            ]
            [ text "Show configuration as Elm code" ]


configurationAsText : Model -> String
configurationAsText model =
    let
        rules : List { import_ : String, configExpression : String }
        rules =
            [ ( model.noDebugEnabled
              , { import_ = "Lint.Rule.NoDebug"
                , configExpression = "Lint.Rule.NoDebug.rule"
                }
              )
            , ( model.noUnusedVariablesEnabled
              , { import_ = "Lint.Rule.NoUnusedVariables"
                , configExpression = "Lint.Rule.NoUnusedVariables.rule"
                }
              )
            , ( model.noImportingEverythingEnabled
              , { import_ = "Lint.Rule.NoImportingEverything"
                , configExpression = "Lint.Rule.NoImportingEverything.rule { exceptions = [] }"
                }
              )
            , ( model.noExtraBooleanComparisonEnabled
              , { import_ = "Lint.Rule.NoExtraBooleanComparison"
                , configExpression = "Lint.Rule.NoExtraBooleanComparison.rule"
                }
              )
            , ( model.noUnusedTypeConstructorsEnabled
              , { import_ = "Lint.Rule.NoUnusedTypeConstructors"
                , configExpression = "Lint.Rule.NoUnusedTypeConstructors.rule"
                }
              )
            ]
                |> List.filter Tuple.first
                |> List.map Tuple.second

        importStatements : String
        importStatements =
            rules
                |> List.map (\{ import_ } -> "import " ++ import_)
                |> String.join "\n"

        configExpressions : String
        configExpressions =
            rules
                |> List.map (\{ configExpression } -> " " ++ configExpression)
                |> String.join "\n    ,"
    in
    """module LintConfig exposing (config)

import Lint.Rule exposing (Rule)
""" ++ importStatements ++ """


config : List Rule
config =
    [""" ++ configExpressions ++ """
    ]
"""


viewCheckbox : Msg -> String -> Bool -> Html Msg
viewCheckbox onClick name checked =
    label
        []
        [ input
            [ Attr.type_ "checkbox"
            , Attr.checked checked
            , Events.onClick onClick
            ]
            []
        , text name
        ]


viewLintErrors : Model -> Html msg
viewLintErrors model =
    lintErrors model
        |> List.map viewPart
        |> Html.div []


viewPart : { str : String, color : Maybe ( Int, Int, Int ) } -> Html msg
viewPart { str, color } =
    Html.span
        [ case color of
            Just ( red, green, blue ) ->
                Attr.style "color" <| "rgb(" ++ String.fromInt red ++ "," ++ String.fromInt green ++ "," ++ String.fromInt blue ++ ")"

            Nothing ->
                Attr.classList []
        ]
        (str
            |> String.lines
            |> List.map Html.text
            |> List.intersperse (Html.br [] [])
        )


lintErrors : Model -> List { str : String, color : Maybe ( Int, Int, Int ) }
lintErrors model =
    if List.isEmpty model.lintErrors then
        [ { str = "I found no linting errors.\nYou're all good!"
          , color = Nothing
          }
        ]

    else
        [ ( file model.sourceCode
          , model.lintErrors
                |> List.map fromLintError
          )
        ]
            |> Reporter.formatReport False


fromLintError : LintError -> Reporter.Error
fromLintError error =
    { moduleName = Lint.errorModuleName error
    , ruleName = Lint.errorRuleName error
    , message = Lint.errorMessage error
    , details = Lint.errorDetails error
    , range = Lint.errorRange error
    , hasFix = Lint.errorFixes error /= Nothing
    }


file : String -> { path : String, source : String }
file source =
    { path = "SOURCE CODE", source = source }
