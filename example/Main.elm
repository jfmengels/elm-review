module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Lint exposing (LintError, Severity(..), lintSource)
import Lint.Rule exposing (Rule)
import Lint.Rule.NoDebug
import Lint.Rule.NoExtraBooleanComparison
import Lint.Rule.NoImportingEverything
import Lint.Rule.NoUnusedTypeConstructors
import Lint.Rule.NoUnusedVariables



-- LINT CONFIGURATION


config : Model -> List ( Severity, Rule )
config model =
    [ ( model.noDebugEnabled, ( Critical, Lint.Rule.NoDebug.rule ) )
    , ( model.noUnusedVariablesEnabled, ( Critical, Lint.Rule.NoUnusedVariables.rule ) )
    , ( model.noImportingEverythingEnabled, ( Critical, Lint.Rule.NoImportingEverything.rule { exceptions = [ "Html" ] } ) )
    , ( model.noExtraBooleanComparisonEnabled, ( Critical, Lint.Rule.NoExtraBooleanComparison.rule ) )
    , ( model.noUnusedTypeConstructorsEnabled, ( Critical, Lint.Rule.NoUnusedTypeConstructors.rule ) )

    -- , ( Critical, Lint.Rule.NoConstantCondition.rule )
    -- , ( Critical, Lint.Rule.NoDuplicateImports.rule )
    -- , ( Critical, Lint.Rule.NoExposingEverything.rule )
    -- , ( Critical, Lint.Rule.NoNestedLet.rule )
    -- , ( Critical, Lint.Rule.NoUnannotatedFunction.rule )
    -- , ( Critical, Lint.Rule.NoUselessIf.rule )
    -- , ( Critical, Lint.Rule.NoUselessPatternMatching.rule )
    -- , ( Warning, Lint.Rule.NoWarningComments.rule )
    -- , ( Critical, Lint.Rule.SimplifyPiping.rule )
    -- , ( Critical, Lint.Rule.SimplifyPropertyAccess.rule )
    -- , ( Critical, Lint.Rule.ElmTest.NoDuplicateTestBodies.rule )
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second



-- MODEL


type alias Model =
    { sourceCode : String
    , lintResult : Result (List String) (List ( Severity, LintError ))
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

        tmpModel : Model
        tmpModel =
            { sourceCode = sourceCode
            , lintResult = Result.Ok []
            , noDebugEnabled = True
            , noUnusedVariablesEnabled = True
            , noImportingEverythingEnabled = True
            , noImportingEverythingExceptions = [ "Html", "Html.Attributes" ]
            , noExtraBooleanComparisonEnabled = True
            , noUnusedTypeConstructorsEnabled = True
            , showConfigurationAsText = False
            }
    in
    { tmpModel | lintResult = lintSource (config tmpModel) sourceCode }



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
            { model
                | sourceCode = sourceCode
                , lintResult = lintSource (config model) sourceCode
            }

        UserToggledNoDebugRule ->
            { model | noDebugEnabled = not model.noDebugEnabled }
                |> rerunLinting

        UserToggledNoUnusedVariablesRule ->
            { model | noUnusedVariablesEnabled = not model.noUnusedVariablesEnabled }
                |> rerunLinting

        UserToggledNoImportingEverythingRule ->
            { model | noImportingEverythingEnabled = not model.noImportingEverythingEnabled }
                |> rerunLinting

        UserToggledNoExtraBooleanComparisonRule ->
            { model | noExtraBooleanComparisonEnabled = not model.noExtraBooleanComparisonEnabled }
                |> rerunLinting

        UserToggledNoUnusedTypeConstructorsRule ->
            { model | noUnusedTypeConstructorsEnabled = not model.noUnusedTypeConstructorsEnabled }
                |> rerunLinting

        UserToggledConfigurationAsText ->
            { model | showConfigurationAsText = not model.showConfigurationAsText }


rerunLinting : Model -> Model
rerunLinting model =
    { model | lintResult = lintSource (config model) model.sourceCode }



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
                [ textarea
                    [ Attr.id "input"
                    , Events.onInput UserEditedSourceCode
                    , Attr.style "height" "500px"
                    , Attr.style "width" "60%"
                    ]
                    [ text model.sourceCode ]
                , div [ Attr.style "margin-left" "2rem" ]
                    [ viewConfigurationPanel model
                    , viewConfigurationAsText model
                    , p [ Attr.class "title" ] [ text "Linting errors" ]
                    , ul [ Attr.id "lint" ]
                        (lintErrors model)
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
                |> List.map (\{ configExpression } -> " ( Critical, " ++ configExpression ++ " )")
                |> String.join "\n    ,"
    in
    """module LintConfig exposing (config)

import Lint exposing (Severity(..))
import Lint.Rule exposing (Rule)
""" ++ importStatements ++ """


config : List ( Severity, Rule )
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


lintErrors : Model -> List (Html Msg)
lintErrors model =
    let
        messages : List String
        messages =
            case model.lintResult of
                Err errors ->
                    errors

                Ok errors ->
                    if List.isEmpty errors then
                        [ "No errors." ]

                    else
                        List.map (Tuple.second >> errorToString) errors
    in
    List.map
        (\message -> li [] [ text message ])
        messages


errorToString : LintError -> String
errorToString { ruleName, message, range } =
    let
        location : String
        location =
            "(line " ++ String.fromInt range.start.row ++ ", column " ++ String.fromInt range.start.column ++ ")"
    in
    ruleName ++ ": " ++ message ++ " " ++ location


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
