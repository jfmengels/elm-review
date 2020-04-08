module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, p, text, textarea)
import Html.Attributes as Attr
import Html.Events as Events
import NoDebug.Log
import NoDebug.TodoOrToString
import NoUnused.CustomTypeConstructors
import NoUnused.Variables
import Reporter
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Rule)



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
    , project : Project
    , reviewErrors : List Rule.ReviewError
    , noDebugEnabled : Bool
    , noUnusedVariablesEnabled : Bool
    , noUnusedTypeConstructorsEnabled : Bool
    , showConfigurationAsText : Bool
    }


init : Model
init =
    let
        sourceCode : String
        sourceCode =
            """module Main exposing (f)

import NotUsed
import SomeModule exposing (notUsed)

type SomeCustomType
  = UsedConstructor
  | NotUsedConstructor

f : Int -> SomeCustomType
f x =
  let
    _ = Debug.log "x" x
  in
  UsedConstructor

g n = n + 1
"""
    in
    { sourceCode = sourceCode
    , project = Project.addModule (file sourceCode) Project.new
    , reviewErrors = []
    , noDebugEnabled = True
    , noUnusedVariablesEnabled = True
    , noUnusedTypeConstructorsEnabled = True
    , showConfigurationAsText = False
    }
        |> runReview



-- REVIEW CONFIGURATION


config : Model -> List Rule
config model =
    [ ( model.noDebugEnabled, NoDebug.Log.rule )
    , ( model.noDebugEnabled, NoDebug.TodoOrToString.rule )
    , ( model.noUnusedVariablesEnabled, NoUnused.Variables.rule )
    , ( model.noUnusedTypeConstructorsEnabled, NoUnused.CustomTypeConstructors.rule [] )
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second



-- UPDATE


type Msg
    = UserEditedSourceCode String
    | UserToggledNoDebugRule
    | UserToggledNoUnusedVariablesRule
    | UserToggledNoUnusedTypeConstructorsRule
    | UserToggledConfigurationAsText


update : Msg -> Model -> Model
update action model =
    case action of
        UserEditedSourceCode sourceCode ->
            { model
                | sourceCode = sourceCode
                , project = Project.addModule (file sourceCode) model.project
            }
                |> runReview

        UserToggledNoDebugRule ->
            { model | noDebugEnabled = not model.noDebugEnabled }
                |> runReview

        UserToggledNoUnusedVariablesRule ->
            { model | noUnusedVariablesEnabled = not model.noUnusedVariablesEnabled }
                |> runReview

        UserToggledNoUnusedTypeConstructorsRule ->
            { model | noUnusedTypeConstructorsEnabled = not model.noUnusedTypeConstructorsEnabled }
                |> runReview

        UserToggledConfigurationAsText ->
            { model | showConfigurationAsText = not model.showConfigurationAsText }


runReview : Model -> Model
runReview model =
    { model
        | reviewErrors =
            Rule.review (config model) model.project
                |> Tuple.first
    }



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
                        [ viewReviewErrors model
                        ]
                    ]
                , div
                    [ Attr.style "margin-left" "2rem"
                    , Attr.style "width" "40%"
                    ]
                    [ viewConfigurationPanel model
                    , viewConfigurationAsText model
                    , p [ Attr.class "title" ] [ text "Review errors" ]
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
            [ viewCheckbox UserToggledNoDebugRule "NoDebug rules" model.noDebugEnabled
            , viewCheckbox UserToggledNoUnusedVariablesRule "NoUnused.Variables" model.noUnusedVariablesEnabled
            , viewCheckbox UserToggledNoUnusedTypeConstructorsRule "NoUnused.CustomTypeConstructors" model.noUnusedTypeConstructorsEnabled
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
              , { import_ = "NoDebug.Log"
                , configExpression = "NoDebug.Log.rule"
                }
              )
            , ( model.noDebugEnabled
              , { import_ = "NoDebug.TodoOrToString"
                , configExpression = "NoDebug.TodoOrToString.rule"
                }
              )
            , ( model.noUnusedVariablesEnabled
              , { import_ = "NoUnused.Variables"
                , configExpression = "NoUnused.Variables.rule"
                }
              )
            , ( model.noUnusedTypeConstructorsEnabled
              , { import_ = "NoUnused.CustomTypeConstructors"
                , configExpression = "NoUnused.CustomTypeConstructors.rule []"
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
    """module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
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


viewReviewErrors : Model -> Html msg
viewReviewErrors model =
    reviewErrors model
        |> List.map viewPart
        |> Html.div []


viewPart : Reporter.TextContent -> Html msg
viewPart { str, color, backgroundColor } =
    Html.span
        [ case color of
            Just ( red, green, blue ) ->
                Attr.style "color" <| "rgb(" ++ String.fromInt red ++ "," ++ String.fromInt green ++ "," ++ String.fromInt blue ++ ")"

            Nothing ->
                Attr.classList []
        , case backgroundColor of
            Just ( red, green, blue ) ->
                Attr.style "background-color" <| "rgb(" ++ String.fromInt red ++ "," ++ String.fromInt green ++ "," ++ String.fromInt blue ++ ")"

            Nothing ->
                Attr.classList []
        ]
        (str
            |> String.lines
            |> List.map Html.text
            |> List.intersperse (Html.br [] [])
        )


reviewErrors : Model -> List Reporter.TextContent
reviewErrors model =
    Reporter.formatReport Reporter.Reviewing
        [ ( file model.sourceCode
          , model.reviewErrors
                |> List.map fromReviewError
          )
        ]


fromReviewError : Rule.ReviewError -> Reporter.Error
fromReviewError error =
    { ruleName = Rule.errorRuleName error
    , message = Rule.errorMessage error
    , details = Rule.errorDetails error
    , range = Rule.errorRange error
    , hasFix = Rule.errorFixes error /= Nothing
    }


file : String -> { path : String, source : String }
file source =
    { path = "SOURCE CODE", source = source }
