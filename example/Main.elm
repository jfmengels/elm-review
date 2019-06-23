module Main exposing (main)

-- Rules

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onInput)
import Lint exposing (Rule, Severity(..), lintSource)
import Lint.Rule.DefaultPatternPosition
import Lint.Rule.NoDebug
import Lint.Rule.NoExtraBooleanComparison
import Lint.Rule.NoImportingEverything
import Lint.Rule.NoUnusedVariables
import Lint.RuleError exposing (RuleError)
import Result exposing (Result)


type Msg
    = UserEditedSourceCode String


config : List ( Severity, Rule )
config =
    [ ( Critical, Lint.Rule.NoDebug.rule )
    , ( Critical, Lint.Rule.NoUnusedVariables.rule )
    , ( Critical, Lint.Rule.NoImportingEverything.rule { exceptions = [ "Html" ] } )
    , ( Critical, Lint.Rule.DefaultPatternPosition.rule { position = Lint.Rule.DefaultPatternPosition.Last } )
    , ( Critical, Lint.Rule.NoExtraBooleanComparison.rule )

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
    , lintResult = lintSource config sourceCode
    }


type alias Model =
    { sourceCode : String
    , lintResult : Result (List String) (List ( Severity, RuleError ))
    }


update : Msg -> Model -> Model
update action model =
    case action of
        UserEditedSourceCode sourceCode ->
            { model
                | sourceCode = sourceCode
                , lintResult = lintSource config sourceCode
            }


errorToString : RuleError -> String
errorToString { rule, message, range } =
    rule ++ ": " ++ message ++ " (line " ++ String.fromInt range.start.row ++ ", column " ++ String.fromInt range.start.column ++ ")"


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


view : Model -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "left" ]
            [ p [ class "title" ] [ text "Source code" ]
            , div
                [ style "display" "flex"
                , style "flex-direction" "row"
                ]
                [ textarea
                    [ id "input"
                    , onInput UserEditedSourceCode
                    , style "height" "500px"
                    , style "width" "60%"
                    ]
                    [ text model.sourceCode ]
                , div [ style "margin-left" "2rem" ]
                    [ p [ class "title" ] [ text "Linting errors" ]
                    , ul [ id "lint" ]
                        (lintErrors model)
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
