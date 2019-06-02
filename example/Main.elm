module Main exposing (main)

-- Rules

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onInput)
import Lint exposing (Rule, Severity(..), lintSource)
import Lint.Error exposing (Error)
import Lint.Rule.NoDebug
import Lint.Rule.NoImportingEverything
import Lint.Rule.NoUnusedVariables
import Result exposing (Result)


type Msg
    = Replace String


config : List ( Severity, Rule )
config =
    [ ( Critical, Lint.Rule.NoDebug.rule )
    , ( Critical, Lint.Rule.NoUnusedVariables.rule )
    , ( Critical, Lint.Rule.NoImportingEverything.rule { exceptions = [ "Html" ] } )

    -- , ( Critical, Lint.Rule.DefaultPatternPosition.rule { position = Lint.Rule.DefaultPatternPosition.Last } )
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


init : String
init =
    """module Main exposing (f)

import Html.Events exposing (..)
import Html exposing (..)

f : Int -> Int
f x = x Debug.log 1

g n = n + 1
"""


update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m


errorToString : Error -> String
errorToString { message, range } =
    message ++ " (line " ++ String.fromInt range.start.row ++ ", column " ++ String.fromInt range.start.column ++ ")"


lint : String -> Html Msg
lint source =
    let
        lintResult : Result (List String) (List ( Severity, Error ))
        lintResult =
            lintSource config source

        messages : List String
        messages =
            case lintResult of
                Err errors ->
                    errors

                Ok errors ->
                    if List.isEmpty errors then
                        [ "No errors." ]

                    else
                        List.map (Tuple.second >> errorToString) errors
    in
    div []
        (List.map
            (\message -> p [] [ text message ])
            messages
        )


view : String -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "left" ]
            [ p [ class "title" ] [ text "Source code" ]
            , textarea
                [ id "input"
                , onInput Replace
                , style "height" "500px"
                , style "width" "500px"
                ]
                [ text model ]
            , div []
                [ p [ class "title" ] [ text "Linting errors" ]
                , ul [ id "lint" ]
                    [ li [] [ lint model ]
                    ]
                ]
            ]
        ]


main : Program () String Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
