module Main exposing (main)

-- Rules

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onInput)
import Lint exposing (Rule, Severity(..), lintSource)
import Lint.Error exposing (Error)
import Lint.Rules.NoDebug
import Result exposing (Result)


type Msg
    = Replace String


config : List ( Severity, Rule )
config =
    [ ( Critical, Lint.Rules.NoDebug.rule )

    -- , ( Critical, Lint.Rules.DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.Last } )
    -- , ( Critical, Lint.Rules.NoConstantCondition.rule )
    -- , ( Critical, Lint.Rules.NoDuplicateImports.rule )
    -- , ( Critical, Lint.Rules.NoExposingEverything.rule )
    -- , ( Critical, Lint.Rules.NoImportingEverything.rule { exceptions = [ "Html" ] } )
    -- , ( Critical, Lint.Rules.NoNestedLet.rule )
    -- , ( Critical, Lint.Rules.NoUnannotatedFunction.rule )
    -- , ( Critical, Lint.Rules.NoUnusedVariables.rule )
    -- , ( Critical, Lint.Rules.NoUselessIf.rule )
    -- , ( Critical, Lint.Rules.NoUselessPatternMatching.rule )
    -- , ( Warning, Lint.Rules.NoWarningComments.rule )
    -- , ( Critical, Lint.Rules.SimplifyPiping.rule )
    -- , ( Critical, Lint.Rules.SimplifyPropertyAccess.rule )
    -- , ( Critical, Lint.Rules.ElmTest.NoDuplicateTestBodies.rule )
    ]


init : String
init =
    """module Main exposing (f)

import Html
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
