port module LintApp exposing (..)

import Json.Decode
import Lint exposing (lintSource)
import Lint.Types exposing (LintRule, LintError, Severity(..))
import LintConfig exposing (config)


type alias File =
    { filename : String
    , source : String
    }


port linting : (List File -> msg) -> Sub msg


port resultPort : String -> Cmd msg


type alias Model =
    {}


type Msg
    = Lint (List File)


enabledRules : List ( Severity, LintRule )
enabledRules =
    config
        |> List.filter (Tuple.first >> (/=) Disabled)


lint : String -> List ( Severity, LintError )
lint source =
    lintSource enabledRules source
        |> (\result ->
                case result of
                    Err errors ->
                        [ ( Critical, { rule = "ParseError", message = String.join "\n" errors } )
                        ]

                    Ok result ->
                        result
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Lint files ->
            let
                lintResult =
                    files
                        |> List.map (\file -> ( file, lint file.source ))
                        |> List.filter
                            (Tuple.second >> List.isEmpty >> not)
                        |> List.map formatReport
                        |> String.join "\n\n"
            in
                ( model
                , resultPort lintResult
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    linting Lint


main : Program Never Model Msg
main =
    Platform.program
        { init = ( Model, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


formatSeverity : Severity -> String
formatSeverity severity =
    case severity of
        Disabled ->
            "Disabled"

        Warning ->
            "Warning"

        Critical ->
            "Critical"


formatReport : ( File, List ( Severity, LintError ) ) -> String
formatReport ( { filename }, errors ) =
    let
        formattedErrors =
            List.map
                (\( severity, { rule, message } ) ->
                    "(" ++ (formatSeverity severity) ++ ") " ++ rule ++ ": " ++ message
                )
                errors
    in
        (toString (List.length errors))
            ++ " errors found in '"
            ++ filename
            ++ "':\n\n\t"
            ++ (String.join "\n\t" formattedErrors)
