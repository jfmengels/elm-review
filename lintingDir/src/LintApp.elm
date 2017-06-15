port module LintApp exposing (..)

import Json.Decode
import Dict exposing (Dict)
import Lint exposing (lintSource)
import Lint.Types exposing (LintError)
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


lint : String -> List String
lint source =
    case lintSource config source of
        Err errors ->
            errors

        Ok errors ->
            errors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Lint files ->
            let
                lintResult =
                    List.map (\file -> ( file, lint file.source )) files
                        |> List.filter (Tuple.second >> List.isEmpty >> not)
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


formatReport : ( File, List String ) -> String
formatReport ( { filename }, errors ) =
    (toString (List.length errors))
        ++ " errors found in '"
        ++ filename
        ++ "':\n\n\t"
        ++ (String.join "\n\t" errors)
