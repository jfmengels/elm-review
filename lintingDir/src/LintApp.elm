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


port resultPort : List ( File, List String ) -> Cmd msg


type alias Model =
    Dict String (List LintError)


type Msg
    = Lint (List File)


lint : String -> List String
lint source =
    case lintSource config source of
        Err errors ->
            errors

        Ok errors ->
            errors


init : ( Model, Cmd Msg )
init =
    ( Dict.empty, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Lint files ->
            ( model
            , resultPort (List.map (\file -> ( file, lint file.source )) files)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    linting Lint


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
