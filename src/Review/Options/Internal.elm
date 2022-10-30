module Review.Options.Internal exposing (Fixes(..), ReviewOptionsData, ReviewOptionsInternal(..), shouldFindFix)

import Dict exposing (Dict)
import Review.Logger exposing (Logger)


type ReviewOptionsInternal
    = ReviewOptionsInternal ReviewOptionsData


type alias ReviewOptionsData =
    { extract : Bool
    , logger : Logger
    , fixes : Fixes
    , fixLimit : Maybe Int
    , suppressions : Dict ( String, String ) Int
    }


type Fixes
    = Disabled
    | Enabled (Maybe Int)


shouldFindFix : String -> ReviewOptionsData -> Maybe (String -> Bool)
shouldFindFix ruleName reviewOptionsData =
    case reviewOptionsData.fixes of
        Enabled _ ->
            if Dict.isEmpty reviewOptionsData.suppressions then
                Just (always True)

            else
                Just (\filePath -> not (Dict.member ( ruleName, filePath ) reviewOptionsData.suppressions))

        Disabled ->
            Nothing
