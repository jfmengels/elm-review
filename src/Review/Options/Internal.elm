module Review.Options.Internal exposing (ReviewOptionsData, ReviewOptionsInternal(..), shouldFindFix)

import Dict exposing (Dict)
import Review.Logger exposing (Logger)


type ReviewOptionsInternal
    = ReviewOptionsInternal ReviewOptionsData


type alias ReviewOptionsData =
    { extract : Bool
    , logger : Logger
    , fixAll : Bool
    , fixLimit : Maybe Int
    , suppressions : Dict ( String, String ) Int
    }


shouldFindFix : String -> ReviewOptionsData -> Maybe (String -> Bool)
shouldFindFix ruleName reviewOptionsData =
    if reviewOptionsData.fixAll then
        if Dict.isEmpty reviewOptionsData.suppressions then
            Just (always True)

        else
            Just (\filePath -> not (Dict.member ( ruleName, filePath ) reviewOptionsData.suppressions))

    else
        Nothing
