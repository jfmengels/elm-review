module Review.Options.Internal exposing (FixMode(..), ReviewOptionsData, ReviewOptionsInternal(..), shouldAbort, shouldFindFix)

import Dict exposing (Dict)
import Review.Fix.FixedErrors as FixedErrors exposing (FixedErrors)
import Review.Logger exposing (Logger)


type ReviewOptionsInternal
    = ReviewOptionsInternal ReviewOptionsData


type alias ReviewOptionsData =
    { extract : Bool
    , logger : Logger
    , fixMode : FixMode
    , suppressions : Dict ( String, String ) Int
    }


type FixMode
    = Disabled
    | Enabled (Maybe Int)


shouldFindFix : String -> ReviewOptionsData -> Maybe (String -> Bool)
shouldFindFix ruleName reviewOptionsData =
    case reviewOptionsData.fixMode of
        Enabled _ ->
            if Dict.isEmpty reviewOptionsData.suppressions then
                Just (always True)

            else
                Just (\filePath -> not (Dict.member ( ruleName, filePath ) reviewOptionsData.suppressions))

        Disabled ->
            Nothing


shouldAbort : ReviewOptionsData -> FixedErrors -> Bool
shouldAbort reviewOptionsData fixedErrors =
    case reviewOptionsData.fixMode of
        Enabled (Just fixLimit) ->
            fixLimit <= FixedErrors.count fixedErrors

        Enabled Nothing ->
            False

        Disabled ->
            False
