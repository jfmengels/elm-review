module Review.Options.Internal exposing (FixMode(..), ReviewOptionsData, ReviewOptionsInternal(..), shouldAbort, shouldApplyFix)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Review.Fix.FixedErrors as FixedErrors exposing (FixedErrors)
import Review.Logger exposing (Logger)


type ReviewOptionsInternal
    = ReviewOptionsInternal ReviewOptionsData


type alias ReviewOptionsData =
    { extract : Bool
    , logger : Logger
    , fixMode : FixMode
    , suppressions : Dict ( String, String ) Int
    , ignoreFix : { ruleName : String, filePath : String, message : String, details : List String, range : Range } -> Bool
    }


type FixMode
    = Disabled
    | Enabled (Maybe Int)


shouldApplyFix : String -> ReviewOptionsData -> Maybe ({ ruleName : String, filePath : String, message : String, details : List String, range : Range } -> Bool)
shouldApplyFix ruleName reviewOptionsData =
    case reviewOptionsData.fixMode of
        Enabled _ ->
            if Dict.isEmpty reviewOptionsData.suppressions then
                Just (\err -> not (reviewOptionsData.ignoreFix err))

            else
                Just (\err -> not (Dict.member ( ruleName, err.filePath ) reviewOptionsData.suppressions) && not (reviewOptionsData.ignoreFix err))

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
