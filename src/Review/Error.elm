module Review.Error exposing (Fix(..), InternalError, ReviewError(..), Target(..), error, withFixes)

import Elm.Syntax.Range exposing (Range)


type ReviewError
    = ReviewError InternalError


type Target
    = Module
    | ElmJson
    | Readme
    | Global


type alias InternalError =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    , fixes : Maybe (List Fix)
    , target : Target
    }


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Fix
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String


error : { message : String, details : List String } -> Range -> ReviewError
error { message, details } range =
    ReviewError
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , fixes = Nothing
        , target = Module
        }


withFixes : List Fix -> ReviewError -> ReviewError
withFixes fixes (ReviewError error_) =
    ReviewError
        { error_
            | fixes =
                if List.isEmpty fixes || String.endsWith ".json" error_.filePath then
                    Nothing

                else
                    Just fixes
        }
