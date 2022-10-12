module Review.Error exposing (InternalError, ReviewError(..), Target(..), doesPreventExtract, error, preventExtract, withFixes)

import Elm.Syntax.Range exposing (Range)
import Review.Fix.Internal exposing (Fix)


type ReviewError
    = ReviewError InternalError


type Target
    = Module
    | ElmJson
    | Readme
    | Global
    | UserGlobal


type alias InternalError =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    , fixes : Maybe (List Fix)
    , target : Target
    , preventsExtract : Bool
    }


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
        , preventsExtract = False
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


preventExtract : InternalError -> InternalError
preventExtract error_ =
    { error_ | preventsExtract = True }


doesPreventExtract : InternalError -> Bool
doesPreventExtract error_ =
    error_.preventsExtract
