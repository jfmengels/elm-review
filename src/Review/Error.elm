module Review.Error exposing (ErrorFixes(..), FixProblem(..), InternalError, ReviewError(..), Target(..), doesPreventExtract, error, fixesFromMaybe, preventExtract, withFixes)

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
    , fixes : ErrorFixes
    , target : Target
    , preventsExtract : Bool
    }


type ErrorFixes
    = NoFixes
    | Available (List Fix)
    | FailedToApply (List Fix) FixProblem


type FixProblem
    = Unchanged
    | SourceCodeIsNotValid String
    | HasCollisionsInFixRanges


fixesFromMaybe : Maybe (List Fix) -> ErrorFixes
fixesFromMaybe maybeFixes =
    case maybeFixes of
        Just fixes ->
            Available fixes

        Nothing ->
            NoFixes


error : { message : String, details : List String } -> Range -> ReviewError
error { message, details } range =
    ReviewError
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , fixes = NoFixes
        , target = Module
        , preventsExtract = False
        }


withFixes : List Fix -> ReviewError -> ReviewError
withFixes fixes (ReviewError error_) =
    ReviewError
        { error_
            | fixes =
                if List.isEmpty fixes || String.endsWith ".json" error_.filePath then
                    NoFixes

                else
                    Available fixes
        }


preventExtract : InternalError -> InternalError
preventExtract error_ =
    { error_ | preventsExtract = True }


doesPreventExtract : InternalError -> Bool
doesPreventExtract error_ =
    error_.preventsExtract
