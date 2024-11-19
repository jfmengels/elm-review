module Review.Error exposing (ErrorFixes(..), InternalError, ReviewError(..), Target(..), doesPreventExtract, error, fixesFromMaybe, markFixesAsProblem, preventExtract, withFixes)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Review.Fix.FixProblem as FixProblem
import Review.Fix.Internal exposing (Fix)


type ReviewError
    = ReviewError InternalError


type Target
    = Module String
    | ElmJson
    | Readme
    | ExtraFile String
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
    | Available (Dict String (List Fix))
    | FailedToApply FixProblem.FixProblem


fixesFromMaybe : String -> Maybe (List Fix) -> ErrorFixes
fixesFromMaybe elmJsonPath maybeFixes =
    case maybeFixes of
        Just fixes ->
            Available (Dict.singleton elmJsonPath fixes)

        Nothing ->
            NoFixes


markFixesAsProblem : FixProblem.FixProblem -> InternalError -> InternalError
markFixesAsProblem fixProblem error_ =
    { error_ | fixes = FailedToApply fixProblem }


error : { message : String, details : List String } -> Range -> ReviewError
error { message, details } range =
    ReviewError
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , fixes = NoFixes
        , target = Module ""
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
                    Available (Dict.singleton error_.filePath fixes)
        }


preventExtract : InternalError -> InternalError
preventExtract error_ =
    { error_ | preventsExtract = True }


doesPreventExtract : InternalError -> Bool
doesPreventExtract error_ =
    error_.preventsExtract
