module Review.Error exposing (ErrorFixes(..), InternalError, ReviewError(..), doesPreventExtract, error, fixesFromMaybe, markFixesAsProblem, preventExtract)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Review.Error.Target exposing (Target(..))
import Review.Fix.FixProblem as FixProblem
import Review.Fix.Internal exposing (Fix)


type ReviewError
    = ReviewError InternalError


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
    | Available (Dict String ( Target, List Fix ))
    | FailedToApply FixProblem.FixProblem


fixesFromMaybe : String -> Maybe (List Fix) -> ErrorFixes
fixesFromMaybe elmJsonPath maybeFixes =
    case maybeFixes of
        Just fixes ->
            Available (Dict.singleton elmJsonPath ( ElmJson, fixes ))

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


preventExtract : InternalError -> InternalError
preventExtract error_ =
    { error_ | preventsExtract = True }


doesPreventExtract : InternalError -> Bool
doesPreventExtract error_ =
    error_.preventsExtract
