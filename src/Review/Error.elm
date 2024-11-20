module Review.Error exposing (ErrorFixes(..), InternalError, ReviewError(..), Target(..), doesPreventExtract, error, fixesFromMaybe, markFixesAsProblem, preventExtract, setCurrentFilePathOnTargetIfNeeded)

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


setCurrentFilePathOnTargetIfNeeded : String -> Target -> Target
setCurrentFilePathOnTargetIfNeeded filePath target =
    case target of
        Module "" ->
            Module filePath

        ExtraFile _ ->
            target

        Module _ ->
            target

        ElmJson ->
            target

        Readme ->
            target

        Global ->
            target

        UserGlobal ->
            target


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
