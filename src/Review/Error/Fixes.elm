module Review.Error.Fixes exposing
    ( ErrorFixes(..)
    , fixesFromMaybe
    )

import Dict exposing (Dict)
import Review.Error.Target exposing (Target(..))
import Review.Fix exposing (Fix)
import Review.Fix.FixProblem exposing (FixProblem)


type ErrorFixes
    = NoFixes
    | Available (Dict String ( Target, List Fix ))
    | FailedToApply FixProblem


fixesFromMaybe : String -> Maybe (List Fix) -> ErrorFixes
fixesFromMaybe elmJsonPath maybeFixes =
    case maybeFixes of
        Just fixes ->
            Available (Dict.singleton elmJsonPath ( ElmJson, fixes ))

        Nothing ->
            NoFixes
