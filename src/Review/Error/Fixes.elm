module Review.Error.Fixes exposing (ErrorFixes(..))

import Dict exposing (Dict)
import Review.Error.Target exposing (Target(..))
import Review.Fix exposing (Fix)
import Review.Fix.FixProblem exposing (FixProblem)


type ErrorFixes
    = NoFixes
    | Available (Dict String ( Target, List Fix ))
    | FailedToApply FixProblem
