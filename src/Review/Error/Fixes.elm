module Review.Error.Fixes exposing (ErrorFixes(..), FileFix)

import Dict exposing (Dict)
import Review.Error.Target exposing (Target(..))
import Review.Fix exposing (Fix)
import Review.Fix.FixProblem exposing (FixProblem)


type ErrorFixes
    = NoFixes
    | Available (Dict String FileFix)
    | FailedToApply FixProblem


type alias FileFix =
    ( Target, List Fix )
