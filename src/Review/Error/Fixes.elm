module Review.Error.Fixes exposing (ErrorFixes(..), FileFix, from)

import Dict exposing (Dict)
import Review.Error.Target as Target exposing (Target(..))
import Review.Fix exposing (Fix)
import Review.Fix.FixProblem exposing (FixProblem)


type ErrorFixes
    = NoFixes
    | Available (Dict String FileFix)
    | FailedToApply FixProblem


type alias FileFix =
    ( Target, List Fix )


from : Target -> List Fix -> ErrorFixes
from target edits =
    case Target.filePath target of
        Just filePath ->
            Dict.singleton filePath ( target, edits )
                |> Available

        Nothing ->
            NoFixes
