module Review.Fix.FixProblem exposing (FixProblem(..))

import Elm.Syntax.Range exposing (Range)


type
    FixProblem
    -- TODO Breaking change: Merge with Fix.Problem
    = Unchanged
    | SourceCodeIsNotValid String
    | HasCollisionsInEditRanges String { range : Range, replacement : String } { range : Range, replacement : String }
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
