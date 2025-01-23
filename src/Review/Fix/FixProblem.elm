module Review.Fix.FixProblem exposing (FixProblem(..))


type
    FixProblem
    -- TODO Breaking change: Merge with Fix.Problem
    = Unchanged
    | SourceCodeIsNotValid String
    | HasCollisionsInFixRanges
    | CreatesImportCycle (List String)
