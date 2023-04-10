module Review.Fix.FixProblem exposing (FixProblem(..))


type FixProblem
    = Unchanged
    | SourceCodeIsNotValid String
    | HasCollisionsInFixRanges
