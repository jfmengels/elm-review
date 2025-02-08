module Review.Fix.FixProblem exposing (FixProblem(..))

import Review.Fix.Edit exposing (Edit)


type
    FixProblem
    -- TODO Breaking change: Merge with Fix.Problem
    = Unchanged
    | SourceCodeIsNotValid { filePath : String, source : String, edits : List Edit }
    | HasCollisionsInEditRanges String Edit Edit
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
