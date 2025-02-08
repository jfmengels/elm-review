module Review.Fix.FixProblem exposing (FixProblem(..))

import Review.Fix.Edit exposing (Edit)


type FixProblem
    = Unchanged
    | SourceCodeIsNotValid { filePath : String, source : String, edits : List Edit }
    | HasCollisionsInEditRanges String Edit Edit
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
