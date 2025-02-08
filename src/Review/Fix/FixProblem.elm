module Review.Fix.FixProblem exposing (FixProblem(..))

import Review.Fix.Edit exposing (Edit)


type FixProblem
    = Unchanged
    | SourceCodeIsNotValid { filePath : String, source : String, edits : List Edit }
    | HasCollisionsInEditRanges { filePath : String, edits : List Edit }
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
