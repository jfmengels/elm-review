module Review.Fix.FixProblem exposing (FixProblem(..))

import Review.Fix.Edit exposing (Edit)


type FixProblem
    = Unchanged
    | InvalidElmFile { filePath : String, source : String, edits : List Edit }
    | EditWithNegativeRange { filePath : String, edit : Edit }
    | HasCollisionsInEditRanges { filePath : String, edits : List Edit }
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
