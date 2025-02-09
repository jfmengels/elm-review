module Review.Fix.FixProblem exposing (FixProblem(..))

import Json.Decode
import Review.Fix.Edit exposing (Edit)


type FixProblem
    = Unchanged
    | InvalidElmFile { filePath : String, source : String, edits : List Edit }
    | InvalidJson { filePath : String, source : String, edits : List Edit, decodingError : Json.Decode.Error }
    | EditWithNegativeRange { filePath : String, edit : Edit }
    | HasCollisionsInEditRanges { filePath : String, edits : List Edit }
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
