module Review.Fix.FixProblem exposing (FixProblem(..))

import Json.Decode
import Parser
import Review.Fix.Edit exposing (Edit)


type FixProblem
    = Unchanged
    | InvalidElm { filePath : String, source : String, edits : List Edit, parsingErrors : List Parser.DeadEnd }
    | InvalidJson { filePath : String, source : String, edits : List Edit, decodingError : Json.Decode.Error }
    | EditWithNegativeRange { filePath : String, edit : Edit }
    | HasCollisionsInEditRanges { filePath : String, edits : List Edit }
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
