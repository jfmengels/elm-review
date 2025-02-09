module Review.Fix.FixProblem exposing (FixProblem(..))

{-|

@docs FixProblem

-}

import Json.Decode
import Parser
import Review.Fix.Edit exposing (Edit)


{-| Represents a problem that may have occurred when attempting to apply an automatic fix.
-}
type FixProblem
    = Unchanged { filePath : String, edits : List Edit }
    | InvalidElm { filePath : String, source : String, edits : List Edit, parsingErrors : List Parser.DeadEnd }
    | InvalidJson { filePath : String, source : String, edits : List Edit, decodingError : Json.Decode.Error }
    | EditWithNegativeRange { filePath : String, edit : Edit }
    | HasCollisionsInEditRanges { filePath : String, edits : List Edit }
    | CreatesImportCycle (List String)
    | RemovesUnknownFile String
    | Other String
