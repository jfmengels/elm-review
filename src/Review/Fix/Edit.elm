module Review.Fix.Edit exposing (Edit(..), Fix)

import Elm.Syntax.Range exposing (Range)


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Edit
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String


type alias Fix =
    Edit
