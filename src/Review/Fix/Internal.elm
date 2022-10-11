module Review.Fix.Internal exposing (Fix(..))

import Elm.Syntax.Range exposing (Range)


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Fix
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String
