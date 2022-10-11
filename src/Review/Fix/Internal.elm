module Review.Fix.Internal exposing (Fix(..), containRangeCollisions)

import Elm.Syntax.Range exposing (Range)
import Vendor.ListExtra as ListExtra


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Fix
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String


containRangeCollisions : List Fix -> Bool
containRangeCollisions fixes =
    fixes
        |> ListExtra.orderIndependentMap getFixRange
        |> ListExtra.anyCombination collide


getFixRange : Fix -> Range
getFixRange fix_ =
    case fix_ of
        Replacement range _ ->
            range

        Removal range ->
            range

        InsertAt position _ ->
            { start = position, end = position }


collide : Range -> Range -> Bool
collide a b =
    case comparePosition a.end b.start of
        LT ->
            False

        EQ ->
            False

        GT ->
            case comparePosition b.end a.start of
                LT ->
                    False

                EQ ->
                    False

                GT ->
                    True


comparePosition : { row : Int, column : Int } -> { row : Int, column : Int } -> Order
comparePosition a b =
    case compare a.row b.row of
        EQ ->
            compare a.column b.column

        order ->
            order
