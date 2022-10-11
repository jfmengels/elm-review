module Review.Fix.Internal exposing (Fix(..), applyFix, containRangeCollisions)

import Array
import Elm.Syntax.Range exposing (Range)
import Unicode
import Vendor.ListExtra as ListExtra


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Fix
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String



-- APPLY FIX


applyFix : Fix -> List String -> List String
applyFix fix_ lines =
    case fix_ of
        Replacement range replacement ->
            applyReplace range replacement lines

        Removal range ->
            applyReplace range "" lines

        InsertAt position insertion ->
            applyReplace { start = position, end = position } insertion lines


applyReplace : Range -> String -> List String -> List String
applyReplace range replacement lines =
    let
        linesBefore : List String
        linesBefore =
            List.take (range.start.row - 1) lines

        linesAfter : List String
        linesAfter =
            List.drop range.end.row lines

        startLine : String
        startLine =
            getRowAtLine lines (range.start.row - 1)
                |> Unicode.left (range.start.column - 1)

        endLine : String
        endLine =
            getRowAtLine lines (range.end.row - 1)
                |> Unicode.dropLeft (range.end.column - 1)
    in
    List.concat
        [ linesBefore
        , startLine ++ replacement ++ endLine |> String.lines
        , linesAfter
        ]


getRowAtLine : List String -> Int -> String
getRowAtLine lines rowIndex =
    case lines |> Array.fromList |> Array.get rowIndex of
        Just line ->
            if String.trim line /= "" then
                line

            else
                ""

        Nothing ->
            ""



-- CONTAINS RANGE COLLISIONS


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
