module Lint.Fix exposing
    ( Fix
    , removeRange, replaceRangeBy, insertAt
    , fix
    )

{-| Gives tools to make changes to the source code.


# Definition

@docs Fix


# Constructors

@docs removeRange, replaceRangeBy, insertAt


# Applying fixes

@docs fix

-}

import Array
import Elm.Syntax.Range exposing (Range)



-- DEFINITION


{-| -}
type Fix
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String



-- CONSTRUCTORS


{-| Remove the code at the given range.
-}
removeRange : Range -> Fix
removeRange =
    Removal


replaceRangeBy : Range -> String -> Fix
replaceRangeBy =
    Replacement


{-| Remove the code at the given range.
-}
insertAt : { row : Int, column : Int } -> String -> Fix
insertAt =
    InsertAt



-- APPLYING FIXES


{-| Apply the changes on the source code.
-}
fix : List Fix -> String -> String
fix fixes sourceCode =
    fixes
        |> List.sortBy (rangePosition >> negate)
        |> List.foldl applyFix sourceCode


rangePosition : Fix -> Int
rangePosition fix_ =
    let
        { row, column } =
            case fix_ of
                Replacement range replacement ->
                    range.start

                Removal range ->
                    range.start

                InsertAt position insertion ->
                    position
    in
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long. Then, as long as ranges don't overlap,
    -- this should work fine.
    row * 1000000 + column


applyFix : Fix -> String -> String
applyFix fix_ source =
    source
        |> (case fix_ of
                Replacement range replacement ->
                    applyReplace range replacement

                Removal range ->
                    applyReplace range ""

                InsertAt position insertion ->
                    applyReplace { start = position, end = position } insertion
           )


applyReplace : Range -> String -> String -> String
applyReplace range replacement source =
    let
        lines : List String
        lines =
            String.lines source

        linesBefore : String
        linesBefore =
            lines
                |> List.take (range.start.row - 1)
                |> String.join "\n"

        linesAfter : String
        linesAfter =
            lines
                |> List.drop range.end.row
                |> String.join "\n"

        line : String
        line =
            getRowAtLine lines (range.start.row - 1)

        lineBefore : String
        lineBefore =
            String.slice 0 (range.start.column - 1) line

        lineAfter : String
        lineAfter =
            String.dropLeft (range.end.column - 1) line
    in
    linesBefore ++ "\n" ++ lineBefore ++ replacement ++ lineAfter ++ "\n" ++ linesAfter


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
