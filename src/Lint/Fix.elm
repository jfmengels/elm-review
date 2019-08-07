module Lint.Fix exposing
    ( Fix
    , removeRange, replaceRangeBy, insertAt
    , Result(..), Problem(..), fix
    , mergeRanges
    )

{-| Gives tools to make changes to the source code.


# Definition

@docs Fix


# Constructors

@docs removeRange, replaceRangeBy, insertAt


# Applying fixes

@docs Result, Problem, fix


# Utilitaries for working with ranges

@docs mergeRanges

-}

import Array
import Elm.Parser
import Elm.Syntax.Range exposing (Range)



-- DEFINITION


{-| Represents a patch that will be applied to a file's source code in order to
automatically fix a linting error.
-}
type Fix
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String



-- CONSTRUCTORS


{-| Remove the code in between a range.
-}
removeRange : Range -> Fix
removeRange =
    Removal


{-| Replace the code in between a range by some other code.
-}
replaceRangeBy : Range -> String -> Fix
replaceRangeBy =
    Replacement


{-| Insert some code at the given position.
-}
insertAt : { row : Int, column : Int } -> String -> Fix
insertAt =
    InsertAt



-- APPLYING FIXES


{-| Represents the result of having applied a list of fixes
-}
type Result
    = Successful String
    | Errored Problem


{-| Represents a problem that may have occurred when attempting to apply a list
of fixes.
-}
type Problem
    = Unchanged
    | SourceCodeIsNotValid String


{-| Apply the changes on the source code.
-}
fix : List Fix -> String -> Result
fix fixes sourceCode =
    let
        resultSourceCode : String
        resultSourceCode =
            fixes
                |> List.sortBy (rangePosition >> negate)
                |> List.foldl applyFix (String.lines sourceCode)
                |> String.join "\n"
    in
    if sourceCode == resultSourceCode then
        Errored Unchanged

    else
        case Elm.Parser.parse resultSourceCode of
            Err _ ->
                Errored <| SourceCodeIsNotValid resultSourceCode

            Ok _ ->
                Successful resultSourceCode


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


comparePosition : { row : Int, column : Int } -> { row : Int, column : Int } -> Order
comparePosition a b =
    let
        order : Order
        order =
            compare a.row b.row
    in
    case order of
        EQ ->
            compare a.column b.column

        _ ->
            order


applyFix : Fix -> List String -> List String
applyFix fix_ lines =
    lines
        |> (case fix_ of
                Replacement range replacement ->
                    applyReplace range replacement

                Removal range ->
                    applyReplace range ""

                InsertAt position insertion ->
                    applyReplace { start = position, end = position } insertion
           )


applyReplace : Range -> String -> List String -> List String
applyReplace range replacement lines =
    let
        linesBefore : List String
        linesBefore =
            lines
                |> List.take (range.start.row - 1)

        linesAfter : List String
        linesAfter =
            lines
                |> List.drop range.end.row

        startLine : String
        startLine =
            getRowAtLine lines (range.start.row - 1)
                |> String.slice 0 (range.start.column - 1)

        endLine : String
        endLine =
            getRowAtLine lines (range.end.row - 1)
                |> String.dropLeft (range.end.column - 1)
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



-- UTILITARIES FOR WORKING WITH RANGES


{-| Create a new range that starts at the start of the range that starts first,
and ends at the end of the range that starts last. If the two ranges are distinct
and there is code in between, that code will be included in the resulting range.

    range : Range
    range =
        Fix.mergeRanges
            (Node.range node1)
            (Node.range node2)

-}
mergeRanges : Range -> Range -> Range
mergeRanges a b =
    let
        start : { row : Int, column : Int }
        start =
            case comparePosition a.start b.start of
                LT ->
                    a.start

                EQ ->
                    a.start

                GT ->
                    b.start

        end : { row : Int, column : Int }
        end =
            case comparePosition a.end b.end of
                LT ->
                    b.end

                EQ ->
                    b.end

                GT ->
                    a.end
    in
    { start = start, end = end }
