module Lint.Fix exposing
    ( Fix
    , removeRange, replaceRangeBy, insertAt
    , FixResult(..), Problem(..), fix
    )

{-| Gives tools to make changes to the source code.


# Definition

@docs Fix


# Constructors

@docs removeRange, replaceRangeBy, insertAt


# Applying fixes

@docs FixResult, Problem, fix

-}

import Array
import Elm.Parser
import Elm.Syntax.Range exposing (Range)
import ListExtra



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


{-| Represents the result of having applied a list of fixes to a source code.
-}
type FixResult
    = Successful String
    | Errored Problem


{-| Represents a problem that may have occurred when attempting to apply a list
of fixes.
-}
type Problem
    = Unchanged
    | SourceCodeIsNotValid String
    | HasCollisionsInFixRanges


{-| Apply the changes on the source code.
-}
fix : List Fix -> String -> FixResult
fix fixes sourceCode =
    if containRangeCollisions fixes then
        Errored HasCollisionsInFixRanges

    else
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


containRangeCollisions : List Fix -> Bool
containRangeCollisions fixes =
    fixes
        |> List.map getFixRange
        |> ListExtra.uniquePairs
        |> List.any (\( a, b ) -> collide a b)


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


rangePosition : Fix -> Int
rangePosition fix_ =
    positionAsInt <|
        case fix_ of
            Replacement range replacement ->
                range.start

            Removal range ->
                range.start

            InsertAt position insertion ->
                position


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
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
