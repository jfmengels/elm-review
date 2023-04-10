module Review.Fix.Internal exposing (Fix(..), applyFix, containRangeCollisions, fixElmJson, fixModule, fixReadme, rangePosition)

import Array
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Range exposing (Range)
import Json.Decode as Decode
import Review.FileParser as FileParser
import Review.Fix.FixProblem as FixProblem
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



-- FIX ELM MODULE


{-| Apply the changes on the source code.
-}
fixModule : List Fix -> String -> Result FixProblem.FixProblem { source : String, ast : File }
fixModule fixes originalSourceCode =
    case tryToApplyFix fixes originalSourceCode of
        Ok fixedSourceCode ->
            case FileParser.parse fixedSourceCode of
                Ok ast ->
                    Ok { source = fixedSourceCode, ast = ast }

                Err _ ->
                    Err (FixProblem.SourceCodeIsNotValid fixedSourceCode)

        Err err ->
            Err err


{-| Apply the changes on the elm.json file.
-}
fixElmJson : List Fix -> String -> Result FixProblem.FixProblem { raw : String, project : Elm.Project.Project }
fixElmJson fixes originalSourceCode =
    case tryToApplyFix fixes originalSourceCode of
        Ok resultAfterFix ->
            case Decode.decodeString Elm.Project.decoder resultAfterFix of
                Ok project ->
                    Ok { raw = resultAfterFix, project = project }

                Err _ ->
                    Err (FixProblem.SourceCodeIsNotValid resultAfterFix)

        Err err ->
            Err err


{-| Apply the changes on the README.md file.
-}
fixReadme : List Fix -> String -> Result FixProblem.FixProblem String
fixReadme fixes originalSourceCode =
    tryToApplyFix fixes originalSourceCode


tryToApplyFix : List Fix -> String -> Result FixProblem.FixProblem String
tryToApplyFix fixes sourceCode =
    if containRangeCollisions fixes then
        Err FixProblem.HasCollisionsInFixRanges

    else
        let
            resultAfterFix : String
            resultAfterFix =
                fixes
                    |> List.sortBy (rangePosition >> negate)
                    |> List.foldl applyFix (String.lines sourceCode)
                    |> String.join "\n"
        in
        if sourceCode == resultAfterFix then
            Err FixProblem.Unchanged

        else
            Ok resultAfterFix



-- CONTAINS RANGE COLLISIONS


containRangeCollisions : List Fix -> Bool
containRangeCollisions fixes =
    fixes
        |> List.map getFixRange
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



-- RANGE POSITION


rangePosition : Fix -> Int
rangePosition fix_ =
    positionAsInt <|
        case fix_ of
            Replacement range _ ->
                range.start

            Removal range ->
                range.start

            InsertAt position _ ->
                position


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long. Then, as long as ranges don't overlap,
    -- this should work fine.
    row * 1000000 + column
