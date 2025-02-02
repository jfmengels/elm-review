module Review.Fix.Internal exposing
    ( Edit(..)
    , Fix
    , applyEdits
    , compileEdits
    , editElmJson
    , editModule
    )

import Array
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Range exposing (Range)
import Json.Decode as Decode
import Review.FileParser as FileParser
import Review.Fix.FixProblem as FixProblem exposing (FixProblem)
import Unicode


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Edit
    = Removal Range
    | Replacement Range String
    | InsertAt { row : Int, column : Int } String


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type alias Fix =
    Edit


compileEdits : List Edit -> Result FixProblem (List Edit)
compileEdits edits =
    compileEditsHelp edits []


compileEditsHelp : List Edit -> List Edit -> Result FixProblem (List Edit)
compileEditsHelp edits acc =
    case edits of
        [] ->
            Ok acc

        edit :: rest ->
            case edit of
                InsertAt _ "" ->
                    compileEditsHelp rest acc

                InsertAt _ _ ->
                    compileEditsHelp rest (edit :: acc)

                Removal range ->
                    if range.start == range.end then
                        compileEditsHelp rest acc

                    else
                        compileEditsHelp rest (edit :: acc)

                Replacement _ _ ->
                    compileEditsHelp rest (edit :: acc)



-- EDIT ELM MODULE


{-| Apply the edits on the source code.
-}
applyEdits : List Edit -> String -> Result FixProblem String
applyEdits fixes sourceCode =
    if containRangeCollisions fixes then
        Err FixProblem.HasCollisionsInFixRanges

    else
        let
            resultAfterEdit : String
            resultAfterEdit =
                fixes
                    |> List.sortBy (rangePosition >> negate)
                    |> applyIndividualEdits (String.lines sourceCode) []
                    |> String.join "\n"
        in
        if sourceCode == resultAfterEdit then
            Err FixProblem.Unchanged

        else
            Ok resultAfterEdit


{-| Apply the changes on the source code.
-}
editModule : List Edit -> String -> Result FixProblem.FixProblem { source : String, ast : File }
editModule fixes originalSourceCode =
    case applyEdits fixes originalSourceCode of
        Ok fixedSourceCode ->
            case FileParser.parse fixedSourceCode of
                Ok ast ->
                    Ok { source = fixedSourceCode, ast = ast }

                Err _ ->
                    Err (FixProblem.SourceCodeIsNotValid fixedSourceCode)

        Err err ->
            Err err


applyIndividualEdits : List String -> List String -> List Edit -> List String
applyIndividualEdits lines linesAfter edits =
    case edits of
        [] ->
            lines ++ linesAfter

        edit :: restOfEdits ->
            let
                ( rangeToReplace, replacement ) =
                    case edit of
                        Replacement range replacement_ ->
                            ( range, replacement_ )

                        Removal range ->
                            ( range, "" )

                        InsertAt position insertion ->
                            ( { start = position, end = position }, insertion )

                ( newLines, newLinesAfter ) =
                    applyReplace rangeToReplace replacement lines
            in
            applyIndividualEdits newLines (newLinesAfter ++ linesAfter) restOfEdits


{-| Apply the changes on the elm.json file.
-}
editElmJson : List Edit -> String -> Result FixProblem.FixProblem { raw : String, project : Elm.Project.Project }
editElmJson fixes originalSourceCode =
    case applyEdits fixes originalSourceCode of
        Ok resultAfterFix ->
            case Decode.decodeString Elm.Project.decoder resultAfterFix of
                Ok project ->
                    Ok { raw = resultAfterFix, project = project }

                Err _ ->
                    Err (FixProblem.SourceCodeIsNotValid resultAfterFix)

        Err err ->
            Err err


applyReplace : Range -> String -> List String -> ( List String, List String )
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
    ( List.concat
        [ linesBefore
        , startLine ++ replacement ++ endLine |> String.lines
        ]
    , linesAfter
    )


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


containRangeCollisions : List Edit -> Bool
containRangeCollisions fixes =
    fixes
        |> List.sortWith (\a b -> compareRanges (getEditRange a) (getEditRange b))
        |> anyCombinationCollides


anyCombinationCollides : List Edit -> Bool
anyCombinationCollides xs =
    case xs of
        [] ->
            False

        x :: xs_ ->
            if List.any (\y -> collide (getEditRange x) (getEditRange y)) xs_ then
                True

            else
                anyCombinationCollides xs_


getEditRange : Edit -> Range
getEditRange edit =
    case edit of
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


compareRanges : Range -> Range -> Order
compareRanges a b =
    case comparePosition a.end b.start of
        EQ ->
            comparePosition b.end a.start

        order ->
            order


comparePosition : { row : Int, column : Int } -> { row : Int, column : Int } -> Order
comparePosition a b =
    case compare a.row b.row of
        EQ ->
            compare a.column b.column

        order ->
            order



-- RANGE POSITION


rangePosition : Edit -> Int
rangePosition edit =
    positionAsInt <|
        case edit of
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
