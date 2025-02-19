module Review.Fix.Internal exposing
    ( applyEdits
    , compileEdits
    , editElmJson
    , editModule
    )

import Array
import Elm.Parser as Parser
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Range exposing (Location, Range)
import Json.Decode as Decode
import Review.Fix.Edit exposing (Edit(..))
import Review.Fix.FixProblem as FixProblem exposing (FixProblem)
import Unicode


compileEdits : String -> List Edit -> Result FixProblem (List Edit)
compileEdits filePath edits =
    compileEditsHelp
        filePath
        (List.sortWith (\a b -> compareRanges (getEditRange b) (getEditRange a)) edits)
        { row = infinity, column = infinity }
        (InsertAt { row = infinity, column = infinity } "")
        Nothing
        []


infinity : Int
infinity =
    round (1 / 0)


compileEditsHelp : String -> List Edit -> Location -> Edit -> Maybe Range -> List Edit -> Result FixProblem (List Edit)
compileEditsHelp filePath edits previousStart previousEdit previousRemoval acc =
    case edits of
        [] ->
            addMaybeRemovalEdit previousRemoval acc
                |> List.reverse
                |> Ok

        edit :: rest ->
            case edit of
                InsertAt _ "" ->
                    compileEditsHelp filePath rest previousStart previousEdit previousRemoval acc

                InsertAt position _ ->
                    case comparePosition position previousStart of
                        GT ->
                            FixProblem.HasCollisionsInEditRanges { filePath = filePath, edits = [ edit, previousEdit ] }
                                |> Err

                        _ ->
                            compileEditsHelp filePath rest position edit Nothing (edit :: addMaybeRemovalEdit previousRemoval acc)

                Removal range ->
                    case comparePosition range.start range.end of
                        EQ ->
                            compileEditsHelp filePath rest previousStart previousEdit previousRemoval acc

                        GT ->
                            FixProblem.EditWithNegativeRange { filePath = filePath, edit = edit }
                                |> Err

                        LT ->
                            case comparePosition range.end previousStart of
                                GT ->
                                    case previousRemoval of
                                        Just { end } ->
                                            compileEditsHelp
                                                filePath
                                                rest
                                                range.start
                                                edit
                                                (Just { start = range.start, end = end })
                                                acc

                                        Nothing ->
                                            FixProblem.HasCollisionsInEditRanges { filePath = filePath, edits = [ edit, previousEdit ] }
                                                |> Err

                                EQ ->
                                    case previousRemoval of
                                        Just { end } ->
                                            compileEditsHelp
                                                filePath
                                                rest
                                                range.start
                                                edit
                                                (Just { start = range.start, end = end })
                                                acc

                                        Nothing ->
                                            compileEditsHelp filePath rest range.start edit (Just range) (addMaybeRemovalEdit previousRemoval acc)

                                LT ->
                                    compileEditsHelp filePath rest range.start edit (Just range) (addMaybeRemovalEdit previousRemoval acc)

                Replacement range _ ->
                    case comparePosition range.start range.end of
                        GT ->
                            FixProblem.EditWithNegativeRange { filePath = filePath, edit = edit }
                                |> Err

                        _ ->
                            case comparePosition range.end previousStart of
                                GT ->
                                    FixProblem.HasCollisionsInEditRanges { filePath = filePath, edits = [ edit, previousEdit ] }
                                        |> Err

                                _ ->
                                    compileEditsHelp filePath rest range.start edit Nothing (edit :: addMaybeRemovalEdit previousRemoval acc)


addMaybeRemovalEdit : Maybe Range -> List Edit -> List Edit
addMaybeRemovalEdit previousRemoval acc =
    case previousRemoval of
        Just range ->
            Removal range :: acc

        Nothing ->
            acc



-- EDIT ELM MODULE


{-| Apply the edits on the source code.
-}
applyEdits : String -> List Edit -> String -> Result FixProblem String
applyEdits filePath edits sourceCode =
    let
        resultAfterEdit : String
        resultAfterEdit =
            edits
                |> applyIndividualEdits (String.lines sourceCode) []
                |> String.join "\n"
    in
    if sourceCode == resultAfterEdit then
        Err (FixProblem.Unchanged { filePath = filePath, edits = edits })

    else
        Ok resultAfterEdit


{-| Apply the changes on the source code.
-}
editModule : List Edit -> String -> String -> Result FixProblem.FixProblem { source : String, ast : File }
editModule edits filePath originalSourceCode =
    case applyEdits filePath edits originalSourceCode of
        Ok fixedSourceCode ->
            case Parser.parseToFile fixedSourceCode of
                Ok ast ->
                    Ok { source = fixedSourceCode, ast = ast }

                Err parsingErrors ->
                    Err (FixProblem.InvalidElm { filePath = filePath, source = fixedSourceCode, edits = edits, parsingErrors = parsingErrors })

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
editElmJson edits originalSourceCode =
    case applyEdits "elm.json" edits originalSourceCode of
        Ok resultAfterFix ->
            case Decode.decodeString Elm.Project.decoder resultAfterFix of
                Ok project ->
                    Ok { raw = resultAfterFix, project = project }

                Err decodingError ->
                    Err (FixProblem.InvalidJson { filePath = "elm.json", source = resultAfterFix, edits = edits, decodingError = decodingError })

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


getEditRange : Edit -> Range
getEditRange edit =
    case edit of
        Replacement range _ ->
            range

        Removal range ->
            range

        InsertAt position _ ->
            { start = position, end = position }


compareRanges : Range -> Range -> Order
compareRanges a b =
    case comparePosition a.start b.start of
        EQ ->
            comparePosition a.end b.end

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
