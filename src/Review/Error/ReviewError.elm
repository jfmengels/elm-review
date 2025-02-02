module Review.Error.ReviewError exposing
    ( InternalError
    , ReviewError(..)
    , doesPreventExtract
    , error
    , errorFixes
    , fromBaseError
    )

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Review.Error.FileTarget as FileTarget exposing (FileTarget)
import Review.Error.Fixes as ErrorFixes exposing (ErrorFixes)
import Review.Error.Target as Target
import Review.Fix exposing (Fix)
import Review.Fix.FixProblem exposing (FixProblem)


type ReviewError
    = ReviewError InternalError


fromBaseError :
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    , fixes : ErrorFixes
    , fixProblem : Maybe FixProblem
    , target : Target.Target
    , preventsExtract : Bool
    }
    -> ReviewError
fromBaseError internalError =
    { message = internalError.message
    , ruleName = internalError.ruleName
    , filePath = internalError.filePath
    , details = internalError.details
    , range = internalError.range
    , originalFixes = internalError.fixes
    , fixes = compileFixes internalError.fixes internalError.fixProblem
    , fixProblem = internalError.fixProblem
    , target = internalError.target
    , preventsExtract = internalError.preventsExtract
    }
        |> ReviewError


compileFixes : ErrorFixes -> Maybe FixProblem -> Result FixProblem (Maybe (List ( String, Maybe (List Fix) )))
compileFixes fixes maybeFixProblem =
    case maybeFixProblem of
        Just fixProblem ->
            Err fixProblem

        Nothing ->
            if ErrorFixes.isEmpty fixes then
                Ok Nothing

            else
                compileFixesHelp (ErrorFixes.toList fixes) []
                    |> Result.map Just


compileFixesHelp : List ( FileTarget, ErrorFixes.FixKind ) -> List ( String, Maybe (List Fix) ) -> Result x (List ( String, Maybe (List Fix) ))
compileFixesHelp fixes acc =
    case fixes of
        [] ->
            Ok acc

        ( target, fixKind ) :: rest ->
            let
                fix : Maybe (List Fix)
                fix =
                    case fixKind of
                        ErrorFixes.Edit edits ->
                            Just edits

                        ErrorFixes.Remove ->
                            Nothing
            in
            compileFixesHelp rest (( FileTarget.filePath target, fix ) :: acc)


type alias InternalError =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    , fixes : Result FixProblem (Maybe (List ( String, Maybe (List Fix) )))
    , originalFixes : ErrorFixes
    , fixProblem : Maybe FixProblem
    , target : Target.Target
    , preventsExtract : Bool
    }


error : { message : String, details : List String } -> Range -> ReviewError
error { message, details } range =
    ReviewError
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , originalFixes = ErrorFixes.none
        , fixes = Ok Nothing
        , fixProblem = Nothing
        , target = Target.module_ ""
        , preventsExtract = False
        }


doesPreventExtract : InternalError -> Bool
doesPreventExtract error_ =
    error_.preventsExtract


errorFixes : ReviewError -> Result FixProblem (List ( FileTarget, ErrorFixes.FixKind ))
errorFixes (ReviewError err) =
    case err.fixProblem of
        Just fixProblem ->
            Err fixProblem

        Nothing ->
            ErrorFixes.toList err.originalFixes
                |> Ok
