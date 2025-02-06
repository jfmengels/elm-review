module Review.Error.ReviewError exposing
    ( InternalError
    , ReviewError(..)
    , error
    , fromBaseError
    )

import Elm.Syntax.Range exposing (Range)
import Review.Error.FileTarget as FileTarget exposing (FileTarget)
import Review.Error.Fixes as ErrorFixes exposing (ErrorFixes)
import Review.Error.Target as Target
import Review.Fix.FixProblem exposing (FixProblem)
import Review.Fix.Internal


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


compileFixes : ErrorFixes -> Maybe FixProblem -> Result FixProblem (Maybe (List ( FileTarget, ErrorFixes.FixKind )))
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


compileFixesHelp : List ( FileTarget, ErrorFixes.FixKind ) -> List ( FileTarget, ErrorFixes.FixKind ) -> Result FixProblem (List ( FileTarget, ErrorFixes.FixKind ))
compileFixesHelp fixes acc =
    case fixes of
        [] ->
            Ok acc

        ( target, fixKind ) :: rest ->
            let
                fix : Result FixProblem ErrorFixes.FixKind
                fix =
                    case fixKind of
                        ErrorFixes.Edit edits ->
                            Review.Fix.Internal.compileEdits (FileTarget.filePath target) edits
                                |> Result.map ErrorFixes.Edit

                        ErrorFixes.Remove ->
                            Ok fixKind
            in
            case fix of
                Ok fix_ ->
                    compileFixesHelp rest (( target, fix_ ) :: acc)

                Err err ->
                    Err err


type alias InternalError =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    , fixes : Result FixProblem (Maybe (List ( FileTarget, ErrorFixes.FixKind )))
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
