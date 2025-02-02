module Review.Error.ReviewError exposing
    ( InternalError
    , ReviewError(..)
    , doesPreventExtract
    , error
    , errorFixes
    , fromBaseError
    , preventExtract
    )

import Elm.Syntax.Range exposing (Range)
import Review.Error.FileTarget exposing (FileTarget)
import Review.Error.Fixes as ErrorFixes exposing (ErrorFixes)
import Review.Error.Target as Target
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
    , fixes = internalError.fixes
    , fixProblem = internalError.fixProblem
    , target = internalError.target
    , preventsExtract = internalError.preventsExtract
    }
        |> condenseFixes
        |> ReviewError


condenseFixes : InternalError -> InternalError
condenseFixes internalError =
    case internalError.fixProblem of
        Just _ ->
            internalError

        Nothing ->
            if ErrorFixes.isEmpty internalError.fixes then
                internalError

            else
                case optimizeFixes internalError.fixes of
                    Ok fixes ->
                        { internalError | fixes = fixes }

                    Err fixProblem ->
                        { internalError | fixProblem = Just fixProblem }


optimizeFixes : ErrorFixes -> Result FixProblem ErrorFixes
optimizeFixes =
    Ok


type alias InternalError =
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


error : { message : String, details : List String } -> Range -> ReviewError
error { message, details } range =
    ReviewError
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.module_ ""
        , preventsExtract = False
        }


preventExtract : InternalError -> InternalError
preventExtract error_ =
    { error_ | preventsExtract = True }


doesPreventExtract : InternalError -> Bool
doesPreventExtract error_ =
    error_.preventsExtract


errorFixes : ReviewError -> Result FixProblem (List ( FileTarget, ErrorFixes.FixKind ))
errorFixes (ReviewError err) =
    case err.fixProblem of
        Just fixProblem ->
            Err fixProblem

        Nothing ->
            ErrorFixes.toList err.fixes
                |> Ok
