module Review.Error.ReviewError exposing
    ( InternalError
    , ReviewError(..)
    , doesPreventExtract
    , error
    , fromBaseError
    , markFixesAsProblem
    , preventExtract
    )

import Elm.Syntax.Range exposing (Range)
import Review.Error.Fixes as ErrorFixes exposing (ErrorFixes)
import Review.Error.Target as Target
import Review.Fix.FixProblem exposing (FixProblem)


type ReviewError
    = ReviewError InternalError


fromBaseError : InternalError -> ReviewError
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
        |> ReviewError


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


markFixesAsProblem : FixProblem -> InternalError -> InternalError
markFixesAsProblem fixProblem error_ =
    { error_ | fixProblem = Just fixProblem }


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
