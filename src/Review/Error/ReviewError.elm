module Review.Error.ReviewError exposing
    ( InternalError
    , ReviewError(..)
    , doesPreventExtract
    , error
    , markFixesAsProblem
    , preventExtract
    )

import Elm.Syntax.Range exposing (Range)
import Review.Error.Fixes as ErrorFixes exposing (ErrorFixes)
import Review.Error.Target as Target exposing (Target(..))
import Review.Fix.FixProblem exposing (FixProblem)


type ReviewError
    = ReviewError InternalError


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
