module Lint.RuleError exposing (RuleError, fromError)

{-| Value that describes an error found by a rule and includes the error.

    error : Range -> Error
    error range =
        Error "Forbidden use of Debug" range

-}

import Elm.Syntax.Range exposing (Range)
import Lint.Error as Error exposing (Error)


type alias RuleError =
    { rule : String
    , message : String
    , range : Range
    }


fromError : String -> Error -> RuleError
fromError rule error =
    { rule = rule
    , message = Error.message error
    , range = Error.range error
    }
