module Lint.Error exposing (Error)

{-| Value that describes an error found by a rule, and contains the name of the rule that raised the error, and a description of the error.

    error : Error
    error =
        Error "NoDebug" "Forbidden use of Debug"

-}

import Elm.Syntax.Range exposing (Range)


type alias Error =
    { rule : String
    , message : String
    , range : Range
    }
