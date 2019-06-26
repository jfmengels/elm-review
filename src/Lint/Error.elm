module Lint.Error exposing
    ( Error
    , create
    , message, range
    )

{-| Value that describes an error found by a rule, and contains the name of the rule that raised the error, and a description of the error.

    error : Range -> Error
    error range =
        Error "Forbidden use of Debug" range


# Definition

@docs Error


# CONSTRUCTOR

@docs create


# Access

@docs message, range

-}

import Elm.Syntax.Range exposing (Range)



-- DEFINITION


type Error
    = Error
        { message : String
        , range : Range
        }



-- CONSTRUCTOR


create : String -> Range -> Error
create message_ range_ =
    Error
        { message = message_
        , range = range_
        }



-- ACCESS


message : Error -> String
message (Error error) =
    error.message


range : Error -> Range
range (Error error) =
    error.range
