module Simplify.Match exposing (Match(..))

{-|

@docs Match

-}


type Match a
    = Determined a
    | Undetermined
