module Review.Target exposing (Target(..))

{-|

@docs Target

-}


{-| Possible error targets.
-}
type Target
    = Module
    | ElmJson
    | Readme
    | Global
