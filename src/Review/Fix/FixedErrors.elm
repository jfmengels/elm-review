module Review.Fix.FixedErrors exposing (FixedErrors)

import Dict exposing (Dict)
import Review.Error exposing (ReviewError)


type alias FixedErrors =
    Dict String (List ReviewError)
