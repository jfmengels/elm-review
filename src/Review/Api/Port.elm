module Review.Api.Port exposing (Port, name, tipe)

-- TODO Expose module

import Review.Api.Type exposing (Type)
import Review.Internal.Port


type alias Port =
    Review.Internal.Port.Port


name : Port -> String
name =
    Review.Internal.Port.name


tipe : Review.Internal.Port.Port -> Type
tipe =
    Review.Internal.Port.tipe
