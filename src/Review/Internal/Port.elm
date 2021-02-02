module Review.Internal.Port exposing (Port, name, tipe)

{-| Fill
-}

import Review.Api.Type exposing (Type)


type Port
    = Port
        { name : String
        , tipe : Type
        }


name : Port -> String
name (Port p) =
    p.name


tipe : Port -> Type
tipe (Port p) =
    p.tipe
