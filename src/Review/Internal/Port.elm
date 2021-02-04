module Review.Internal.Port exposing (Port, create, name, tipe)

{-| Fill
-}

import Review.Api.Type exposing (Type)


type Port
    = Port
        { name : String
        , tipe : Type
        }


create : { name : String, tipe : Type } -> Port
create =
    Port


name : Port -> String
name (Port p) =
    p.name


tipe : Port -> Type
tipe (Port p) =
    p.tipe
