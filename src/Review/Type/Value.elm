module Review.Type.Value exposing
    ( Value
    , create
    , documentation
    , fromElmDocs
    , name
    , tipe
    , toElmDocs
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Elm.Docs
import Review.Internal.Value
import Review.Type as Type exposing (Type(..))


type alias Value =
    Review.Internal.Value.Value


create : { name : String, documentation : String, tipe : Type } -> Value
create =
    Review.Internal.Value.create


fromElmDocs : Elm.Docs.Value -> Value
fromElmDocs =
    Review.Internal.Value.fromElmDocs


toElmDocs : Value -> Maybe Elm.Docs.Value
toElmDocs =
    Review.Internal.Value.toElmDocs



-- ACCESS


name : Value -> String
name =
    Review.Internal.Value.name


documentation : Value -> String
documentation =
    Review.Internal.Value.documentation


tipe : Value -> Type.Type
tipe =
    Review.Internal.Value.tipe
