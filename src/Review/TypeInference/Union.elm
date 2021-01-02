module Review.TypeInference.Union exposing
    ( Union
    , args
    , comment
    , create
    , fromMetadataUnion
    , name
    , tags
    , toMetadataUnion
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Elm.Docs
import Elm.Type


type Union
    = Union Elm.Docs.Union


create :
    { name : String
    , comment : String
    , args : List String
    , tags : List ( String, List Elm.Type.Type )
    }
    -> Union
create =
    Union


fromMetadataUnion : Elm.Docs.Union -> Union
fromMetadataUnion union =
    Union union


toMetadataUnion : Union -> Elm.Docs.Union
toMetadataUnion (Union union) =
    union


name : Union -> String
name (Union union) =
    union.name


tags : Union -> List ( String, List Elm.Type.Type )
tags (Union union) =
    union.tags


args : Union -> List String
args (Union union) =
    union.args


comment : Union -> String
comment (Union union) =
    union.comment
