module Review.TypeInference.Union exposing
    ( Union
    , args
    , create
    , documentation
    , fromMetadataUnion
    , name
    , tags
    , toMetadataUnion
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Elm.Docs
import Elm.Type


type Union
    = Union
        { name : String
        , documentation : String
        , args : List String
        , tags : List ( String, List Elm.Type.Type )
        }


create :
    { name : String
    , documentation : String
    , args : List String
    , tags : List ( String, List Elm.Type.Type )
    }
    -> Union
create params =
    Union
        { name = params.name
        , documentation = params.documentation
        , args = params.args
        , tags = params.tags
        }


fromMetadataUnion : Elm.Docs.Union -> Union
fromMetadataUnion union =
    Union
        { name = union.name
        , documentation = union.comment
        , args = union.args
        , tags = union.tags
        }


toMetadataUnion : Union -> Elm.Docs.Union
toMetadataUnion (Union union) =
    { name = union.name
    , comment = union.documentation
    , args = union.args
    , tags = union.tags
    }


name : Union -> String
name (Union union) =
    union.name


tags : Union -> List ( String, List Elm.Type.Type )
tags (Union union) =
    union.tags


args : Union -> List String
args (Union union) =
    union.args


documentation : Union -> String
documentation (Union union) =
    union.documentation
