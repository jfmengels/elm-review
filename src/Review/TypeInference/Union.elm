module Review.TypeInference.Union exposing
    ( Union
    , args
    , constructors
    , constructorsAsDict
    , create
    , documentation
    , fromMetadataUnion
    , name
    , toMetadataUnion
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Dict exposing (Dict)
import Elm.Docs
import Elm.Type


type Union
    = Union
        { name : String
        , documentation : String
        , args : List String
        , constructors : Dict String (List Elm.Type.Type)
        }


create :
    { name : String
    , documentation : String
    , args : List String
    , constructors : List ( String, List Elm.Type.Type )
    }
    -> Union
create params =
    Union
        { name = params.name
        , documentation = params.documentation
        , args = params.args
        , constructors = Dict.fromList params.constructors
        }


fromMetadataUnion : Elm.Docs.Union -> Union
fromMetadataUnion union =
    Union
        { name = union.name
        , documentation = union.comment
        , args = union.args
        , constructors = Dict.fromList union.tags
        }


toMetadataUnion : Union -> Elm.Docs.Union
toMetadataUnion (Union union) =
    { name = union.name
    , comment = union.documentation
    , args = union.args
    , tags = Dict.toList union.constructors
    }


name : Union -> String
name (Union union) =
    union.name


constructors : Union -> List ( String, List Elm.Type.Type )
constructors (Union union) =
    Dict.toList union.constructors


constructorsAsDict : Union -> Dict String (List Elm.Type.Type)
constructorsAsDict (Union union) =
    union.constructors


args : Union -> List String
args (Union union) =
    union.args


documentation : Union -> String
documentation (Union union) =
    union.documentation
