module Review.TypeInference.Union exposing
    ( Union
    , create
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
