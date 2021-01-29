module Review.Type.Union exposing
    ( Union
    , name, args, documentation
    , constructors, constructorsAsDict
    , create
    )

{-| Represents custom type definitions found in modules.

@docs Union


# Access

@docs name, args, documentation
@docs constructors, constructorsAsDict


# Creation

@docs create

-}

import Dict exposing (Dict)
import Review.Internal.Union
import Review.Type exposing (Type)


type alias Union =
    Review.Internal.Union.Union


create :
    { name : String
    , documentation : Maybe String
    , args : List String
    , constructors : List ( String, List Type )
    }
    -> Union
create =
    Review.Internal.Union.create


name : Union -> String
name =
    Review.Internal.Union.name


constructors : Union -> List ( String, List Type )
constructors =
    Review.Internal.Union.constructorsAsDict >> Dict.toList


constructorsAsDict : Union -> Dict String (List Type)
constructorsAsDict =
    Review.Internal.Union.constructorsAsDict


args : Union -> List String
args =
    Review.Internal.Union.args


documentation : Union -> Maybe String
documentation =
    Review.Internal.Union.documentation
