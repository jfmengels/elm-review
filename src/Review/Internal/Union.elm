module Review.Internal.Union exposing
    ( Union
    , args
    , constructors
    , create
    , documentation
    , fromElmDocs
    , name
    , relateToModule
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Api.Type as Type exposing (Type)


type Union
    = Union
        { name : String
        , documentation : Maybe String
        , args : List String
        , constructors : Dict String (List Type)
        }


create :
    { name : String
    , documentation : Maybe String
    , args : List String
    , constructors : Dict String (List Type)
    }
    -> Union
create =
    Union


relateToModule : ModuleName -> Union -> Union
relateToModule moduleName (Union union) =
    Union { union | constructors = Dict.map (\_ types -> List.map (Type.relateToModule moduleName) types) union.constructors }


fromElmDocs : Elm.Docs.Union -> Union
fromElmDocs union =
    Union
        { name = union.name
        , documentation = Just union.comment
        , args = union.args
        , constructors =
            union.tags
                |> List.map (\( name_, type_ ) -> ( name_, List.map Type.fromElmDocs type_ ))
                |> Dict.fromList
        }


name : Union -> String
name (Union union) =
    union.name


constructors : Union -> Dict String (List Type)
constructors (Union union) =
    union.constructors


args : Union -> List String
args (Union union) =
    union.args


documentation : Union -> Maybe String
documentation (Union union) =
    union.documentation
