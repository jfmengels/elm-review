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
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Review.TypeInference.Type as Type exposing (Type)


type Union
    = Union
        { name : String
        , documentation : String
        , args : List String
        , constructors : Dict String (List Type)
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
        , constructors =
            params.constructors
                |> List.map (\( name_, types ) -> ( name_, List.map Type.fromMetadataType types ))
                |> Dict.fromList
        }


relateToModule : ModuleName -> Union -> Union
relateToModule moduleName (Union union) =
    Union { union | constructors = Dict.map (\_ types -> List.map (Type.relateToModule moduleName) types) union.constructors }


fromMetadataUnion : Elm.Docs.Union -> Union
fromMetadataUnion union =
    Union
        { name = union.name
        , documentation = union.comment
        , args = union.args
        , constructors =
            union.tags
                |> List.map (\( name_, type_ ) -> ( name_, List.map Type.fromMetadataType type_ ))
                |> Dict.fromList
        }


toMetadataUnion : Union -> Elm.Docs.Union
toMetadataUnion (Union union) =
    { name = union.name
    , comment = union.documentation
    , args = union.args
    , tags =
        union.constructors
            |> Dict.toList
            |> List.map (\( name_, type_ ) -> ( name_, List.map (Type.toMetadataType >> Maybe.withDefault (Elm.Type.Var "unknown")) type_ ))
    }


name : Union -> String
name (Union union) =
    union.name


constructors : Union -> List ( String, List Type )
constructors (Union union) =
    Dict.toList union.constructors


constructorsAsDict : Union -> Dict String (List Type)
constructorsAsDict (Union union) =
    union.constructors


args : Union -> List String
args (Union union) =
    union.args


documentation : Union -> String
documentation (Union union) =
    union.documentation
