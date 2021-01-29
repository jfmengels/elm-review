module Review.Internal.Union exposing
    ( Union
    , args
    , constructorsAsDict
    , create
    , documentation
    , fromElmDocs
    , name
    , relateToModule
    , toElmDocs
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Review.Type as Type exposing (Type)


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
    , constructors : List ( String, List Type )
    }
    -> Union
create params =
    Union
        { name = params.name
        , documentation = params.documentation
        , args = params.args
        , constructors = Dict.fromList params.constructors
        }


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


toElmDocs : Union -> Elm.Docs.Union
toElmDocs (Union union) =
    { name = union.name
    , comment = Maybe.withDefault "" union.documentation
    , args = union.args
    , tags =
        union.constructors
            |> Dict.toList
            |> List.map (\( name_, type_ ) -> ( name_, List.map (Type.toElmDocs >> Maybe.withDefault (Elm.Type.Var "unknown")) type_ ))
    }


name : Union -> String
name (Union union) =
    union.name


constructorsAsDict : Union -> Dict String (List Type)
constructorsAsDict (Union union) =
    union.constructors


args : Union -> List String
args (Union union) =
    union.args


documentation : Union -> Maybe String
documentation (Union union) =
    union.documentation
