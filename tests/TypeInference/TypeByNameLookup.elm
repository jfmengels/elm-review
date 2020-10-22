module TypeInference.TypeByNameLookup exposing
    ( TypeByNameLookup
    , addNewScope
    , addType
    , byName
    , empty
    )

import Dict exposing (Dict)
import TypeInference.Type exposing (Type)


type TypeByNameLookup
    = TypeByNameLookup
        { typeDict : Dict String Type
        , scopes : List (Dict String Type)
        }


empty : TypeByNameLookup
empty =
    TypeByNameLookup { typeDict = Dict.empty, scopes = [] }


addType : List ( String, Type ) -> TypeByNameLookup -> TypeByNameLookup
addType types (TypeByNameLookup lookup) =
    TypeByNameLookup
        { lookup
            | typeDict =
                Dict.union
                    (Dict.fromList types)
                    lookup.typeDict
        }


addNewScope : TypeByNameLookup -> TypeByNameLookup
addNewScope (TypeByNameLookup lookup) =
    TypeByNameLookup
        { typeDict = Dict.empty
        , scopes = lookup.typeDict :: lookup.scopes
        }


byName : TypeByNameLookup -> String -> Maybe Type
byName (TypeByNameLookup lookup) name =
    lookupTypeByNameInternal name (lookup.typeDict :: lookup.scopes)


lookupTypeByNameInternal : String -> List (Dict String Type) -> Maybe Type
lookupTypeByNameInternal name lookupTables =
    case lookupTables of
        [] ->
            Nothing

        lookupTable :: restOfLookupTables ->
            case Dict.get name lookupTable of
                Just type_ ->
                    Just type_

                Nothing ->
                    lookupTypeByNameInternal name restOfLookupTables
