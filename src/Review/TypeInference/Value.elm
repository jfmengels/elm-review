module Review.TypeInference.Value exposing
    ( Value
    , comment
    , create
    , fromMetadataAlias
    , fromMetadataUnion
    , fromMetadataValue
    , name
    , relateToModule
    , tipe
    )

import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Review.TypeInference.Type as Type


type Value
    = Value
        { name : String
        , documentation : String
        , tipe : Type.Type
        }


create : { name : String, documentation : String, tipe : Type.Type } -> Value
create =
    Value


relateToModule : ModuleName -> Value -> Value
relateToModule moduleName (Value value) =
    Value { value | tipe = Type.relateToModule moduleName value.tipe }


fromMetadataValue : Elm.Docs.Value -> Value
fromMetadataValue value =
    Value
        { name = value.name
        , documentation = value.comment
        , tipe = Type.fromMetadataType value.tipe
        }


fromMetadataUnion : ModuleName -> Elm.Docs.Union -> List Value
fromMetadataUnion moduleName customType =
    List.map
        (\( name_, types ) ->
            Value
                { name = name_
                , documentation = customType.comment
                , tipe =
                    List.foldl
                        (\input output -> Type.Function (Type.fromMetadataType input) output)
                        (Type.Type moduleName customType.name (List.map Type.Generic customType.args))
                        types
                }
        )
        customType.tags


fromMetadataAlias : ModuleName -> Elm.Docs.Alias -> Maybe Value
fromMetadataAlias moduleName alias_ =
    -- TODO Need to create a test for this
    case alias_.tipe of
        Elm.Type.Record fields _ ->
            Just
                (Value
                    { name = alias_.name
                    , documentation = alias_.comment
                    , tipe =
                        List.foldl
                            (\( _, input ) output -> Type.Function (Type.fromMetadataType input) output)
                            (Type.Type moduleName alias_.name (List.map Type.Generic alias_.args))
                            fields
                    }
                )

        _ ->
            Nothing


name : Value -> String
name (Value value) =
    value.name


comment : Value -> String
comment (Value value) =
    value.documentation


tipe : Value -> Type.Type
tipe (Value value) =
    value.tipe
