module Review.Type.Value exposing
    ( Value
    , create
    , documentation
    , fromAlias
    , fromMetadataAlias
    , fromMetadataUnion
    , fromMetadataValue
    , fromUnion
    , name
    , relateToModule
    , tipe
    , toMetadataValue
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Review.Type as Type exposing (Type(..))
import Review.Type.Alias as Alias exposing (Alias)
import Review.Type.Union as Union exposing (Union)


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


toMetadataValue : Value -> Maybe Elm.Docs.Value
toMetadataValue (Value value) =
    if wasDeclaredAsAFunction value.name then
        Type.toMetadataType value.tipe
            |> Maybe.map
                (\tipe_ ->
                    { name = value.name
                    , comment = value.documentation
                    , tipe = tipe_
                    }
                )

    else
        Nothing


wasDeclaredAsAFunction : String -> Bool
wasDeclaredAsAFunction name_ =
    case String.uncons name_ of
        Just ( firstChar, _ ) ->
            Char.isLower firstChar

        Nothing ->
            False


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


fromUnion : ModuleName -> Union -> List Value
fromUnion moduleName union =
    let
        documentation_ : String
        documentation_ =
            Union.documentation union
    in
    List.map
        (\( constructorName, types ) ->
            Value
                { name = constructorName
                , documentation = documentation_
                , tipe =
                    List.foldl
                        (\input output -> Type.Function input output)
                        (Type.Type moduleName (Union.name union) (List.map Type.Generic (Union.args union)))
                        types
                }
        )
        (Union.constructors union)


fromMetadataAlias : ModuleName -> Elm.Docs.Alias -> Maybe Value
fromMetadataAlias moduleName alias_ =
    -- TODO Need to create a test for this
    case alias_.tipe of
        Elm.Type.Record fields _ ->
            if List.isEmpty alias_.args then
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

            else
                Nothing

        _ ->
            Nothing


fromAlias : ModuleName -> Alias -> Maybe Value
fromAlias moduleName alias =
    -- TODO Need to create a test for this
    case Alias.tipe alias of
        Record { fields } ->
            if List.isEmpty (Alias.args alias) then
                Just
                    (Value
                        { name = Alias.name alias
                        , documentation = Alias.documentation alias
                        , tipe =
                            List.foldl
                                (\( _, input ) output -> Type.Function input output)
                                (Type.Type moduleName (Alias.name alias) (List.map Type.Generic (Alias.args alias)))
                                fields
                        }
                    )

            else
                Nothing

        _ ->
            Nothing


name : Value -> String
name (Value value) =
    value.name


documentation : Value -> String
documentation (Value value) =
    value.documentation


tipe : Value -> Type.Type
tipe (Value value) =
    value.tipe
