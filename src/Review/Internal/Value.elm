module Review.Internal.Value exposing
    ( Value
    , create
    , documentation
    , fromAlias
    , fromElmDocs
    , fromUnion
    , name
    , relateToModule
    , tipe
    , toElmDocs
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
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


fromElmDocs : Elm.Docs.Value -> Value
fromElmDocs value =
    Value
        { name = value.name
        , documentation = value.comment
        , tipe = Type.fromElmDocs value.tipe
        }


toElmDocs : Value -> Maybe Elm.Docs.Value
toElmDocs (Value value) =
    if wasDeclaredAsAFunction value.name then
        Type.toElmDocs value.tipe
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
