module TypeInference.Type exposing (Type(..), fromMetadataType, relateToModule, toMetadataType)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type


type Type
    = Unknown
    | Generic String
    | Function Type Type
    | Tuple (List Type)
    | Type ModuleName String (List Type)
    | Record
        { fields : List ( String, Type )
        , generic : Maybe String
        , canHaveMoreFields : Bool
        }


fromMetadataType : Elm.Type.Type -> Type
fromMetadataType type_ =
    case type_ of
        Elm.Type.Var string ->
            Generic string

        Elm.Type.Lambda input output ->
            Function (fromMetadataType input) (fromMetadataType output)

        Elm.Type.Tuple types ->
            Tuple (List.map fromMetadataType types)

        Elm.Type.Type name originalTypes ->
            let
                types : List Type
                types =
                    List.map fromMetadataType originalTypes
            in
            case String.split "." name |> List.reverse of
                [] ->
                    Type [] name types

                typeName :: reversedModuleName ->
                    Type (List.reverse reversedModuleName) typeName types

        Elm.Type.Record originalFields generic ->
            Record
                { fields = List.map (Tuple.mapSecond fromMetadataType) originalFields
                , generic = generic
                , canHaveMoreFields = False
                }


toMetadataType : Type -> Maybe Elm.Type.Type
toMetadataType type_ =
    case type_ of
        Unknown ->
            Nothing

        Generic generic ->
            Just (Elm.Type.Var generic)

        Tuple types ->
            listOfMaybeToMaybeList types
                |> Maybe.map Elm.Type.Tuple

        Function input output ->
            case toMetadataType input of
                Just inputType ->
                    case toMetadataType output of
                        Just outputType ->
                            Just (Elm.Type.Lambda inputType outputType)

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Type moduleName typeName types ->
            case listOfMaybeToMaybeList types of
                Just argumentTypes ->
                    Just (Elm.Type.Type (String.join "." (moduleName ++ [ typeName ])) argumentTypes)

                Nothing ->
                    Nothing

        Record { fields, generic, canHaveMoreFields } ->
            if canHaveMoreFields then
                Nothing

            else
                let
                    fieldsFoo : ( String, Type ) -> Maybe ( String, Elm.Type.Type )
                    fieldsFoo ( name, field ) =
                        case toMetadataType field of
                            Just fieldType ->
                                Just ( name, fieldType )

                            Nothing ->
                                Nothing

                    fieldTypes : List ( String, Elm.Type.Type )
                    fieldTypes =
                        List.filterMap fieldsFoo fields
                in
                if List.length fieldTypes == List.length fields then
                    Just (Elm.Type.Record fieldTypes generic)

                else
                    Nothing


relateToModule : ModuleName -> Type -> Type
relateToModule moduleName type_ =
    case type_ of
        Type originalModuleName name types ->
            let
                moduleNameToUse : ModuleName
                moduleNameToUse =
                    if originalModuleName == [] then
                        moduleName

                    else
                        originalModuleName
            in
            Type moduleNameToUse name (List.map (relateToModule moduleName) types)

        Unknown ->
            type_

        Generic _ ->
            type_

        Function input output ->
            Function
                (relateToModule moduleName input)
                (relateToModule moduleName output)

        Tuple types ->
            Tuple (List.map (relateToModule moduleName) types)

        Record record ->
            Record { record | fields = List.map (Tuple.mapSecond (relateToModule moduleName)) record.fields }


listOfMaybeToMaybeList : List Type -> Maybe (List Elm.Type.Type)
listOfMaybeToMaybeList types =
    let
        elements : List Elm.Type.Type
        elements =
            List.filterMap toMetadataType types
    in
    if List.length elements == List.length types then
        Just elements

    else
        Nothing
