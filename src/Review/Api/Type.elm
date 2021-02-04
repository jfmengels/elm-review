module Review.Api.Type exposing (Type(..), fromElmDocs, relateToModule, toElmDocs)

-- TODO Expose, consider what to expose
{- TODO Add a function to "expand", with knowledge of type alias?
   Same thing for type aliases that are aliases to other types. `type alias A = B


    type alias A = { a : Int }
    someType : A
    -- type of someType: Type [] A []
    -- Expanded someType: Record { fields = [ ( "a", Type [ "Basics" ] "Int" [] ) ], generic = Nothing, mayHaveMoreFields = False }

-}

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

        -- TODO Decide whether this is something that we want to expose or not
        , mayHaveMoreFields : Bool
        }


fromElmDocs : Elm.Type.Type -> Type
fromElmDocs type_ =
    case type_ of
        Elm.Type.Var string ->
            Generic string

        Elm.Type.Lambda input output ->
            Function (fromElmDocs input) (fromElmDocs output)

        Elm.Type.Tuple types ->
            Tuple (List.map fromElmDocs types)

        Elm.Type.Type name originalTypes ->
            let
                types : List Type
                types =
                    List.map fromElmDocs originalTypes
            in
            case String.split "." name |> List.reverse of
                [] ->
                    Type [] name types

                typeName :: reversedModuleName ->
                    Type (List.reverse reversedModuleName) typeName types

        Elm.Type.Record originalFields generic ->
            Record
                { fields = List.map (Tuple.mapSecond fromElmDocs) originalFields
                , generic = generic
                , mayHaveMoreFields = False
                }


toElmDocs : Type -> Maybe Elm.Type.Type
toElmDocs type_ =
    case type_ of
        Unknown ->
            Nothing

        Generic generic ->
            Just (Elm.Type.Var generic)

        Tuple types ->
            listOfMaybeToMaybeList types
                |> Maybe.map Elm.Type.Tuple

        Function input output ->
            case toElmDocs input of
                Just inputType ->
                    case toElmDocs output of
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

        Record { fields, generic, mayHaveMoreFields } ->
            if mayHaveMoreFields then
                Nothing

            else
                let
                    fieldsFoo : ( String, Type ) -> Maybe ( String, Elm.Type.Type )
                    fieldsFoo ( name, field ) =
                        case toElmDocs field of
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
            List.filterMap toElmDocs types
    in
    if List.length elements == List.length types then
        Just elements

    else
        Nothing
