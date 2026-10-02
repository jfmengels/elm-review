module Elm.TypeInference.Type exposing
    ( Type(..), toString, toMultilineString, toTypeAnnotation
    , PackageName, VarName
    )

{-| A data structure representing the Elm types.

_Note:_ This module is not named `Elm.Type` because that already exists in `elm/project-metadata-utils`.

@docs Type, toString, toMultilineString, toTypeAnnotation
@docs PackageName, VarName

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra
import Elm.Syntax.Node as Node
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


{-| Eg. `"elm/core"`.
Comes from `elm.json` dependency keys.

Empty string for the user's project.

-}
type alias PackageName =
    String


{-| Eg. `"foobar"` in `\foobar -> foobar + 1`.
-}
type alias VarName =
    String


{-| The inferred type.
-}
type Type
    = TypeVar String
    | Function
        { from : Type
        , to : Type
        }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List Type
    | Unit
    | Tuple2 Type Type
    | Tuple3 Type Type Type
    | Record { fields : Dict String Type }
    | ExtensibleRecord
        { fields : Dict String Type
        , extensionTypevar : String
        }
    | Named
        { package : PackageName
        , moduleName : ModuleName
        , name : String
        , arguments : List Type
        }
    | WebGLShader
        { attributesFields : Dict String Type
        , attributesExtensionTypevar : Maybe String
        , uniformsFields : Dict String Type
        , uniformsExtensionTypevar : Maybe String
        , varyingsFields : Dict String Type
        , varyingsExtensionTypevar : Maybe String
        }


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously
as an argument of a type constructor application.
-}
wrapped : Type -> String
wrapped t =
    case t of
        Function _ ->
            paren t

        List _ ->
            paren t

        WebGLShader _ ->
            paren t

        Named r ->
            if List.isEmpty r.arguments then
                toString t

            else
                paren t

        _ ->
            toString t


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously on the
left of `->`.

`->` is right-associative and type application binds tighter, so only a nested
`->` needs parens there: `List a -> b` already parses as `(List a) -> b`.

-}
wrappedFrom : Type -> String
wrappedFrom t =
    case t of
        Function _ ->
            paren t

        _ ->
            toString t


paren : Type -> String
paren t =
    "(" ++ toString t ++ ")"


{-| Display a type.

    toString (Function { from = Int, to = TypeVar "a" })
    --> "Int -> a"

-}
toString : Type -> String
toString t =
    case t of
        TypeVar name ->
            name

        Function { from, to } ->
            wrappedFrom from ++ " -> " ++ toString to

        Int ->
            "Int"

        Float ->
            "Float"

        Char ->
            "Char"

        String ->
            "String"

        Bool ->
            "Bool"

        List inner ->
            "List " ++ wrapped inner

        Unit ->
            "()"

        Tuple2 a b ->
            "( " ++ toString a ++ ", " ++ toString b ++ " )"

        Tuple3 a b c ->
            "( " ++ toString a ++ ", " ++ toString b ++ ", " ++ toString c ++ " )"

        Record { fields } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString fieldType)
            in
            "{" ++ String.join ", " fieldStrings ++ "}"

        ExtensibleRecord { fields, extensionTypevar } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString fieldType)
            in
            "{ " ++ extensionTypevar ++ " | " ++ String.join ", " fieldStrings ++ " }"

        Named { moduleName, name, arguments } ->
            let
                argStrings : List String
                argStrings =
                    arguments
                        |> List.map wrapped

                qualifiedName : String
                qualifiedName =
                    Elm.Syntax.ModuleName.Extra.toString moduleName
                        ++ "."
                        ++ name
            in
            (qualifiedName
                :: argStrings
            )
                |> String.join " "

        WebGLShader r ->
            [ "Shader"
            , shaderSlotToString r.attributesFields r.attributesExtensionTypevar
            , shaderSlotToString r.uniformsFields r.uniformsExtensionTypevar
            , shaderSlotToString r.varyingsFields r.varyingsExtensionTypevar
            ]
                |> String.join " "


{-| Display a type, breaking it to new lines when the full type is longer than
`maxWidth` chars.

Short types stay on one line:

    a -> b

Long `->` chains break one arrow per line:

    Foo bar baz
    -> Foo bar baz
    -> Foo bar baz

Long records break one field per line:

     { a : Int
     , b : String
     }

-}
toMultilineString : Int -> Type -> String
toMultilineString maxWidth t =
    if String.length (toString t) <= maxWidth then
        toString t

    else
        breakType maxWidth t


{-| Assumes `toString t` is longer than `maxWidth`.
-}
breakType : Int -> Type -> String
breakType maxWidth t =
    case t of
        Function _ ->
            let
                ( args, result ) =
                    flattenFunction t
            in
            (List.map (\arg -> arg |> renderFromPart maxWidth) args
                ++ [ toMultilineString maxWidth result ]
            )
                |> String.join "\n-> "

        Record { fields } ->
            breakRecordFields
                maxWidth
                Nothing
                (Dict.toList fields)

        ExtensibleRecord { fields, extensionTypevar } ->
            breakRecordFields
                maxWidth
                (Just extensionTypevar)
                (Dict.toList fields)

        List inner ->
            "List " ++ wrappedMultiline maxWidth inner

        Tuple2 a b ->
            "( "
                ++ toMultilineString maxWidth a
                ++ ", "
                ++ toMultilineString maxWidth b
                ++ " )"

        Tuple3 a b c ->
            "( "
                ++ toMultilineString maxWidth a
                ++ ", "
                ++ toMultilineString maxWidth b
                ++ ", "
                ++ toMultilineString maxWidth c
                ++ " )"

        Named { moduleName, name, arguments } ->
            let
                qualifiedName : String
                qualifiedName =
                    Elm.Syntax.ModuleName.Extra.toString moduleName
                        ++ "."
                        ++ name
            in
            (qualifiedName :: List.map (\arg -> arg |> wrappedMultiline maxWidth) arguments)
                |> String.join " "

        WebGLShader r ->
            [ "Shader"
            , toMultilineString maxWidth (shaderSlotToType r.attributesFields r.attributesExtensionTypevar)
            , toMultilineString maxWidth (shaderSlotToType r.uniformsFields r.uniformsExtensionTypevar)
            , toMultilineString maxWidth (shaderSlotToType r.varyingsFields r.varyingsExtensionTypevar)
            ]
                |> String.join " "

        _ ->
            toString t


{-| Split a right-nested chain, eg.

    Function { from = _1, to = Function { from = _2, to = Function ... } }
    --> _1
        -> _2
        -> ...

-}
flattenFunction : Type -> ( List Type, Type )
flattenFunction t =
    case t of
        Function { from, to } ->
            let
                ( args, result ) =
                    flattenFunction to
            in
            ( from :: args, result )

        _ ->
            ( [], t )


{-| Render one `->` argument, keeping Function parens
-}
renderFromPart : Int -> Type -> String
renderFromPart maxWidth t =
    case t of
        Function _ ->
            "(" ++ toMultilineString maxWidth t ++ ")"

        _ ->
            toMultilineString maxWidth t


wrappedMultiline : Int -> Type -> String
wrappedMultiline maxWidth t =
    case t of
        Function _ ->
            "(" ++ toMultilineString maxWidth t ++ ")"

        List _ ->
            "(" ++ toMultilineString maxWidth t ++ ")"

        WebGLShader _ ->
            "(" ++ toMultilineString maxWidth t ++ ")"

        Named r ->
            if List.isEmpty r.arguments then
                toString t

            else
                "(" ++ toMultilineString maxWidth t ++ ")"

        _ ->
            toMultilineString maxWidth t


breakRecordFields : Int -> Maybe String -> List ( String, Type ) -> String
breakRecordFields maxWidth extensionTypevar fields =
    case fields of
        [] ->
            toString
                (case extensionTypevar of
                    Nothing ->
                        Record { fields = Dict.empty }

                    Just var ->
                        ExtensibleRecord { fields = Dict.empty, extensionTypevar = var }
                )

        ( firstName, firstType ) :: rest ->
            let
                firstLine : String
                firstLine =
                    case extensionTypevar of
                        Nothing ->
                            "{ " ++ firstName ++ " : " ++ toMultilineString maxWidth firstType

                        Just var ->
                            "{ " ++ var ++ " | " ++ firstName ++ " : " ++ toMultilineString maxWidth firstType

                restLines : List String
                restLines =
                    List.map (\( name, fieldType ) -> "\n, " ++ name ++ " : " ++ toMultilineString maxWidth fieldType) rest
            in
            firstLine ++ String.concat restLines ++ "\n}"


{-| Convert a `Type` to an `elm-syntax` `TypeAnnotation`.

All `Node`s use dummy ranges.

-}
toTypeAnnotation : Type -> TypeAnnotation
toTypeAnnotation type_ =
    case type_ of
        TypeVar name ->
            TypeAnnotation.GenericType name

        Function { from, to } ->
            TypeAnnotation.FunctionTypeAnnotation
                (Node.empty (toTypeAnnotation from))
                (Node.empty (toTypeAnnotation to))

        Int ->
            typeAnnotationBasicsInt

        Float ->
            typeAnnotationBasicsFloat

        Char ->
            typeAnnotationCharChar

        String ->
            typeAnnotationStringString

        Bool ->
            typeAnnotationBasicsBool

        List itemType ->
            TypeAnnotation.Typed listListNameNode
                [ Node.empty (toTypeAnnotation itemType) ]

        Unit ->
            TypeAnnotation.Unit

        Tuple2 t1 t2 ->
            TypeAnnotation.Tupled
                [ Node.empty (toTypeAnnotation t1)
                , Node.empty (toTypeAnnotation t2)
                ]

        Tuple3 t1 t2 t3 ->
            TypeAnnotation.Tupled
                [ Node.empty (toTypeAnnotation t1)
                , Node.empty (toTypeAnnotation t2)
                , Node.empty (toTypeAnnotation t3)
                ]

        Record { fields } ->
            TypeAnnotation.Record
                (recordFieldsToRecordDefinition fields)

        ExtensibleRecord { fields, extensionTypevar } ->
            TypeAnnotation.GenericRecord
                (Node.empty extensionTypevar)
                (Node.empty (recordFieldsToRecordDefinition fields))

        Named { moduleName, name, arguments } ->
            TypeAnnotation.Typed
                (Node.empty ( moduleName, name ))
                (List.map (\arg -> Node.empty (toTypeAnnotation arg)) arguments)

        WebGLShader r ->
            TypeAnnotation.Typed
                (Node.empty ( [ "WebGL" ], "Shader" ))
                [ Node.empty <| shaderSlotToTypeAnnotation r.attributesFields r.attributesExtensionTypevar
                , Node.empty <| shaderSlotToTypeAnnotation r.uniformsFields r.uniformsExtensionTypevar
                , Node.empty <| shaderSlotToTypeAnnotation r.varyingsFields r.varyingsExtensionTypevar
                ]


typeAnnotationBasicsInt : TypeAnnotation
typeAnnotationBasicsInt =
    TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Int" )) []


typeAnnotationBasicsFloat : TypeAnnotation
typeAnnotationBasicsFloat =
    TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Float" )) []


typeAnnotationBasicsBool : TypeAnnotation
typeAnnotationBasicsBool =
    TypeAnnotation.Typed (Node.empty ( [ "Basics" ], "Bool" )) []


typeAnnotationCharChar : TypeAnnotation
typeAnnotationCharChar =
    TypeAnnotation.Typed (Node.empty ( [ "Char" ], "Char" )) []


typeAnnotationStringString : TypeAnnotation
typeAnnotationStringString =
    TypeAnnotation.Typed (Node.empty ( [ "String" ], "String" )) []


listListNameNode : Node.Node ( ModuleName, String )
listListNameNode =
    Node.empty ( [ "List" ], "List" )


recordFieldsToRecordDefinition : Dict String Type -> TypeAnnotation.RecordDefinition
recordFieldsToRecordDefinition fields =
    fields
        |> Dict.foldr
            (\fieldName fieldType acc ->
                Node.empty
                    ( Node.empty fieldName
                    , Node.empty (toTypeAnnotation fieldType)
                    )
                    :: acc
            )
            []


shaderSlotToType : Dict String Type -> Maybe String -> Type
shaderSlotToType fields extensionTypevar =
    case extensionTypevar of
        Nothing ->
            Record { fields = fields }

        Just var ->
            if Dict.isEmpty fields then
                TypeVar var

            else
                ExtensibleRecord { fields = fields, extensionTypevar = var }


shaderSlotToString : Dict String Type -> Maybe String -> String
shaderSlotToString fields extensionTypevar =
    toString (shaderSlotToType fields extensionTypevar)


shaderSlotToTypeAnnotation : Dict String Type -> Maybe String -> TypeAnnotation
shaderSlotToTypeAnnotation fields extensionTypevar =
    toTypeAnnotation (shaderSlotToType fields extensionTypevar)
