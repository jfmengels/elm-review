module Elm.TypeInference.ModuleIndex exposing
    ( ModuleIndex, ImportIndex, ExposingIndex(..)
    , fromFile
    , importCouldExposeValue, importExposesType, importExposesValue
    , modulesWithAlias, isImportedUnaliased
    )

{-| Precomputed index for name resolution.

@docs ModuleIndex, ImportIndex, ExposingIndex
@docs fromFile
@docs importCouldExposeValue, importExposesType, importExposesValue
@docs modulesWithAlias, isImportedUnaliased

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..))
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Import
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern.Extra
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.TypeInference.Type exposing (VarName)
import Set exposing (Set)


type alias ModuleIndex =
    { moduleName : FullModuleName
    , dottedModuleName : String
    , declaredValues : Set VarName
    , declaredTypes : Set VarName
    , exposedValues : Set VarName
    , exposedTypes : Set VarName
    , unionConstructors : Dict VarName (List VarName)
    , recordAliases : Set VarName
    , infixes : Dict VarName VarName
    , imports : List ImportIndex
    }


type alias ImportIndex =
    { moduleName : FullModuleName
    , dottedModuleName : String
    , alias_ : Maybe String
    , exposing_ : ExposingIndex
    }


type ExposingIndex
    = ExposesNothing -- import Foo
    | ExposesAll -- import Foo exposing (..)
    | ExposesExplicit
        { values : Set VarName -- import Foo exposing (foo, bar)
        , types : Set VarName -- import Foo exposing (Foo, Bar(..))
        , hasOpenedUnion : Bool -- at least one of the exposed types has (..). Used to pre-filter in importCouldExposeValue.
        , openTypes : Set VarName -- import Foo exposing (Bar(..)): the types exposed with (..)
        , opaqueTypes : Set VarName -- import Foo exposing (Foo)
        }


fromFile : File -> ModuleIndex
fromFile file =
    let
        moduleName : FullModuleName
        moduleName =
            file.moduleDefinition
                |> Node.value
                |> Module.moduleName
                |> FullModuleName.fromModuleName_

        exposing_ : Exposing
        exposing_ =
            file.moduleDefinition
                |> Node.value
                |> Module.exposingList

        decls : Declarations
        decls =
            List.foldl
                (Node.value >> addDeclaration)
                emptyDeclarations
                file.declarations
    in
    { moduleName = moduleName
    , dottedModuleName = FullModuleName.toString moduleName
    , declaredValues = decls.values
    , declaredTypes = decls.types
    , exposedValues = exposedValues exposing_ decls
    , exposedTypes = exposedTypes exposing_ decls
    , unionConstructors = decls.unionConstructors
    , recordAliases = decls.recordAliases
    , infixes = decls.infixes
    , imports = List.map (Node.value >> importIndex) file.imports
    }



-- DECLARATIONS


type alias Declarations =
    { values : Set VarName
    , types : Set VarName
    , unionConstructors : Dict VarName (List VarName)
    , recordAliases : Set VarName
    , infixes : Dict VarName VarName
    }


emptyDeclarations : Declarations
emptyDeclarations =
    { values = Set.empty
    , types = Set.empty
    , unionConstructors = Dict.empty
    , recordAliases = Set.empty
    , infixes = Dict.empty
    }


addDeclaration : Declaration -> Declarations -> Declarations
addDeclaration decl acc =
    case decl of
        FunctionDeclaration fn ->
            { values = Set.insert (Elm.Syntax.Expression.Extra.functionName fn) acc.values
            , types = acc.types
            , unionConstructors = acc.unionConstructors
            , recordAliases = acc.recordAliases
            , infixes = acc.infixes
            }

        AliasDeclaration typeAlias ->
            let
                name : VarName
                name =
                    Node.value typeAlias.name

                isRecord : Bool
                isRecord =
                    case Node.value typeAlias.typeAnnotation of
                        TypeAnnotation.Record _ ->
                            True

                        _ ->
                            False
            in
            { values =
                if isRecord then
                    Set.insert name acc.values

                else
                    acc.values
            , types = Set.insert name acc.types
            , unionConstructors = acc.unionConstructors
            , recordAliases =
                if isRecord then
                    Set.insert name acc.recordAliases

                else
                    acc.recordAliases
            , infixes = acc.infixes
            }

        CustomTypeDeclaration customType ->
            let
                typeName : VarName
                typeName =
                    Node.value customType.name

                ctorNames : List VarName
                ctorNames =
                    List.map (\ctor -> Node.value (Node.value ctor).name) customType.constructors
            in
            { values = List.foldl Set.insert acc.values ctorNames
            , types = Set.insert typeName acc.types
            , unionConstructors =
                if Dict.member typeName acc.unionConstructors then
                    acc.unionConstructors

                else
                    Dict.insert typeName ctorNames acc.unionConstructors
            , recordAliases = acc.recordAliases
            , infixes = acc.infixes
            }

        PortDeclaration signature ->
            { values = Set.insert (Node.value signature.name) acc.values
            , types = acc.types
            , unionConstructors = acc.unionConstructors
            , recordAliases = acc.recordAliases
            , infixes = acc.infixes
            }

        InfixDeclaration infix ->
            let
                operator : VarName
                operator =
                    Node.value infix.operator
            in
            { values = Set.insert operator acc.values
            , types = acc.types
            , unionConstructors = acc.unionConstructors
            , recordAliases = acc.recordAliases
            , infixes =
                if Dict.member operator acc.infixes then
                    acc.infixes

                else
                    Dict.insert operator (Node.value infix.function) acc.infixes
            }

        Destructuring pattern _ ->
            { values = List.foldl Set.insert acc.values (Elm.Syntax.Pattern.Extra.varNames (Node.value pattern))
            , types = acc.types
            , unionConstructors = acc.unionConstructors
            , recordAliases = acc.recordAliases
            , infixes = acc.infixes
            }



-- EXPOSING


exposedValues : Exposing -> Declarations -> Set VarName
exposedValues exposing_ decls =
    case exposing_ of
        All _ ->
            decls.values

        Explicit exposedNodes ->
            exposedNodes
                |> List.foldl
                    (\exposedNode acc ->
                        case Node.value exposedNode of
                            Exposing.FunctionExpose fn ->
                                Set.insert fn acc

                            Exposing.InfixExpose op ->
                                Set.insert op acc

                            Exposing.TypeOrAliasExpose name ->
                                -- Only a record alias brings a value (its
                                -- constructor function) along with the type.
                                if Set.member name decls.recordAliases then
                                    Set.insert name acc

                                else
                                    acc

                            Exposing.TypeExpose exposedType ->
                                if exposedType.open /= Nothing then
                                    Dict.get exposedType.name decls.unionConstructors
                                        |> Maybe.withDefault []
                                        |> List.foldl Set.insert acc

                                else
                                    acc
                    )
                    Set.empty


exposedTypes : Exposing -> Declarations -> Set VarName
exposedTypes exposing_ decls =
    case exposing_ of
        All _ ->
            decls.types

        Explicit exposedNodes ->
            exposedNodes
                |> List.foldl
                    (\exposedNode acc ->
                        case Node.value exposedNode of
                            Exposing.TypeOrAliasExpose name ->
                                Set.insert name acc

                            Exposing.TypeExpose exposedType ->
                                Set.insert exposedType.name acc

                            _ ->
                                acc
                    )
                    Set.empty



-- IMPORTS


importIndex : Elm.Syntax.Import.Import -> ImportIndex
importIndex import_ =
    let
        moduleName : FullModuleName
        moduleName =
            FullModuleName.fromModuleName_ (Node.value import_.moduleName)
    in
    { moduleName = moduleName
    , dottedModuleName = FullModuleName.toString moduleName
    , alias_ =
        case Maybe.map Node.value import_.moduleAlias of
            Just [ single ] ->
                Just single

            _ ->
                Nothing
    , exposing_ =
        case Maybe.map Node.value import_.exposingList of
            Nothing ->
                ExposesNothing

            Just (All _) ->
                ExposesAll

            Just (Explicit exposedNodes) ->
                ExposesExplicit <|
                    List.foldl
                        (\exposedNode acc ->
                            case Node.value exposedNode of
                                Exposing.FunctionExpose fn ->
                                    { values = Set.insert fn acc.values
                                    , types = acc.types
                                    , hasOpenedUnion = acc.hasOpenedUnion
                                    , openTypes = acc.openTypes
                                    , opaqueTypes = acc.opaqueTypes
                                    }

                                Exposing.InfixExpose op ->
                                    { values = Set.insert op acc.values
                                    , types = acc.types
                                    , hasOpenedUnion = acc.hasOpenedUnion
                                    , openTypes = acc.openTypes
                                    , opaqueTypes = acc.opaqueTypes
                                    }

                                Exposing.TypeOrAliasExpose name ->
                                    { values = acc.values
                                    , types = Set.insert name acc.types
                                    , hasOpenedUnion = acc.hasOpenedUnion
                                    , openTypes = acc.openTypes
                                    , opaqueTypes = Set.insert name acc.opaqueTypes
                                    }

                                Exposing.TypeExpose exposedType ->
                                    { values = acc.values
                                    , types = Set.insert exposedType.name acc.types
                                    , hasOpenedUnion = acc.hasOpenedUnion || exposedType.open /= Nothing
                                    , openTypes =
                                        if exposedType.open /= Nothing then
                                            Set.insert exposedType.name acc.openTypes

                                        else
                                            acc.openTypes
                                    , opaqueTypes = acc.opaqueTypes
                                    }
                        )
                        { values = Set.empty
                        , types = Set.empty
                        , hasOpenedUnion = False
                        , openTypes = Set.empty
                        , opaqueTypes = Set.empty
                        }
                        exposedNodes
    }


{-| Could this import bring this value/operator into unqualified scope?

A pre-filter for speed (`True` doesn't mean "does expose" -- the target
module's `exposedValues` settles that).

-}
importCouldExposeValue : ImportIndex -> VarName -> Bool
importCouldExposeValue import_ varName =
    case import_.exposing_ of
        ExposesNothing ->
            False

        ExposesAll ->
            True

        ExposesExplicit e ->
            Set.member varName e.values
                || -- The `exposing (Foo(..))` doesn't say what's inside the `..` so we can't say for sure
                   (couldBeConstructorName varName
                        && (e.hasOpenedUnion || Set.member varName e.opaqueTypes)
                   )


{-| Does this import actually bring this value into unqualified scope?
-}
importExposesValue : ModuleIndex -> ImportIndex -> VarName -> Bool
importExposesValue target import_ varName =
    case import_.exposing_ of
        ExposesNothing ->
            False

        ExposesAll ->
            Set.member varName target.exposedValues

        ExposesExplicit e ->
            if Set.member varName e.values then
                Set.member varName target.exposedValues

            else if not (couldBeConstructorName varName) then
                False

            else
                let
                    viaOpenUnion : Bool
                    viaOpenUnion =
                        Set.member varName target.exposedValues
                            && List.any
                                (\openType ->
                                    case Dict.get openType target.unionConstructors of
                                        Just ctors ->
                                            List.member varName ctors

                                        Nothing ->
                                            False
                                )
                                (Set.toList e.openTypes)

                    viaRecordAlias : Bool
                    viaRecordAlias =
                        Set.member varName e.opaqueTypes
                            && Set.member varName target.recordAliases
                            && Set.member varName target.exposedValues
                in
                viaRecordAlias || viaOpenUnion


{-| Does this import's own `exposing` clause name this type?
-}
importExposesType : ImportIndex -> VarName -> Bool
importExposesType import_ typeName =
    case import_.exposing_ of
        ExposesNothing ->
            False

        ExposesAll ->
            True

        ExposesExplicit e ->
            Set.member typeName e.types


couldBeConstructorName : VarName -> Bool
couldBeConstructorName varName =
    case String.uncons varName of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


{-| Every module aliased to the given name, in import order.
-}
modulesWithAlias : ModuleIndex -> String -> List FullModuleName
modulesWithAlias index wantedAlias =
    index.imports
        |> List.filterMap
            (\import_ ->
                if import_.alias_ == Just wantedAlias then
                    Just import_.moduleName

                else
                    Nothing
            )


isImportedUnaliased : ModuleIndex -> ModuleName -> Bool
isImportedUnaliased index moduleName =
    List.any
        (\import_ ->
            FullModuleName.toModuleName import_.moduleName == moduleName && import_.alias_ == Nothing
        )
        index.imports
