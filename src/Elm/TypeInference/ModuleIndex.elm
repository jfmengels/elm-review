module Elm.TypeInference.ModuleIndex exposing
    ( ModuleIndex, ImportIndex, ExposingIndex(..)
    , fromFile
    , importCouldExposeValue, importExposesType, importExposesValue
    , modulesWithAlias, isImportedUnaliased
    , effectCommandVar, effectSubscriptionVar
    )

{-| Precomputed index for name resolution.

@docs ModuleIndex, ImportIndex, ExposingIndex
@docs fromFile
@docs importCouldExposeValue, importExposesType, importExposesValue
@docs modulesWithAlias, isImportedUnaliased
@docs effectCommandVar, effectSubscriptionVar

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
import Elm.Syntax.ModuleName.Extra as ModuleNameExtra
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern.Extra
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.Type exposing (VarName)
import Set exposing (Set)
import Set.Extra


type alias ModuleIndex =
    { moduleName : FullModuleName
    , moduleId : ModuleId
    , dottedModuleName : String
    , declaredValues : Set VarName
    , declaredTypes : Set VarName
    , exposedValues : Set VarName
    , exposedTypes : Set VarName
    , unionConstructors : Dict VarName (List VarName)
    , recordAliases : Set VarName
    , infixes : Dict VarName VarName
    , imports : List ImportIndex
    , importsByAlias : Dict String (List ModuleId)
    , unaliasedImports : Set String
    , effectCommand : Maybe VarName
    , effectSubscription : Maybe VarName
    }


type alias ImportIndex =
    { moduleName : FullModuleName
    , moduleId : ModuleId
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


fromFile : ModuleIds.Mapping -> File -> ( ModuleIndex, ModuleIds.Mapping )
fromFile moduleMapping file =
    let
        moduleName : FullModuleName
        moduleName =
            file.moduleDefinition
                |> Node.value
                |> Module.moduleName
                |> FullModuleName.fromModuleName_

        ( moduleId, moduleMapping1 ) =
            ModuleIds.intern moduleName moduleMapping

        exposing_ : Exposing
        exposing_ =
            file.moduleDefinition
                |> Node.value
                |> Module.exposingList

        decls : Declarations
        decls =
            List.foldl
                (\(Node.Node _ declaration) acc -> addDeclaration declaration acc)
                emptyDeclarations
                file.declarations

        ( effectCommand, effectSubscription ) =
            effectTypes file

        ( imports, moduleMapping2 ) =
            List.foldl
                (\importNode ( acc, accModuleMapping ) ->
                    let
                        ( importIndex_, newModuleMapping ) =
                            importIndex accModuleMapping (Node.value importNode)
                    in
                    ( importIndex_ :: acc, newModuleMapping )
                )
                ( [], moduleMapping1 )
                file.imports
                |> (\( reversed, finalModuleMapping ) -> ( List.reverse reversed, finalModuleMapping ))
    in
    ( { moduleName = moduleName
      , moduleId = moduleId
      , dottedModuleName = FullModuleName.toString moduleName
      , declaredValues =
            decls.values
                |> (case effectCommand of
                        Just _ ->
                            Set.insert effectCommandVar

                        Nothing ->
                            identity
                   )
                |> (case effectSubscription of
                        Just _ ->
                            Set.insert effectSubscriptionVar

                        Nothing ->
                            identity
                   )
      , declaredTypes = decls.types
      , exposedValues = exposedValues exposing_ decls
      , exposedTypes = exposedTypes exposing_ decls
      , unionConstructors = decls.unionConstructors
      , recordAliases = decls.recordAliases
      , infixes = decls.infixes
      , imports = imports
      , importsByAlias = importsByAlias imports
      , unaliasedImports = unaliasedImports imports
      , effectCommand = effectCommand
      , effectSubscription = effectSubscription
      }
    , moduleMapping2
    )


{-| Magic value names introduced by `effect module` headers.

    effect module Random where { command = MyCmd } exposing (..)

    --> `command : MyCmd msg -> Cmd msg` is available unqualified in Random

-}
effectCommandVar : VarName
effectCommandVar =
    "command"


effectSubscriptionVar : VarName
effectSubscriptionVar =
    "subscription"


{-| The custom type names from an `effect module` header, if any.

    effect module Http where { command = MyCmd, subscription = MySub } exposing (..)

    --> ( Just "MyCmd", Just "MySub" )

-}
effectTypes : File -> ( Maybe VarName, Maybe VarName )
effectTypes file =
    case Node.value file.moduleDefinition of
        Module.EffectModule { command, subscription } ->
            ( Maybe.map Node.value command
            , Maybe.map Node.value subscription
            )

        _ ->
            ( Nothing, Nothing )



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
                                case exposedType.open of
                                    Nothing ->
                                        acc

                                    Just _ ->
                                        Dict.get exposedType.name decls.unionConstructors
                                            |> Maybe.withDefault []
                                            |> List.foldl Set.insert acc
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


importIndex : ModuleIds.Mapping -> Elm.Syntax.Import.Import -> ( ImportIndex, ModuleIds.Mapping )
importIndex moduleMapping import_ =
    let
        moduleName : FullModuleName
        moduleName =
            FullModuleName.fromModuleName_ (Node.value import_.moduleName)

        ( moduleId, moduleMapping1 ) =
            ModuleIds.intern moduleName moduleMapping
    in
    ( { moduleName = moduleName
      , moduleId = moduleId
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
                                        , hasOpenedUnion =
                                            acc.hasOpenedUnion
                                                || (case exposedType.open of
                                                        Nothing ->
                                                            False

                                                        Just _ ->
                                                            True
                                                   )
                                        , openTypes =
                                            case exposedType.open of
                                                Just _ ->
                                                    Set.insert exposedType.name acc.openTypes

                                                Nothing ->
                                                    acc.openTypes
                                        , opaqueTypes = acc.opaqueTypes
                                        }
                            )
                            explicitExposingIndexEmpty
                            exposedNodes
      }
    , moduleMapping1
    )


explicitExposingIndexEmpty :
    { values : Set VarName
    , types : Set VarName
    , hasOpenedUnion : Bool
    , openTypes : Set VarName
    , opaqueTypes : Set VarName
    }
explicitExposingIndexEmpty =
    { values = Set.empty
    , types = Set.empty
    , hasOpenedUnion = False
    , openTypes = Set.empty
    , opaqueTypes = Set.empty
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
                    viaOpenUnion : () -> Bool
                    viaOpenUnion () =
                        Set.Extra.any
                            (\openType ->
                                case Dict.get openType target.unionConstructors of
                                    Just ctors ->
                                        List.member varName ctors

                                    Nothing ->
                                        False
                            )
                            e.openTypes

                    viaRecordAlias : Bool
                    viaRecordAlias =
                        Set.member varName e.opaqueTypes
                            && Set.member varName target.recordAliases
                in
                Set.member varName target.exposedValues
                    && (viaRecordAlias || viaOpenUnion ())


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


{-| Alias -> modules imported under it, in import order.
-}
importsByAlias : List ImportIndex -> Dict String (List ModuleId)
importsByAlias imports =
    List.foldl
        (\import_ acc ->
            case import_.alias_ of
                Just alias ->
                    Dict.update alias
                        (\maybeModules ->
                            Just (Maybe.withDefault [] maybeModules ++ [ import_.moduleId ])
                        )
                        acc

                Nothing ->
                    acc
        )
        Dict.empty
        imports


{-| Dotted names of unaliased imports.
-}
unaliasedImports : List ImportIndex -> Set String
unaliasedImports imports =
    List.foldl
        (\import_ acc ->
            case import_.alias_ of
                Nothing ->
                    Set.insert import_.dottedModuleName acc

                Just _ ->
                    acc
        )
        Set.empty
        imports


{-| Every module aliased to the given name, in import order.
-}
modulesWithAlias : ModuleIndex -> String -> List ModuleId
modulesWithAlias index wantedAlias =
    Dict.get wantedAlias index.importsByAlias
        |> Maybe.withDefault []


isImportedUnaliased : ModuleIndex -> ModuleName -> Bool
isImportedUnaliased index moduleName =
    Set.member (ModuleNameExtra.toString moduleName) index.unaliasedImports
