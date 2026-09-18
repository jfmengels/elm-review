module Elm.TypeInference.Dependencies exposing
    ( Dependencies
    , DependencyPackage
    , Resolver
    , fromList
    , register
    )

{-| Dependency types from docs.json.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.FullModuleName as FullModuleName
import Elm.Syntax.ModuleName.Extra as ModuleNameExtra
import Elm.Type
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType(..))
import Elm.TypeInference.TypeVar as TypeVar
import Elm.TypeInference.Unify exposing (TypeAlias)
import Result.Extra


type alias DependencyPackage =
    { name : PackageName
    , dependencies : List PackageName
    , modules : List Elm.Docs.Module
    }


type alias Dependencies =
    Dict PackageName DependencyPackage


fromList : List DependencyPackage -> Dependencies
fromList packages =
    packages
        |> List.foldl (\pkg acc -> Dict.insert pkg.name pkg acc)
            Dict.empty


{-| Resolves a module name from docs.json to its package.
-}
type alias Resolver =
    String -> Result ErrorDetails ( PackageName, ModuleId )


resolverFor : ModuleIds.Mapping -> Dependencies -> PackageName -> Resolver
resolverFor moduleMapping deps selfPackage =
    let
        searchOrder : List PackageName
        searchOrder =
            selfPackage
                :: (Dict.get selfPackage deps
                        |> Maybe.map .dependencies
                        |> Maybe.withDefault []
                   )

        addModule : PackageName -> Elm.Docs.Module -> Dict String (List PackageName) -> Dict String (List PackageName)
        addModule pkgName mod acc =
            Dict.update mod.name
                (\existing -> Just (Maybe.withDefault [] existing ++ [ pkgName ]))
                acc

        ownersByModule : Dict String (List PackageName)
        ownersByModule =
            searchOrder
                |> List.foldl
                    (\pkgName accAcrossPks ->
                        Dict.get pkgName deps
                            |> Maybe.map
                                (\pkg ->
                                    List.foldl (\mod acc -> addModule pkgName mod acc) accAcrossPks pkg.modules
                                )
                            |> Maybe.withDefault accAcrossPks
                    )
                    Dict.empty

        moduleIdOf : String -> Result ErrorDetails ModuleId
        moduleIdOf dotted =
            if String.isEmpty dotted then
                -- Impossible in principle (Elm compiler generates docs.json with fully qualified types).
                -- Possible in practice (if somebody hand-crafts a docs.json file).
                Err
                    (AmbiguousModuleOwner
                        { moduleName = dotted
                        , possiblePackages = []
                        }
                    )

            else
                case ModuleIds.getIdByDotted dotted moduleMapping of
                    Just moduleId ->
                        Ok moduleId

                    Nothing ->
                        -- Impossible if we pre-intern docs modules properly.
                        -- Possible if we have a bug.
                        Err
                            (AmbiguousModuleOwner
                                { moduleName = dotted
                                , possiblePackages = []
                                }
                            )
    in
    \moduleNameStr ->
        moduleIdOf moduleNameStr
            |> Result.andThen
                (\moduleId ->
                    case Dict.get moduleNameStr ownersByModule |> Maybe.withDefault [] of
                        [] ->
                            Ok ( selfPackage, moduleId )

                        [ owner ] ->
                            Ok ( owner, moduleId )

                        matches ->
                            Err <|
                                AmbiguousModuleOwner
                                    { moduleName = moduleNameStr
                                    , possiblePackages = matches
                                    }
                )


fromDocsType : Resolver -> Elm.Type.Type -> Result ErrorDetails MonoType
fromDocsType resolver type_ =
    case type_ of
        Elm.Type.Var name ->
            Ok (TypeVar (TypeVar.parse name))

        Elm.Type.Lambda from to ->
            Result.map2 (\f t -> Function { from = f, to = t })
                (fromDocsType resolver from)
                (fromDocsType resolver to)

        Elm.Type.Tuple [] ->
            Ok Unit

        Elm.Type.Tuple [ a, b ] ->
            Result.map2 Tuple2
                (fromDocsType resolver a)
                (fromDocsType resolver b)

        Elm.Type.Tuple [ a, b, c ] ->
            Result.map3 Tuple3
                (fromDocsType resolver a)
                (fromDocsType resolver b)
                (fromDocsType resolver c)

        Elm.Type.Tuple _ ->
            Err (ImpossibleDocsType type_)

        Elm.Type.Type qualifiedName args ->
            let
                ( moduleNameStr, typeName ) =
                    ModuleNameExtra.splitLastDot qualifiedName
            in
            Result.andThen
                (\( package, moduleId ) ->
                    Result.Extra.combineMap (\arg -> fromDocsType resolver arg) args
                        |> Result.map
                            (\argTypes ->
                                case TypeI.collapsePrimitive package moduleId typeName argTypes of
                                    Just collapsed ->
                                        collapsed

                                    Nothing ->
                                        UserDefinedType
                                            { package = package
                                            , moduleId = moduleId
                                            , name = typeName
                                            , args = argTypes
                                            }
                            )
                )
                (resolver moduleNameStr)

        Elm.Type.Record fields Nothing ->
            fromDocsFields resolver fields
                |> Result.map (\fields_ -> Record { fields = Dict.fromList fields_ })

        Elm.Type.Record fields (Just rowVar) ->
            fromDocsFields resolver fields
                |> Result.map
                    (\resolvedFields ->
                        ExtensibleRecord
                            { extensionTypevar = TypeVar (TypeVar.parse rowVar)
                            , fields = Dict.fromList resolvedFields
                            }
                    )


fromDocsFields : Resolver -> List ( String, Elm.Type.Type ) -> Result ErrorDetails (List ( String, MonoType ))
fromDocsFields resolver fields =
    Result.Extra.combineMap
        (\( name, value ) ->
            fromDocsType resolver value
                |> Result.map (\valueType -> ( name, valueType ))
        )
        fields


register : ModuleIds.Mapping -> Dependencies -> StateM ( Dict ( ModuleId, PackageName, VarName ) TypeAlias, ModuleIds.Mapping )
register moduleMapping deps =
    let
        moduleMapping1 : ModuleIds.Mapping
        moduleMapping1 =
            deps
                |> Dict.foldl
                    (\_ item accAcrossDeps ->
                        item.modules
                            |> List.foldl
                                (\mod acc ->
                                    ModuleIds.intern (FullModuleName.fromDotted mod.name) acc
                                        |> Tuple.second
                                )
                                accAcrossDeps
                    )
                    moduleMapping
    in
    deps
        |> Dict.toList
        |> State.foldl
            (\( pkgName, pkg ) dict ->
                State.map (\registeredPackage -> Dict.union registeredPackage dict)
                    (registerPackage moduleMapping1 deps pkgName pkg)
            )
            Dict.empty
        |> State.map (\dict -> ( dict, moduleMapping1 ))


registerPackage :
    ModuleIds.Mapping
    -> Dependencies
    -> PackageName
    -> DependencyPackage
    -> StateM (Dict ( ModuleId, PackageName, VarName ) TypeAlias)
registerPackage moduleMapping deps pkgName pkg =
    let
        resolver : Resolver
        resolver =
            resolverFor moduleMapping deps pkgName
    in
    pkg.modules
        |> State.foldl
            (\mod dict ->
                State.map (\registeredPackage -> Dict.union registeredPackage dict)
                    (registerModule moduleMapping pkgName resolver mod)
            )
            Dict.empty


registerModule :
    ModuleIds.Mapping
    -> PackageName
    -> Resolver
    -> Elm.Docs.Module
    -> StateM (Dict ( ModuleId, PackageName, VarName ) TypeAlias)
registerModule moduleMapping pkgName resolver mod =
    case ModuleIds.getIdByDotted mod.name moduleMapping of
        Nothing ->
            -- Impossible if we intern modules properly.
            -- Possible if we have a bug.
            State.error
                { moduleName = ModuleNameExtra.fromDotted mod.name
                , declarationNames = []
                , details =
                    AmbiguousModuleOwner
                        { moduleName = mod.name
                        , possiblePackages = []
                        }
                }

        Just moduleId ->
            let
                toError : ErrorDetails -> Error
                toError details =
                    { moduleName = ModuleNameExtra.fromDotted mod.name
                    , declarationNames = []
                    , details = details
                    }

                addBinding : VarName -> Elm.Type.Type -> StateM ()
                addBinding name tipe =
                    State.do (State.fromResult (Result.mapError toError (fromDocsType resolver tipe))) <| \monoType ->
                    State.addGlobalBinding ( moduleId, pkgName, name ) (TypeI.closeOver monoType)
            in
            State.do (State.traverseUnit (\v -> addBinding v.name v.tipe) mod.values) <| \() ->
            State.do (State.traverseUnit (\b -> addBinding b.name b.tipe) mod.binops) <| \() ->
            State.do (State.traverseUnit (\union -> registerUnion pkgName moduleId mod.name resolver union) mod.unions) <| \() ->
            mod.aliases
                |> State.foldl
                    (\typeAlias acc ->
                        State.map
                            (\maybeRegisteredTypeAlias ->
                                case maybeRegisteredTypeAlias of
                                    Nothing ->
                                        acc

                                    Just ( typeAliasKey, registeredTypeAlias ) ->
                                        Dict.insert typeAliasKey registeredTypeAlias acc
                            )
                            (registerAlias pkgName moduleId mod.name resolver typeAlias)
                    )
                    Dict.empty


registerUnion : PackageName -> ModuleId -> String -> Resolver -> Elm.Docs.Union -> StateM ()
registerUnion pkgName moduleId dottedModuleName resolver union =
    let
        toError : ErrorDetails -> Error
        toError details =
            { moduleName = ModuleNameExtra.fromDotted dottedModuleName
            , declarationNames = []
            , details = details
            }

        args : List MonoType
        args =
            union.args |> List.map (\argName -> TypeVar (TypeVar.parse argName))

        resultType : MonoType
        resultType =
            -- We later expect eg. Bools in IfBlock conditions instead of
            -- UserDefinedType "Bool"s, so let's collapse here
            case TypeI.collapsePrimitive pkgName moduleId union.name args of
                Just collapsed ->
                    collapsed

                Nothing ->
                    UserDefinedType
                        { package = pkgName
                        , moduleId = moduleId
                        , name = union.name
                        , args = args
                        }
    in
    union.tags
        |> State.traverseUnit
            (\( ctorName, argTypeStrings ) ->
                State.do
                    (State.fromResult
                        (Result.mapError toError
                            (Result.Extra.combineMap
                                (\argDocsType -> fromDocsType resolver argDocsType)
                                argTypeStrings
                            )
                        )
                    )
                <| \argTypes ->
                let
                    ctorType : MonoType
                    ctorType =
                        argTypes
                            |> List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType
                in
                State.addGlobalBinding ( moduleId, pkgName, ctorName ) (TypeI.closeOver ctorType)
            )


{-| A record type definition gets a constructor function as well
-}
registerAlias :
    PackageName
    -> ModuleId
    -> String
    -> Resolver
    -> Elm.Docs.Alias
    -> StateM (Maybe ( ( ModuleId, PackageName, VarName ), TypeAlias ))
registerAlias pkgName moduleId dottedModuleName resolver alias_ =
    let
        toError : ErrorDetails -> Error
        toError details =
            { moduleName = ModuleNameExtra.fromDotted dottedModuleName
            , declarationNames = []
            , details = details
            }
    in
    State.do (State.fromResult (Result.mapError toError (fromDocsType resolver alias_.tipe))) <| \aliasMono ->
    let
        registerConstructor : StateM ()
        registerConstructor =
            case alias_.tipe of
                Elm.Type.Record fields Nothing ->
                    State.do (State.fromResult (Result.mapError toError (fromDocsFields resolver fields))) <| \resolvedFields ->
                    let
                        ctorType : MonoType
                        ctorType =
                            List.foldr
                                (\( _, fieldT ) acc -> Function { from = fieldT, to = acc })
                                aliasMono
                                resolvedFields
                    in
                    State.addGlobalBinding ( moduleId, pkgName, alias_.name ) (TypeI.closeOver ctorType)

                _ ->
                    State.pureUnit
    in
    State.do registerConstructor <| \() ->
    State.pure <|
        Just
            ( ( moduleId, pkgName, alias_.name )
            , { args = List.map TypeVar.parse alias_.args, type_ = aliasMono }
            )
