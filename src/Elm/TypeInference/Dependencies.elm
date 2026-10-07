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
import Elm.TypeInference.Error.Internal exposing (ResolverAmbiguity)
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.ProjectError exposing (Location, ProjectError(..))
import Elm.TypeInference.State exposing (GlobalKey)
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
    String -> Result ResolverAmbiguity ( PackageName, ModuleId )


type FromDocsTypeError
    = ImpossibleDocs Elm.Type.Type
    | AmbiguousDocsModule ResolverAmbiguity


toProjectError : PackageName -> String -> VarName -> FromDocsTypeError -> ProjectError
toProjectError pkgName dottedModuleName declarationName err =
    let
        location : Location
        location =
            { package = pkgName
            , moduleName = ModuleNameExtra.fromDotted dottedModuleName
            , declarationName = declarationName
            }
    in
    case err of
        ImpossibleDocs type_ ->
            ImpossibleDocsType { location = location, type_ = type_ }

        AmbiguousDocsModule ambiguity ->
            AmbiguousModuleOwner
                { location = location
                , moduleName = ambiguity.moduleName
                , possiblePackages = ambiguity.possiblePackages
                }


resolverFor : ModuleIds.Mapping -> Dependencies -> PackageName -> Resolver
resolverFor moduleMapping deps selfPackage =
    let
        searchOrder : List PackageName
        searchOrder =
            case Dict.get selfPackage deps of
                Just selfPkg ->
                    selfPackage :: selfPkg.dependencies

                Nothing ->
                    [ selfPackage ]

        addModule : PackageName -> Elm.Docs.Module -> Dict String (List PackageName) -> Dict String (List PackageName)
        addModule pkgName mod acc =
            case Dict.get mod.name acc of
                Just existing ->
                    Dict.insert mod.name (existing ++ [ pkgName ]) acc

                Nothing ->
                    Dict.insert mod.name [ pkgName ] acc

        ownersByModule : Dict String (List PackageName)
        ownersByModule =
            searchOrder
                |> List.foldl
                    (\pkgName accAcrossPks ->
                        case Dict.get pkgName deps of
                            Just pkg ->
                                List.foldl (\mod acc -> addModule pkgName mod acc) accAcrossPks pkg.modules

                            Nothing ->
                                accAcrossPks
                    )
                    Dict.empty

        moduleIdOf : String -> Result ResolverAmbiguity ModuleId
        moduleIdOf dotted =
            case ModuleIds.getIdByDotted dotted moduleMapping of
                Just moduleId ->
                    Ok moduleId

                Nothing ->
                    -- Impossible if we pre-intern docs modules properly.
                    -- Possible if we have a bug.
                    Err
                        { moduleName = dotted
                        , possiblePackages = []
                        }
    in
    \moduleNameStr ->
        moduleIdOf moduleNameStr
            |> Result.andThen
                (\moduleId ->
                    case Dict.get moduleNameStr ownersByModule of
                        Nothing ->
                            Ok ( selfPackage, moduleId )

                        Just [] ->
                            Ok ( selfPackage, moduleId )

                        Just [ owner ] ->
                            Ok ( owner, moduleId )

                        Just matches ->
                            Err
                                { moduleName = moduleNameStr
                                , possiblePackages = matches
                                }
                )


fromDocsType : Resolver -> Elm.Type.Type -> Result FromDocsTypeError MonoType
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
            Err (ImpossibleDocs type_)

        Elm.Type.Type qualifiedName args ->
            let
                ( moduleNameStr, typeName ) =
                    ModuleNameExtra.splitLastDot qualifiedName
            in
            if String.isEmpty moduleNameStr then
                -- Impossible in principle (Elm compiler generates docs.json with fully qualified types).
                -- Possible in practice (if somebody hand-crafts a docs.json file).
                Err (ImpossibleDocs type_)

            else
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
                    (resolver moduleNameStr |> Result.mapError AmbiguousDocsModule)

        Elm.Type.Record fields Nothing ->
            fromDocsFields resolver fields
                |> Result.map (\fields_ -> Record (Dict.fromList fields_))

        Elm.Type.Record fields (Just rowVar) ->
            fromDocsFields resolver fields
                |> Result.map
                    (\resolvedFields ->
                        ExtensibleRecord
                            { extensionTypevar = TypeVar (TypeVar.parse rowVar)
                            , fields = Dict.fromList resolvedFields
                            }
                    )


fromDocsFields : Resolver -> List ( String, Elm.Type.Type ) -> Result FromDocsTypeError (List ( String, MonoType ))
fromDocsFields resolver fields =
    Result.Extra.combineMap
        (\( name, value ) ->
            fromDocsType resolver value
                |> Result.map (\valueType -> ( name, valueType ))
        )
        fields


{-| What registering dependencies contributes: constructor/value types and
type alias bodies.
-}
type alias Registered =
    { globalEnv : Dict GlobalKey TypeI.Type
    , typeAliases : Dict GlobalKey TypeAlias
    }


register :
    ModuleIds.Mapping
    -> Dependencies
    ->
        Result
            ProjectError
            { globalEnv : Dict GlobalKey TypeI.Type
            , typeAliases : Dict GlobalKey TypeAlias
            , moduleMapping : ModuleIds.Mapping
            }
register moduleMapping deps =
    let
        moduleMappingWithAllPackageModules : ModuleIds.Mapping
        moduleMappingWithAllPackageModules =
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
        |> Result.Extra.foldlWhileOk
            (\( pkgName, pkg ) acc ->
                registerPackage
                    moduleMappingWithAllPackageModules
                    deps
                    pkgName
                    pkg
                    acc
            )
            { globalEnv = Dict.empty
            , typeAliases = Dict.empty
            }
        |> Result.map
            (\registered ->
                { globalEnv = registered.globalEnv
                , typeAliases = registered.typeAliases
                , moduleMapping = moduleMappingWithAllPackageModules
                }
            )


registerPackage :
    ModuleIds.Mapping
    -> Dependencies
    -> PackageName
    -> DependencyPackage
    -> Registered
    -> Result ProjectError Registered
registerPackage moduleMapping deps pkgName pkg registered =
    let
        resolver : Resolver
        resolver =
            resolverFor moduleMapping deps pkgName
    in
    pkg.modules
        |> Result.Extra.foldlWhileOk
            (\mod acc ->
                registerModule
                    moduleMapping
                    pkgName
                    resolver
                    mod
                    acc
            )
            registered


addGlobalBinding : GlobalKey -> MonoType -> Registered -> Registered
addGlobalBinding key monoType registered =
    { globalEnv = Dict.insert key (TypeI.closeOver monoType) registered.globalEnv
    , typeAliases = registered.typeAliases
    }


registerModule :
    ModuleIds.Mapping
    -> PackageName
    -> Resolver
    -> Elm.Docs.Module
    -> Registered
    -> Result ProjectError Registered
registerModule moduleMapping pkgName resolver mod registered =
    let
        moduleId : ModuleId
        moduleId =
            -- `register` already interned every package module, so this
            -- only looks up the existing id and the mapping stays the same.
            ModuleIds.intern (FullModuleName.fromDotted mod.name) moduleMapping
                |> Tuple.first

        addBinding : VarName -> Elm.Type.Type -> Registered -> Result ProjectError Registered
        addBinding name tipe acc =
            fromDocsType resolver tipe
                |> Result.mapError (toProjectError pkgName mod.name name)
                |> Result.map (\monoType -> addGlobalBinding ( moduleId, pkgName, name ) monoType acc)
    in
    registered
        |> (\acc -> Result.Extra.foldlWhileOk (\v -> addBinding v.name v.tipe) acc mod.values)
        |> Result.andThen (\acc -> Result.Extra.foldlWhileOk (\b -> addBinding b.name b.tipe) acc mod.binops)
        |> Result.andThen (\acc -> Result.Extra.foldlWhileOk (registerUnion pkgName moduleId mod.name resolver) acc mod.unions)
        |> Result.andThen (\acc -> Result.Extra.foldlWhileOk (registerAlias pkgName moduleId mod.name resolver) acc mod.aliases)


registerUnion : PackageName -> ModuleId -> String -> Resolver -> Elm.Docs.Union -> Registered -> Result ProjectError Registered
registerUnion pkgName moduleId dottedModuleName resolver union registered =
    let
        toError : FromDocsTypeError -> ProjectError
        toError =
            toProjectError pkgName dottedModuleName union.name

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
        |> Result.Extra.foldlWhileOk
            (\( ctorName, argTypeStrings ) acc ->
                Result.Extra.combineMap
                    (\argDocsType -> fromDocsType resolver argDocsType)
                    argTypeStrings
                    |> Result.mapError toError
                    |> Result.map
                        (\argTypes ->
                            let
                                ctorType : MonoType
                                ctorType =
                                    argTypes
                                        |> List.foldr
                                            (\argT ctorAcc ->
                                                Function
                                                    { from = argT
                                                    , to = ctorAcc
                                                    }
                                            )
                                            resultType
                            in
                            addGlobalBinding
                                ( moduleId, pkgName, ctorName )
                                ctorType
                                acc
                        )
            )
            registered


{-| A record type definition gets a constructor function as well
-}
registerAlias :
    PackageName
    -> ModuleId
    -> String
    -> Resolver
    -> Elm.Docs.Alias
    -> Registered
    -> Result ProjectError Registered
registerAlias pkgName moduleId dottedModuleName resolver alias_ registered =
    let
        toError : FromDocsTypeError -> ProjectError
        toError =
            toProjectError pkgName dottedModuleName alias_.name
    in
    fromDocsType resolver alias_.tipe
        |> Result.mapError toError
        |> Result.andThen
            (\aliasMono ->
                let
                    withConstructor : Result ProjectError Registered
                    withConstructor =
                        case alias_.tipe of
                            Elm.Type.Record fields Nothing ->
                                fromDocsFields resolver fields
                                    |> Result.mapError toError
                                    |> Result.map
                                        (\resolvedFields ->
                                            let
                                                ctorType : MonoType
                                                ctorType =
                                                    List.foldr
                                                        (\( _, fieldT ) acc ->
                                                            Function
                                                                { from = fieldT
                                                                , to = acc
                                                                }
                                                        )
                                                        aliasMono
                                                        resolvedFields
                                            in
                                            addGlobalBinding
                                                ( moduleId, pkgName, alias_.name )
                                                ctorType
                                                registered
                                        )

                            _ ->
                                Ok registered
                in
                withConstructor
                    |> Result.map
                        (\acc ->
                            { globalEnv = acc.globalEnv
                            , typeAliases =
                                Dict.insert ( moduleId, pkgName, alias_.name )
                                    { args = List.map TypeVar.parse alias_.args
                                    , type_ = aliasMono
                                    }
                                    acc.typeAliases
                            }
                        )
            )
