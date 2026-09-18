module Elm.TypeInference.Dependencies exposing
    ( Dependencies
    , DependencyPackage
    , Resolver
    , fromList
    , register
    , splitLastDot
    )

{-| Dependency types from docs.json.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Type
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
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
        |> List.map (\pkg -> ( pkg.name, pkg ))
        |> Dict.fromList


{-| Resolves a module name from docs.json to its package.
-}
type alias Resolver =
    String -> Result ErrorDetails ( PackageName, FullModuleName )


resolverFor : Dependencies -> PackageName -> Resolver
resolverFor deps selfPackage =
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
                    (\pkgName acc ->
                        Dict.get pkgName deps
                            |> Maybe.map (\pkg -> List.foldl (addModule pkgName) acc pkg.modules)
                            |> Maybe.withDefault acc
                    )
                    Dict.empty
    in
    \moduleNameStr ->
        case Dict.get moduleNameStr ownersByModule |> Maybe.withDefault [] of
            [] ->
                Ok ( selfPackage, FullModuleName.fromDotted moduleNameStr )

            [ owner ] ->
                Ok ( owner, FullModuleName.fromDotted moduleNameStr )

            matches ->
                Err <|
                    AmbiguousModuleOwner
                        { moduleName = moduleNameStr
                        , possiblePackages = matches
                        }


{-| "Platform.Cmd.Cmd" -> ("Platform.Cmd", "Cmd")
-}
splitLastDot : String -> ( String, String )
splitLastDot qualifiedName =
    let
        parts : List String
        parts =
            String.split "." qualifiedName
    in
    case List.reverse parts of
        [] ->
            ( "", qualifiedName )

        [ single ] ->
            ( "", single )

        last :: rest ->
            ( rest |> List.reverse |> String.join ".", last )


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
                    splitLastDot qualifiedName
            in
            Result.andThen
                (\( package, fullModuleName ) ->
                    Result.Extra.combineMap (fromDocsType resolver) args
                        |> Result.map
                            (\argTypes ->
                                TypeI.collapsePrimitive package fullModuleName typeName argTypes
                                    |> Maybe.withDefault
                                        (UserDefinedType
                                            { package = package
                                            , moduleName = fullModuleName
                                            , name = typeName
                                            , args = argTypes
                                            }
                                        )
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
        (\( name, t ) -> fromDocsType resolver t |> Result.map (Tuple.pair name))
        fields


register : Dependencies -> StateM (Dict ( PackageName, FullModuleName, VarName ) TypeAlias)
register deps =
    deps
        |> Dict.toList
        |> State.traverse (\( pkgName, pkg ) -> registerPackage deps pkgName pkg)
        |> State.map (List.foldl Dict.union Dict.empty)


registerPackage :
    Dependencies
    -> PackageName
    -> DependencyPackage
    -> StateM (Dict ( PackageName, FullModuleName, VarName ) TypeAlias)
registerPackage deps pkgName pkg =
    let
        resolver : Resolver
        resolver =
            resolverFor deps pkgName
    in
    pkg.modules
        |> State.traverse (registerModule pkgName resolver)
        |> State.map (List.foldl Dict.union Dict.empty)


registerModule :
    PackageName
    -> Resolver
    -> Elm.Docs.Module
    -> StateM (Dict ( PackageName, FullModuleName, VarName ) TypeAlias)
registerModule pkgName resolver mod =
    let
        fullModuleName : FullModuleName
        fullModuleName =
            FullModuleName.fromDotted mod.name

        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName fullModuleName
            , declarationNames = []
            , details = details
            }

        addBinding : VarName -> Elm.Type.Type -> StateM ()
        addBinding name tipe =
            State.do (State.fromResult (Result.mapError toError (fromDocsType resolver tipe))) <|
                \monoType ->
                    State.addGlobalBinding ( pkgName, fullModuleName, name ) (TypeI.closeOver monoType)
    in
    State.do (State.traverse (\v -> addBinding v.name v.tipe) mod.values) <|
        \_ ->
            State.do (State.traverse (\b -> addBinding b.name b.tipe) mod.binops) <|
                \_ ->
                    State.do (State.traverse (registerUnion pkgName fullModuleName resolver) mod.unions) <|
                        \_ ->
                            mod.aliases
                                |> State.traverse (registerAlias pkgName fullModuleName resolver)
                                |> State.map (List.filterMap identity >> Dict.fromList)


registerUnion : PackageName -> FullModuleName -> Resolver -> Elm.Docs.Union -> StateM ()
registerUnion pkgName fullModuleName resolver union =
    let
        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName fullModuleName
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
            TypeI.collapsePrimitive pkgName fullModuleName union.name args
                |> Maybe.withDefault
                    (UserDefinedType
                        { package = pkgName
                        , moduleName = fullModuleName
                        , name = union.name
                        , args = args
                        }
                    )
    in
    union.tags
        |> State.traverse
            (\( ctorName, argTypeStrings ) ->
                State.do (State.fromResult (Result.mapError toError (Result.Extra.combineMap (fromDocsType resolver) argTypeStrings))) <|
                    \argTypes ->
                        let
                            ctorType : MonoType
                            ctorType =
                                argTypes
                                    |> List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType
                        in
                        State.addGlobalBinding ( pkgName, fullModuleName, ctorName ) (TypeI.closeOver ctorType)
            )
        |> State.map (always ())


{-| A record type definition gets a constructor function as well
-}
registerAlias :
    PackageName
    -> FullModuleName
    -> Resolver
    -> Elm.Docs.Alias
    -> StateM (Maybe ( ( PackageName, FullModuleName, VarName ), TypeAlias ))
registerAlias pkgName fullModuleName resolver alias_ =
    let
        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName fullModuleName
            , declarationNames = []
            , details = details
            }
    in
    State.do (State.fromResult (Result.mapError toError (fromDocsType resolver alias_.tipe))) <|
        \aliasMono ->
            let
                registerConstructor : StateM ()
                registerConstructor =
                    case alias_.tipe of
                        Elm.Type.Record fields Nothing ->
                            State.do (State.fromResult (Result.mapError toError (fromDocsFields resolver fields))) <|
                                \resolvedFields ->
                                    let
                                        ctorType : MonoType
                                        ctorType =
                                            List.foldr
                                                (\( _, fieldT ) acc -> Function { from = fieldT, to = acc })
                                                aliasMono
                                                resolvedFields
                                    in
                                    State.addGlobalBinding ( pkgName, fullModuleName, alias_.name ) (TypeI.closeOver ctorType)

                        _ ->
                            State.pure ()
            in
            State.do registerConstructor <|
                \() ->
                    State.pure <|
                        Just
                            ( ( pkgName, fullModuleName, alias_.name )
                            , { args = List.map TypeVar.parse alias_.args, type_ = aliasMono }
                            )
