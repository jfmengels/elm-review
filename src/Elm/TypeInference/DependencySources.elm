module Elm.TypeInference.DependencySources exposing (aliases, neededSources, referencedModules)

{-| Get type alias bodies from dependency source files.
We need the alias bodies to know if they're records or unions, for type inference later.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as FileExtra
import Elm.Syntax.FullModuleName as FullModuleName
import Elm.Syntax.ModuleName.Extra as ModuleNameExtra
import Elm.Syntax.Node as Node
import Elm.Type
import Elm.TypeInference.Dependencies exposing (Dependencies)
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.ModuleIds as ModuleIds
import Elm.TypeInference.ModuleIndex as ModuleIndex
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.State exposing (GlobalKey)
import Elm.TypeInference.Type exposing (PackageName)
import Elm.TypeInference.Type.Internal as TypeI
import Elm.TypeInference.TypeVar as TypeVar
import Elm.TypeInference.Unify exposing (TypeAlias)
import List.ExtraExtra
import Result.Extra
import Set exposing (Set)


aliases : ModuleIds.Mapping -> Dependencies -> Dict PackageName (List File) -> Result Error ( Dict GlobalKey TypeAlias, ModuleIds.Mapping )
aliases moduleMapping deps sources =
    Dict.foldl
        (\package files accResult ->
            accResult
                |> Result.andThen
                    (\( accDict, accModuleMapping ) ->
                        packageAliases accModuleMapping deps package files
                            |> Result.map
                                (\( pkgDict, newModuleMapping ) ->
                                    ( Dict.union pkgDict accDict, newModuleMapping )
                                )
                    )
        )
        (Ok ( Dict.empty, moduleMapping ))
        sources


{-| Every dotted module name referenced anywhere in docs.json types.

docs.json values can mention unexposed modules (e.g. `Css.pct` returns
`Css.Internal.ExplicitLength`). We need to intern those too.

-}
referencedModules : Dependencies -> List String
referencedModules deps =
    let
        allModules : List Elm.Docs.Module
        allModules =
            Dict.foldr (\_ { modules } acc -> List.append modules acc) [] deps

        addNameIfUnique : String -> List String -> List String
        addNameIfUnique name names =
            if String.isEmpty name || List.member name names then
                names

            else
                name :: names
    in
    List.foldl (\( name, _ ) acc -> addNameIfUnique name acc) [] (docsModuleRefs allModules)
        |> (\accWithoutModNames ->
                List.foldl (\mod acc -> addNameIfUnique mod.name acc) accWithoutModNames allModules
           )


{-| Which packages' `docs.json` types use unknown modules, or types that
aren't exposed (eg. a `type alias` used in an exposed function's signature,
but not itself in the module's `exposing` list)?

docs.json can't tell us the underlying (record) shape of such a type, so we
need the actual source to know whether it's a record we can unify
structurally against.

-}
neededSources : Dependencies -> Dict PackageName (List File) -> List ( PackageName, List String )
neededSources deps sources =
    let
        docsTypes : Dict String (Set String)
        docsTypes =
            deps
                |> Dict.foldl
                    (\_ dep accAcrossDeps ->
                        dep.modules
                            |> List.foldl
                                (\mod acc ->
                                    let
                                        value : Set String
                                        value =
                                            case Dict.get mod.name acc of
                                                Just existing ->
                                                    Set.union
                                                        (documentedTypeNames mod)
                                                        existing

                                                Nothing ->
                                                    documentedTypeNames mod
                                    in
                                    Dict.insert mod.name value acc
                                )
                                accAcrossDeps
                    )
                    Dict.empty
    in
    deps
        |> Dict.foldr
            (\package pkg needsSourcesAcc ->
                let
                    supplied : Set String
                    supplied =
                        suppliedModuleNames package sources

                    remaining : Set String
                    remaining =
                        docsModuleRefs pkg.modules
                            |> List.foldl
                                (\(( m, _ ) as ref) acc ->
                                    if isKnownRef docsTypes ref || Set.member m supplied then
                                        acc

                                    else
                                        Set.insert m acc
                                )
                                Set.empty
                in
                if Set.isEmpty remaining then
                    needsSourcesAcc

                else
                    ( package
                    , Set.foldr (\m acc -> ModuleNameExtra.dottedToFilePath m :: acc) [] remaining
                    )
                        :: needsSourcesAcc
            )
            []


suppliedModuleNames : PackageName -> Dict PackageName (List File) -> Set String
suppliedModuleNames package sources =
    case Dict.get package sources of
        Just files ->
            files
                |> List.foldl (\m acc -> Set.insert (fileDottedName m) acc)
                    Set.empty

        Nothing ->
            Set.empty


fileDottedName : File -> String
fileDottedName file =
    FileExtra.moduleName file
        |> ModuleNameExtra.toString


documentedTypeNames : Elm.Docs.Module -> Set String
documentedTypeNames mod =
    List.foldl (\union acc -> Set.insert union.name acc)
        (List.foldl (\typeAlias acc -> Set.insert typeAlias.name acc)
            Set.empty
            mod.aliases
        )
        mod.unions


isKnownRef : Dict String (Set String) -> ( String, String ) -> Bool
isKnownRef docsTypes ( moduleName, typeName ) =
    case Dict.get moduleName docsTypes of
        Nothing ->
            False

        Just typeNames ->
            Set.member typeName typeNames


docsModuleRefs : List Elm.Docs.Module -> List ( String, String )
docsModuleRefs modules =
    modules
        |> List.ExtraExtra.fastConcatMap
            (\mod ->
                []
                    |> List.ExtraExtra.fastConcatMapWithInitial (\value -> docsTypeRefs value.tipe) mod.values
                    |> List.ExtraExtra.fastConcatMapWithInitial (\binop -> docsTypeRefs binop.tipe) mod.binops
                    |> List.ExtraExtra.fastConcatMapWithInitial (\union -> List.ExtraExtra.fastConcatMap (\( _, payload ) -> List.ExtraExtra.fastConcatMap docsTypeRefs payload) union.tags) mod.unions
                    |> List.ExtraExtra.fastConcatMapWithInitial (\typeAlias -> docsTypeRefs typeAlias.tipe) mod.aliases
            )


docsTypeRefs : Elm.Type.Type -> List ( String, String )
docsTypeRefs tipe =
    case tipe of
        Elm.Type.Var _ ->
            []

        Elm.Type.Lambda from to ->
            docsTypeRefs from ++ docsTypeRefs to

        Elm.Type.Tuple parts ->
            List.ExtraExtra.fastConcatMap docsTypeRefs parts

        Elm.Type.Type qualifiedName args ->
            let
                ( moduleName, typeName ) =
                    ModuleNameExtra.splitLastDot qualifiedName
            in
            -- Skip elm/core stuff
            (if isPrimitiveRef moduleName typeName then
                []

             else
                modulePart qualifiedName
                    |> List.map (\m -> ( m, typeName ))
            )
                ++ List.ExtraExtra.fastConcatMap docsTypeRefs args

        Elm.Type.Record fields _ ->
            List.ExtraExtra.fastConcatMap (\( _, value ) -> docsTypeRefs value) fields


isPrimitiveRef : String -> String -> Bool
isPrimitiveRef moduleName typeName =
    case moduleName of
        "Basics" ->
            case typeName of
                "Int" ->
                    True

                "Float" ->
                    True

                "Bool" ->
                    True

                _ ->
                    False

        "Char" ->
            typeName == "Char"

        "String" ->
            typeName == "String"

        "List" ->
            typeName == "List"

        _ ->
            False


{-|

     "Platform.Cmd.Cmd"
     --> ["Platform.Cmd"]

     "Int"
     --> []

-}
modulePart : String -> List String
modulePart qualifiedName =
    case ModuleNameExtra.splitLastDot qualifiedName of
        ( "", _ ) ->
            []

        ( moduleName, _ ) ->
            [ moduleName ]


packageAliases :
    ModuleIds.Mapping
    -> Dependencies
    -> PackageName
    -> List File
    -> Result Error ( Dict GlobalKey TypeAlias, ModuleIds.Mapping )
packageAliases moduleMapping deps package files =
    let
        ( modules, moduleMapping1 ) =
            files
                |> List.foldl
                    (\file ( acc, accModuleMapping ) ->
                        let
                            ( moduleIndex, newModuleMapping ) =
                                ModuleIndex.fromFile accModuleMapping file
                        in
                        ( Dict.insert moduleIndex.moduleId moduleIndex acc, newModuleMapping )
                    )
                    ( Dict.empty, moduleMapping )

        visiblePackages : List PackageName
        visiblePackages =
            package :: (Dict.get package deps |> Maybe.map .dependencies |> Maybe.withDefault [])

        index : ModuleLookup.Index
        index =
            deps
                |> Dict.filter (\name _ -> List.member name visiblePackages)
                |> ModuleLookup.buildIndex moduleMapping1
                |> Tuple.first
    in
    files
        |> Result.Extra.foldlWhileOk
            (\file dictAcrossFiles ->
                let
                    ( thisModule, _ ) =
                        ModuleIndex.fromFile moduleMapping1 file

                    resolver : TypeI.TypeResolver
                    resolver qualifier name =
                        ModuleLookup.typeResolverFor moduleMapping1 index modules thisModule qualifier name
                            |> Result.map
                                (\( owner, moduleId ) ->
                                    ( if owner == "" then
                                        package

                                      else
                                        owner
                                    , moduleId
                                    )
                                )
                in
                file.declarations
                    |> Result.Extra.foldlWhileOk
                        (\node dict ->
                            case Node.value node of
                                Declaration.AliasDeclaration alias_ ->
                                    TypeI.fromTypeAnnotation resolver (Node.value alias_.typeAnnotation)
                                        |> Result.mapError
                                            (\err ->
                                                { moduleName = FullModuleName.toModuleName thisModule.moduleName
                                                , declarationNames = [ Node.value alias_.name ]
                                                , details = TypeI.fromTypeAnnotationError err
                                                }
                                            )
                                        |> Result.map
                                            (\body ->
                                                Dict.insert
                                                    ( thisModule.moduleId, package, Node.value alias_.name )
                                                    { args = List.map (\(Node.Node _ generic) -> TypeVar.parse generic) alias_.generics
                                                    , type_ = body
                                                    }
                                                    dict
                                            )

                                _ ->
                                    Ok dict
                        )
                        dictAcrossFiles
            )
            Dict.empty
        |> Result.map (\dict -> ( dict, moduleMapping1 ))
