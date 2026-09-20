module Elm.TypeInference.ModuleLookup exposing
    ( Index
    , buildIndex
    , moduleOfVar
    , resolveOperatorFunction
    , typeResolverFor
    , wrapError
    )

{-| -}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Elm.TypeInference.Dependencies exposing (Dependencies)
import Elm.TypeInference.Error.Internal exposing (ResolverAmbiguity)
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.InferError exposing (InferErrorDetails(..))
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ImportIndex, ModuleIndex)
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal exposing (TypeResolver)
import List.ExtraExtra
import Result.Extra
import Set exposing (Set)
import String.ExtraExtra


{-| Precomputed index of `module id -> value/type name -> packages defining it`.
-}
type Index
    = Index
        { values : NameIndex
        , types : NameIndex
        , ctorParents : Dict ModuleId (Dict VarName VarName)
        , recordAliases : Dict ModuleId (Set VarName)
        }


type alias NameIndex =
    Dict ModuleId (Dict VarName (List PackageName))


buildIndex : ModuleIds.Mapping -> Dependencies -> ( Index, ModuleIds.Mapping )
buildIndex moduleMapping deps =
    Dict.foldl
        (\packageName pkg outerAcc ->
            List.foldl
                (\mod innerAcc -> addModule packageName mod innerAcc)
                outerAcc
                pkg.modules
        )
        ( emptyIndex, moduleMapping )
        deps


addModule : PackageName -> Elm.Docs.Module -> ( Index, ModuleIds.Mapping ) -> ( Index, ModuleIds.Mapping )
addModule packageName mod ( Index idx, moduleMapping ) =
    let
        ( moduleId, moduleMapping1 ) =
            ModuleIds.intern (FullModuleName.fromDotted mod.name) moduleMapping
    in
    ( Index
        { values = List.foldl (\name acc -> addName moduleId packageName name acc) idx.values (valueNamesOf mod)
        , types = List.foldl (\name acc -> addName moduleId packageName name acc) idx.types (typeNamesOf mod)
        , ctorParents = addCtorParents moduleId mod idx.ctorParents
        , recordAliases = addRecordAliases moduleId mod idx.recordAliases
        }
    , moduleMapping1
    )


valueNamesOf : Elm.Docs.Module -> List VarName
valueNamesOf mod =
    List.map .name mod.values
        ++ List.map .name mod.binops
        ++ List.ExtraExtra.fastConcatMap (\u -> List.map Tuple.first u.tags) mod.unions
        ++ List.filterMap
            (\a ->
                if isRecordAlias a then
                    Just a.name

                else
                    Nothing
            )
            mod.aliases


typeNamesOf : Elm.Docs.Module -> List VarName
typeNamesOf mod =
    List.map .name mod.unions ++ List.map .name mod.aliases


addCtorParents : ModuleId -> Elm.Docs.Module -> Dict ModuleId (Dict VarName VarName) -> Dict ModuleId (Dict VarName VarName)
addCtorParents moduleId mod acc =
    List.foldl
        (\union inner ->
            List.foldl
                (\( ctor, _ ) innerDict ->
                    case Dict.get moduleId innerDict of
                        Just ctors ->
                            Dict.insert moduleId (Dict.insert ctor union.name ctors) innerDict

                        Nothing ->
                            Dict.insert moduleId (Dict.singleton ctor union.name) innerDict
                )
                inner
                union.tags
        )
        acc
        mod.unions


addRecordAliases : ModuleId -> Elm.Docs.Module -> Dict ModuleId (Set VarName) -> Dict ModuleId (Set VarName)
addRecordAliases moduleId mod acc =
    List.foldl
        (\alias inner ->
            if isRecordAlias alias then
                case Dict.get moduleId inner of
                    Just set ->
                        Dict.insert moduleId (Set.insert alias.name set) inner

                    Nothing ->
                        Dict.insert moduleId (Set.singleton alias.name) inner

            else
                inner
        )
        acc
        mod.aliases


addName :
    ModuleId
    -> PackageName
    -> VarName
    -> NameIndex
    -> NameIndex
addName moduleId packageName name acc =
    case Dict.get moduleId acc of
        Just inner ->
            case Dict.get name inner of
                Just owners ->
                    Dict.insert moduleId (Dict.insert name (owners ++ [ packageName ]) inner) acc

                Nothing ->
                    Dict.insert moduleId (Dict.insert name [ packageName ] inner) acc

        Nothing ->
            Dict.insert moduleId (Dict.singleton name [ packageName ]) acc


ownersOf : NameIndex -> ModuleId -> VarName -> List PackageName
ownersOf index moduleId name =
    case Dict.get moduleId index of
        Just vars ->
            case Dict.get name vars of
                Just owners ->
                    owners

                Nothing ->
                    []

        Nothing ->
            []


emptyIndex : Index
emptyIndex =
    Index
        { values = Dict.empty
        , types = Dict.empty
        , ctorParents = Dict.empty
        , recordAliases = Dict.empty
        }


moduleIdToModuleName : ModuleIds.Mapping -> ModuleId -> ModuleName
moduleIdToModuleName moduleMapping moduleId =
    ModuleIds.moduleNameForDisplay moduleId moduleMapping


moduleIdToString : ModuleIds.Mapping -> ModuleId -> String
moduleIdToString moduleMapping moduleId =
    ModuleIds.dottedForDisplay moduleId moduleMapping


{-| Find the package and interned module id of the var.
-}
moduleOfVar :
    ModuleIds.Mapping
    -> Index
    -> Dict ModuleId ModuleIndex
    -> ModuleIndex
    -> Maybe FullModuleName
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
moduleOfVar moduleMapping index modules thisModule maybeModuleName varName =
    case maybeModuleName of
        Nothing ->
            case unqualifiedVarInThisModule thisModule varName of
                Ok Nothing ->
                    unqualifiedVarOutsideThisModule moduleMapping index modules thisModule varName

                found ->
                    found

        Just qualifier ->
            qualifiedVar moduleMapping index modules thisModule qualifier varName


{-| Wraps pure `moduleOfVar` result into `StateM` for `State.do` chains.
Wraps error details in InferError.
Doesn't do anything with State otherwise.
-}
wrapError :
    ModuleIndex
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
    -> StateM ( PackageName, ModuleId )
wrapError thisModule varName resolved =
    case resolved of
        Ok (Just result) ->
            State.pure result

        Ok Nothing ->
            let
                moduleName : ModuleName
                moduleName =
                    FullModuleName.toModuleName thisModule.moduleName
            in
            State.error
                { moduleName = moduleName
                , declarationNames = []
                , details =
                    VarNotFound
                        { varName = varName
                        , usedIn = moduleName
                        }
                }

        Err details ->
            State.error
                { moduleName = FullModuleName.toModuleName thisModule.moduleName
                , declarationNames = []
                , details = details
                }


{-| `infix left 6 (+) = add` only gives unqualified `add`.
`add` could be defined in this module, or perhaps imported? (I didn't check what
the compiler allows as defining operators is pretty niche functionality only
reserved for elm/\* packages).
-}
resolveOperatorFunction :
    ModuleIds.Mapping
    -> Dict ModuleId ModuleIndex
    -> ModuleId
    -> VarName
    -> Result InferErrorDetails (Maybe ( ModuleId, VarName ))
resolveOperatorFunction moduleMapping modules operatorModuleId operator =
    case Dict.get operatorModuleId modules of
        Nothing ->
            Ok Nothing

        Just operatorModule ->
            case Dict.get operator operatorModule.infixes of
                Nothing ->
                    Ok Nothing

                Just functionName ->
                    moduleOfVar moduleMapping emptyIndex modules operatorModule Nothing functionName
                        |> Result.map (\mod -> mod |> Maybe.map (\( _, functionModuleId ) -> ( functionModuleId, functionName )))


unqualifiedVarInThisModule :
    ModuleIndex
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
unqualifiedVarInThisModule thisModule varName =
    Ok <|
        if Set.member varName thisModule.declaredValues then
            Just ( "", thisModule.moduleId )

        else
            Nothing


unqualifiedVarOutsideThisModule :
    ModuleIds.Mapping
    -> Index
    -> Dict ModuleId ModuleIndex
    -> ModuleIndex
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
unqualifiedVarOutsideThisModule moduleMapping index modules thisModule varName =
    Result.Extra.combineMap
        (\import_ ->
            if ModuleIndex.importCouldExposeValue import_ varName then
                explicitImportDefinesValue moduleMapping index modules import_ varName

            else
                Ok Nothing
        )
        thisModule.imports
        |> Result.andThen
            (\explicitMaybeMatches ->
                let
                    home : ModuleId
                    home =
                        ImplicitImports.implicitValueHomeId varName
                in
                dependencyModuleDefines moduleMapping index home varName
                    |> Result.map
                        (\maybePackage ->
                            let
                                explicitMatches : List ( PackageName, ModuleId )
                                explicitMatches =
                                    List.filterMap identity explicitMaybeMatches
                            in
                            case maybePackage |> Maybe.map (\package -> ( package, home )) of
                                Nothing ->
                                    explicitMatches

                                Just implicitMatch ->
                                    explicitMatches ++ [ implicitMatch ]
                        )
            )
        |> Result.andThen
            (\allMatches ->
                case dedupeOwners allMatches of
                    [] ->
                        Ok Nothing

                    [ single ] ->
                        Ok (Just single)

                    many ->
                        Err <|
                            AmbiguousName
                                { varName = varName
                                , usedIn = FullModuleName.toModuleName thisModule.moduleName
                                , possibleModules = List.map (\( _, modId ) -> moduleIdToModuleName moduleMapping modId) many
                                }
            )


explicitImportDefinesValue :
    ModuleIds.Mapping
    -> Index
    -> Dict ModuleId ModuleIndex
    -> ImportIndex
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
explicitImportDefinesValue moduleMapping index modules import_ varName =
    case Dict.get import_.moduleId modules of
        Just importedModule ->
            Ok <|
                if ModuleIndex.importExposesValue importedModule import_ varName then
                    Just ( "", import_.moduleId )

                else
                    Nothing

        Nothing ->
            dependencyImportDefinesValue moduleMapping index import_ varName


dependencyImportDefinesValue :
    ModuleIds.Mapping
    -> Index
    -> ImportIndex
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
dependencyImportDefinesValue moduleMapping (Index idx) import_ varName =
    case import_.exposing_ of
        ModuleIndex.ExposesNothing ->
            Ok Nothing

        ModuleIndex.ExposesAll ->
            dependencyModuleDefines moduleMapping (Index idx) import_.moduleId varName
                |> Result.map
                    (\maybePackage ->
                        maybePackage |> Maybe.map (\package -> ( package, import_.moduleId ))
                    )

        ModuleIndex.ExposesExplicit e ->
            if Set.member varName e.values then
                dependencyModuleDefines moduleMapping (Index idx) import_.moduleId varName
                    |> Result.map
                        (\maybePackage ->
                            maybePackage |> Maybe.map (\package -> ( package, import_.moduleId ))
                        )

            else if not (couldBeConstructorName varName) then
                Ok Nothing

            else
                let
                    viaRecordAlias : Bool
                    viaRecordAlias =
                        Set.member varName e.opaqueTypes
                            && (case Dict.get import_.moduleId idx.recordAliases of
                                    Just aliases ->
                                        Set.member varName aliases

                                    Nothing ->
                                        False
                               )

                    viaOpenUnion : Bool
                    viaOpenUnion =
                        case
                            Dict.get import_.moduleId idx.ctorParents
                                |> Maybe.andThen (\p -> Dict.get varName p)
                        of
                            Just parent ->
                                Set.member parent e.openTypes

                            Nothing ->
                                False
                in
                if viaRecordAlias || viaOpenUnion then
                    dependencyModuleDefines moduleMapping (Index idx) import_.moduleId varName
                        |> Result.map
                            (\maybePackage ->
                                maybePackage |> Maybe.map (\package -> ( package, import_.moduleId ))
                            )

                else
                    Ok Nothing


qualifiedVar :
    ModuleIds.Mapping
    -> Index
    -> Dict ModuleId ModuleIndex
    -> ModuleIndex
    -> FullModuleName
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
qualifiedVar moduleMapping index modules thisModule qualifier varName =
    case qualifier of
        ( single, [] ) ->
            let
                aliasCandidates : List ModuleId
                aliasCandidates =
                    let
                        singleModulesWithAlias : List ModuleId
                        singleModulesWithAlias =
                            ModuleIndex.modulesWithAlias thisModule single
                    in
                    dedupeModuleIds
                        (case ImplicitImports.unaliasModuleId single of
                            Just m ->
                                singleModulesWithAlias ++ [ m ]

                            Nothing ->
                                singleModulesWithAlias
                        )
            in
            Result.Extra.combineMap
                (\unaliased -> qualifiedModuleDefines moduleMapping index modules unaliased varName)
                aliasCandidates
                |> Result.andThen
                    (\aliasMatches ->
                        case dedupeOwners (List.filterMap identity aliasMatches) of
                            [] ->
                                let
                                    qualifierModuleName : ModuleName
                                    qualifierModuleName =
                                        FullModuleName.toModuleName qualifier
                                in
                                if
                                    ModuleIndex.isImportedUnaliased thisModule qualifierModuleName
                                        || ImplicitImports.isImplicitlyImportedModule qualifierModuleName
                                then
                                    qualifiedModuleDefinesByName moduleMapping index modules qualifier varName

                                else
                                    Ok Nothing

                            [ singleDef ] ->
                                -- The alias wins even if the literal
                                -- module also defines the name.
                                Ok (Just singleDef)

                            multiple ->
                                Err <|
                                    AmbiguousName
                                        { varName = varName
                                        , usedIn = FullModuleName.toModuleName thisModule.moduleName
                                        , possibleModules = List.map (\( _, modId ) -> moduleIdToModuleName moduleMapping modId) multiple
                                        }
                    )

        _ ->
            if ModuleIndex.isImportedUnaliased thisModule (FullModuleName.toModuleName qualifier) then
                qualifiedModuleDefinesByName moduleMapping index modules qualifier varName

            else
                Ok Nothing


qualifiedModuleDefines :
    ModuleIds.Mapping
    -> Index
    -> Dict ModuleId ModuleIndex
    -> ModuleId
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
qualifiedModuleDefines moduleMapping index modules moduleId varName =
    case Dict.get moduleId modules of
        Just moduleIndex ->
            Ok <|
                if Set.member varName moduleIndex.exposedValues then
                    Just ( "", moduleId )

                else
                    Nothing

        Nothing ->
            dependencyModuleDefines moduleMapping index moduleId varName
                |> Result.map
                    (\maybePackage ->
                        maybePackage |> Maybe.map (\package -> ( package, moduleId ))
                    )


qualifiedModuleDefinesByName :
    ModuleIds.Mapping
    -> Index
    -> Dict ModuleId ModuleIndex
    -> FullModuleName
    -> VarName
    -> Result InferErrorDetails (Maybe ( PackageName, ModuleId ))
qualifiedModuleDefinesByName moduleMapping index modules qualifier varName =
    case ModuleIds.getId qualifier moduleMapping of
        Nothing ->
            Ok Nothing

        Just moduleId ->
            qualifiedModuleDefines moduleMapping index modules moduleId varName


dedupeOwners : List ( PackageName, ModuleId ) -> List ( PackageName, ModuleId )
dedupeOwners pairs =
    deduplicate pairs


dedupeModuleIds : List ModuleId -> List ModuleId
dedupeModuleIds names =
    deduplicate names


{-| Drop repeats, keeping first occurrences in order.
-}
deduplicate : List comparable -> List comparable
deduplicate items =
    deduplicateHelp items Set.empty []


deduplicateHelp : List comparable -> Set comparable -> List comparable -> List comparable
deduplicateHelp items seen acc =
    case items of
        [] ->
            List.reverse acc

        item :: rest ->
            if Set.member item seen then
                deduplicateHelp rest seen acc

            else
                deduplicateHelp rest (Set.insert item seen) (item :: acc)


dependencyModuleDefines : ModuleIds.Mapping -> Index -> ModuleId -> VarName -> Result InferErrorDetails (Maybe PackageName)
dependencyModuleDefines moduleMapping (Index index) moduleId varName =
    let
        matches : List PackageName
        matches =
            ownersOf index.values moduleId varName
    in
    case matches of
        [] ->
            Ok Nothing

        [ single ] ->
            Ok (Just single)

        _ :: _ :: _ ->
            Err (AmbiguousModuleOwner { moduleName = moduleIdToString moduleMapping moduleId, possiblePackages = matches })


couldBeConstructorName : VarName -> Bool
couldBeConstructorName =
    String.ExtraExtra.firstCharIsUpper


isRecordAlias : Elm.Docs.Alias -> Bool
isRecordAlias alias_ =
    case alias_.tipe of
        Elm.Type.Record _ Nothing ->
            True

        _ ->
            False


dependencyModuleDefinesType : Index -> ModuleId -> VarName -> Maybe ( PackageName, ModuleId )
dependencyModuleDefinesType (Index index) moduleId typeName =
    case ownersOf index.types moduleId typeName of
        [] ->
            Nothing

        packageName :: _ ->
            Just ( packageName, moduleId )


implicitTypeModule : ModuleName -> VarName -> Maybe ( PackageName, ModuleId )
implicitTypeModule qualifier typeName =
    if not (List.isEmpty qualifier) then
        Nothing

    else
        ImplicitImports.moduleExposingTypeId typeName
            |> Maybe.map (\id -> ( ImplicitImports.elmCorePackage, id ))


{-| A qualifier like `Parser.` can mean two different modules at once:

    import Elm.Parser as Parser
    import Parser

Elm accepts this as long as each individual name is unambiguous, so we can't
just resolve `Parser` to a single module and be done - we have to try the
modules the qualifier could stand for and pick the one that actually declares
the type.

-}
qualifierCandidates : ModuleIds.Mapping -> ModuleIndex -> ModuleName -> List ModuleId
qualifierCandidates moduleMapping thisModule qualifier =
    let
        aliasedModules : List ModuleId
        aliasedModules =
            case qualifier of
                [ single ] ->
                    ModuleIndex.modulesWithAlias thisModule single

                _ ->
                    []

        implicitAlias : List ModuleId
        implicitAlias =
            case qualifier of
                [ single ] ->
                    case ImplicitImports.unaliasModuleId single of
                        Just m ->
                            [ m ]

                        Nothing ->
                            []

                _ ->
                    []

        aliasCandidates : List ModuleId
        aliasCandidates =
            deduplicate (aliasedModules ++ implicitAlias)

        literalAvailable : Bool
        literalAvailable =
            -- Unqualified lookup always runs: it checks local declarations,
            -- then explicit imports (last wins), then the implicit prelude.
            List.isEmpty qualifier
                || ModuleIndex.isImportedUnaliased thisModule qualifier
                || ImplicitImports.isImplicitlyImportedModule qualifier

        literalId : Maybe ModuleId
        literalId =
            if List.isEmpty qualifier then
                Just thisModule.moduleId

            else
                ModuleIds.getId (FullModuleName.fromModuleName_ qualifier) moduleMapping
    in
    if List.isEmpty aliasCandidates then
        if literalAvailable then
            case literalId of
                Just lid ->
                    [ lid ]

                Nothing ->
                    []

        else
            []

    else if literalAvailable then
        case literalId of
            Just literalId_ ->
                aliasCandidates ++ [ literalId_ ]

            Nothing ->
                aliasCandidates

    else
        aliasCandidates


typeResolverFor : ModuleIds.Mapping -> Index -> Dict ModuleId ModuleIndex -> ModuleIndex -> TypeResolver
typeResolverFor moduleMapping ((Index index) as wrappedIndex) modules thisModule qualifier typeName =
    let
        candidates : List ModuleId
        candidates =
            qualifierCandidates moduleMapping thisModule qualifier

        firstParty : ModuleId -> Maybe ( PackageName, ModuleId )
        firstParty unaliasedId =
            if unaliasedId == thisModule.moduleId && List.isEmpty qualifier then
                if Set.member typeName thisModule.declaredTypes then
                    Just ( "", thisModule.moduleId )

                else
                    thisModule.imports
                        |> List.ExtraExtra.findLastMap
                            (\import_ ->
                                if not (ModuleIndex.importExposesType import_ typeName) then
                                    Nothing

                                else
                                    case Dict.get import_.moduleId modules of
                                        Just importedModule ->
                                            if Set.member typeName importedModule.exposedTypes then
                                                Just ( "", import_.moduleId )

                                            else
                                                Nothing

                                        Nothing ->
                                            dependencyModuleDefinesType wrappedIndex import_.moduleId typeName
                            )

            else
                Dict.get unaliasedId modules
                    |> Maybe.andThen
                        (\moduleIndex ->
                            if Set.member typeName moduleIndex.exposedTypes then
                                Just ( "", unaliasedId )

                            else
                                Nothing
                        )

        dependency : ModuleId -> Result ResolverAmbiguity (Maybe ( PackageName, ModuleId ))
        dependency unaliasedId =
            if unaliasedId == thisModule.moduleId && List.isEmpty qualifier then
                Ok Nothing

            else
                let
                    matchingPackages : List PackageName
                    matchingPackages =
                        ownersOf index.types unaliasedId typeName
                in
                case matchingPackages of
                    [] ->
                        Ok Nothing

                    [ single ] ->
                        Ok (Just ( single, unaliasedId ))

                    _ :: _ :: _ ->
                        Err
                            { moduleName = moduleIdToString moduleMapping unaliasedId
                            , possiblePackages = matchingPackages
                            }
    in
    firstJustInCandidates firstParty dependency candidates
        |> Result.map
            (\resolved ->
                case resolved of
                    (Just _) as justFound ->
                        justFound

                    Nothing ->
                        implicitTypeModule qualifier typeName
            )
        |> Result.andThen
            (\maybeFound ->
                case maybeFound of
                    Just found ->
                        Ok found

                    Nothing ->
                        let
                            defaultId : Result ResolverAmbiguity ModuleId
                            defaultId =
                                case candidates of
                                    head :: _ ->
                                        Ok head

                                    [] ->
                                        if List.isEmpty qualifier then
                                            Ok thisModule.moduleId

                                        else
                                            case ModuleIds.getId (FullModuleName.fromModuleName_ qualifier) moduleMapping of
                                                Just lid ->
                                                    Ok lid

                                                Nothing ->
                                                    -- Unknown qualifier (not imported/implicit/interned).
                                                    Err
                                                        { moduleName = String.join "." qualifier
                                                        , possiblePackages = []
                                                        }
                        in
                        Result.map (\id -> ( "", id )) defaultId
            )


firstJustInCandidates :
    (ModuleId -> Maybe ( PackageName, ModuleId ))
    -> (ModuleId -> Result ResolverAmbiguity (Maybe ( PackageName, ModuleId )))
    -> List ModuleId
    -> Result ResolverAmbiguity (Maybe ( PackageName, ModuleId ))
firstJustInCandidates firstParty dependency candidates =
    case candidates of
        [] ->
            Ok Nothing

        candidate :: rest ->
            case firstParty candidate of
                (Just _) as found ->
                    Ok found

                Nothing ->
                    case dependency candidate of
                        Ok Nothing ->
                            firstJustInCandidates firstParty dependency rest

                        found ->
                            found
