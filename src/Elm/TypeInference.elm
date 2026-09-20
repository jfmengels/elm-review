module Elm.TypeInference exposing
    ( init, empty, Project, Dependency
    , getType, getAllTypes, expand
    , addFile, removeFile
    )

{-| Type inference for
[`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
ASTs.

The process:

  - Load project and dependency data into a [`Project`](#Project) with [`init`](#init).
  - Get a [`Type`](Elm-TypeInference-Type#Type) for a given AST
    [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Node#Node)'s
    [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#Range)
    with [`getType`](#getType). The computed data is cached into a new version
    of the [`Project`](#Project).
  - Let the `Project` know about changed or deleted files with
    [`addFile`](#addFile) and [`removeFile`](#removeFile).

@docs init, empty, Project, Dependency

@docs getType, getAllTypes, expand

@docs addFile, removeFile

-}

import Array
import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.File.Extra as FileExtra
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra as ModuleNameExtra
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as SyntaxType
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Dependencies as Dependencies exposing (Dependencies)
import Elm.TypeInference.DependencySources as DependencySources
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError)
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.InferError exposing (InferError, InferErrorDetails(..))
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.ProjectError as ProjectError exposing (ProjectError)
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (GlobalKey, StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (PackageName, Type, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType(..), TypeResolver)
import Elm.TypeInference.TypeVar as TypeVar
import Elm.TypeInference.Unify as Unify exposing (TypeAlias)
import List.ExtraExtra
import RangeLike
import Result.Extra
import Set exposing (Set)



-- PROJECT INDEXING


{-| An indexed project: every file's imports are resolved, but nothing has
been solved yet. Cheap to build.

Keep sources up to date incrementally with [`addFile`](#addFile) and
[`removeFile`](#removeFile).

-}
type Project
    = Project
        { currentPackage : Maybe PackageName
        , depEnv : DependencyEnv
        , moduleMapping : ModuleIds.Mapping
        , modulesById : Dict ModuleId ProjectModule
        , importedBy : Dict ModuleId (Set ModuleId)
        , acc : ProjectAcc
        }


type alias LookupTable =
    { nodeIds : Dict RangeLike.RangeLike TypeI.Id
    , -- Index into `declIds` of which top-level declaration each ID is in
      declOfId : Dict TypeI.Id Int
    , declIds : Array.Array (List TypeI.Id)
    , subst : SubstitutionMap.SubstitutionMap
    , moduleMapping : ModuleIds.Mapping
    , cache : Array.Array (Maybe Type)
    , pool : Dict String Type
    }


{-| Build a [`Project`](#Project) from dependencies and project sources.

  - `directDependencies`: names of the packages listed in the project's
    `elm.json` (e.g. `"elm/core"`).
  - `allDependencies`: type information for every dependency in the closure,
    parsed from each dependency's `elm.json` and `docs.json`.
  - `sourcesToResolveAmbiguity`: Elm sources from dependencies' `ELM_HOME`
    needed to disambiguate hidden types. Start with `Dict.empty`; if you get
    `Err (NeedPackageSources needed)`, read and parse those
    files and retry with them supplied.
  - `projectPackageName`: the project's own package name (`Just` for packages,
    `Nothing` for applications).
  - `projectFiles`: the project's Elm sources, keyed by module name.

-}
init :
    { directDependencies : List PackageName
    , allDependencies : List Dependency
    , sourcesToResolveAmbiguity : Dict PackageName (List File)
    , projectPackageName : Maybe PackageName
    , projectFiles : Dict ModuleName File
    }
    -> Result ProjectError Project
init { directDependencies, allDependencies, sourcesToResolveAmbiguity, projectPackageName, projectFiles } =
    case
        buildDependencyEnv
            directDependencies
            allDependencies
            sourcesToResolveAmbiguity
    of
        Err err ->
            Err err

        Ok dep ->
            let
                files : List File
                files =
                    Dict.values projectFiles

                ( modulesReversed, missingModuleName, moduleMapping ) =
                    List.foldl
                        (\file ( acc, accMissingModuleName, accModuleMapping ) ->
                            let
                                key : ModuleName
                                key =
                                    FileExtra.moduleName file
                            in
                            if ModuleNameExtra.isNotEmpty key then
                                let
                                    ( index, newModuleMapping ) =
                                        ModuleIndex.fromFile accModuleMapping file
                                in
                                ( { key = key, index = index, file = file } :: acc
                                , accMissingModuleName
                                , newModuleMapping
                                )

                            else
                                ( acc, True, accModuleMapping )
                        )
                        ( [], False, dep.moduleMapping )
                        files
            in
            if missingModuleName then
                Err ProjectError.MissingModuleName

            else
                let
                    modulesById : Dict ModuleId ProjectModule
                    modulesById =
                        modulesReversed
                            |> List.foldl
                                (\m acc -> Dict.insert m.index.moduleId m acc)
                                Dict.empty

                    importedBy : Dict ModuleId (Set ModuleId)
                    importedBy =
                        modulesReversed
                            |> List.foldl (\m acc -> addReverseEdges m.index acc) Dict.empty
                in
                Ok
                    (Project
                        { currentPackage = projectPackageName
                        , depEnv = dep
                        , moduleMapping = moduleMapping
                        , modulesById = modulesById
                        , importedBy = importedBy
                        , acc =
                            { tables = Dict.empty
                            , interfaces = Dict.empty
                            , sccsInTopoOrder = Nothing
                            , values = dep.globalEnv
                            , aliases = dep.typeAliases
                            }
                        }
                    )


addReverseEdges : ModuleIndex -> Dict ModuleId (Set ModuleId) -> Dict ModuleId (Set ModuleId)
addReverseEdges index acc =
    index.imports
        |> List.foldl
            (\import_ innerAcc ->
                case Dict.get import_.moduleId innerAcc of
                    Just importers ->
                        Dict.insert import_.moduleId (Set.insert index.moduleId importers) innerAcc

                    Nothing ->
                        Dict.insert import_.moduleId (Set.singleton index.moduleId) innerAcc
            )
            acc


firstPartyImportsOf : Dict ModuleId ProjectModule -> ModuleId -> List ModuleId
firstPartyImportsOf modulesById moduleId =
    case Dict.get moduleId modulesById of
        Nothing ->
            []

        Just m ->
            m.index.imports
                |> List.filterMap
                    (\import_ ->
                        if Dict.member import_.moduleId modulesById then
                            Just import_.moduleId

                        else
                            Nothing
                    )


{-| Every module reachable from `start`, `start` included.
-}
importClosure : (ModuleId -> List ModuleId) -> ModuleId -> Set ModuleId
importClosure edges start =
    importClosureHelp edges [ start ] Set.empty


importClosureHelp : (ModuleId -> List ModuleId) -> List ModuleId -> Set ModuleId -> Set ModuleId
importClosureHelp edges queue visited =
    case queue of
        [] ->
            visited

        node :: rest ->
            if Set.member node visited then
                importClosureHelp edges rest visited

            else
                importClosureHelp edges (edges node ++ rest) (Set.insert node visited)


inferNodes : Set ModuleId -> Project -> Project
inferNodes nodes (Project p) =
    let
        sccsInTopoOrder : List (List ModuleId)
        sccsInTopoOrder =
            case p.acc.sccsInTopoOrder of
                Just cached ->
                    cached

                Nothing ->
                    SCC.stronglyConnectedComponents
                        (Dict.keys p.modulesById)
                        (\node -> firstPartyImportsOf p.modulesById node)

        newAcc : ProjectAcc
        newAcc =
            sccsInTopoOrder
                |> List.foldl
                    (\component acc ->
                        List.foldl
                            (\id subAcc ->
                                if not (Set.member id nodes) then
                                    subAcc

                                else if Dict.member id subAcc.interfaces then
                                    subAcc

                                else
                                    case Dict.get id p.modulesById of
                                        Just m ->
                                            inferOne p.currentPackage p.depEnv p.moduleMapping m subAcc

                                        Nothing ->
                                            subAcc
                            )
                            acc
                            component
                    )
                    p.acc
    in
    Project
        { acc =
            { tables = newAcc.tables
            , interfaces = newAcc.interfaces
            , sccsInTopoOrder = Just sccsInTopoOrder
            , values = newAcc.values
            , aliases = newAcc.aliases
            }
        , currentPackage = p.currentPackage
        , depEnv = p.depEnv
        , moduleMapping = p.moduleMapping
        , modulesById = p.modulesById
        , importedBy = p.importedBy
        }


{-| Get the [`Type`](Elm-TypeInference-Type#Type) for a given source
[`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#Range).

The computed data is cached into a new version of the [`Project`](#Project),
save it into your model to speed up future `getType` calls.

-}
getType : ModuleName -> Range -> Project -> ( Result InferError Type, Project )
getType moduleName range proj =
    case moduleData moduleName proj of
        Nothing ->
            ( Err
                { moduleName = moduleName
                , declarationNames = []
                , details = ModuleNotFound
                }
            , proj
            )

        Just m ->
            lookupRange moduleName range m (ensureInferred m proj)


{-| Fully expand the type aliases in a [`Type`](Elm-TypeInference-Type#Type).

    type alias X =
        { a : Int, b : String }

    foo : X
    foo = { a = 5, b = "hello" }

    -- Assuming you `getType` for the `foo`:
    Named
       { package = ""
       , moduleName = ["Main"]
       , name = "X"
       , arguments = []
       }

    -- You can expand it:
    expand project theTypeAbove
    -->
    Record
        { fields =
            Dict.fromList
                [ ( "a", Int )
                , ( "b", String )
                ]
        }

-}
expand : Project -> Type -> Type
expand (Project p) type_ =
    case TypeI.fromPublicType p.moduleMapping type_ of
        Nothing ->
            type_

        Just mono ->
            Unify.expandAliasDeep (knownAliases p mono) mono
                |> TypeI.toPublicType p.moduleMapping { alreadyNormalized = True }


knownAliases :
    { a
        | currentPackage : Maybe PackageName
        , depEnv : DependencyEnv
        , moduleMapping : ModuleIds.Mapping
        , modulesById : Dict ModuleId ProjectModule
        , acc : ProjectAcc
    }
    -> MonoType
    -> Dict GlobalKey TypeAlias
knownAliases p mono =
    knownAliasesHelp
        p
        (Set.toList (TypeI.moduleIdsIn mono Set.empty))
        Set.empty
        p.acc.aliases


knownAliasesHelp :
    { a
        | currentPackage : Maybe PackageName
        , depEnv : DependencyEnv
        , moduleMapping : ModuleIds.Mapping
        , modulesById : Dict ModuleId ProjectModule
        , acc : ProjectAcc
    }
    -> List ModuleId
    -> Set ModuleId
    -> Dict GlobalKey TypeAlias
    -> Dict GlobalKey TypeAlias
knownAliasesHelp p todo visited acc =
    case todo of
        [] ->
            acc

        moduleId :: rest ->
            let
                visited1 : Set ModuleId
                visited1 =
                    Set.insert moduleId visited
            in
            if Set.member moduleId visited || Dict.member moduleId p.acc.interfaces then
                knownAliasesHelp p rest visited1 acc

            else
                case Dict.get moduleId p.modulesById of
                    Nothing ->
                        knownAliasesHelp p rest visited1 acc

                    Just m ->
                        let
                            own : Dict GlobalKey TypeAlias
                            own =
                                uninferredModuleAliases p m

                            mentioned : List ModuleId
                            mentioned =
                                Dict.foldl
                                    (\_ alias_ inner -> TypeI.moduleIdsIn alias_.type_ inner)
                                    Set.empty
                                    own
                                    |> Set.toList
                        in
                        knownAliasesHelp p (mentioned ++ rest) visited1 (Dict.union own acc)


uninferredModuleAliases :
    { a
        | currentPackage : Maybe PackageName
        , depEnv : DependencyEnv
        , moduleMapping : ModuleIds.Mapping
        , modulesById : Dict ModuleId ProjectModule
        , acc : ProjectAcc
    }
    -> ProjectModule
    -> Dict GlobalKey TypeAlias
uninferredModuleAliases p m =
    let
        imported : Dict ModuleId ModuleInterface
        imported =
            m.index.imports
                |> List.foldl
                    (\import_ inner ->
                        case Dict.get import_.moduleId p.acc.interfaces of
                            Just interface ->
                                Dict.insert import_.moduleId interface inner

                            Nothing ->
                                case Dict.get import_.moduleId p.modulesById of
                                    Just other ->
                                        Dict.insert import_.moduleId
                                            { moduleIndex = other.index
                                            , exposedValues = Dict.empty
                                            , ownTypeAliases = Dict.empty
                                            }
                                            inner

                                    Nothing ->
                                        inner
                    )
                    Dict.empty

        ctx : ModuleCtx
        ctx =
            moduleCtx
                p.currentPackage
                p.depEnv
                p.moduleMapping
                p.acc.values
                p.acc.aliases
                imported
                m.index
    in
    case
        gatherTypeAliases ctx m.file
            |> State.run (State.init p.acc.values)
            |> Tuple.first
    of
        Ok own ->
            own

        Err _ ->
            Dict.empty


{-| Mostly a test helper.

Returns [`Type`](Elm-TypeInference-Type#Type)s of all AST nodes in the project.

-}
getAllTypes : ModuleName -> Project -> ( Result InferError (List ( Range, Type )), Project )
getAllTypes moduleName proj =
    case moduleData moduleName proj of
        Nothing ->
            ( Err
                { moduleName = moduleName
                , declarationNames = []
                , details = ModuleNotFound
                }
            , proj
            )

        Just m ->
            let
                (Project newP) =
                    ensureInferred m proj
            in
            case Dict.get m.key newP.acc.tables of
                Nothing ->
                    ( Err
                        { moduleName = moduleName
                        , declarationNames = []
                        , details = ModuleNotFound
                        }
                    , Project newP
                    )

                Just (Err err) ->
                    ( Err err
                    , Project newP
                    )

                Just (Ok tlt) ->
                    let
                        folded :
                            { pairs : List ( Range, Type )
                            , table : LookupTable
                            }
                        folded =
                            Dict.foldl
                                (\rangeLike id acc ->
                                    case Array.get id acc.table.cache |> Maybe.andThen identity of
                                        Just cached ->
                                            { pairs = ( RangeLike.toRange rangeLike, cached ) :: acc.pairs
                                            , table = acc.table
                                            }

                                        Nothing ->
                                            let
                                                ( pubType, newTable ) =
                                                    resolveIdToPublicType id acc.table
                                            in
                                            { pairs = ( RangeLike.toRange rangeLike, pubType ) :: acc.pairs
                                            , table = newTable
                                            }
                                )
                                { pairs = []
                                , table = tlt
                                }
                                tlt.nodeIds
                    in
                    ( Ok (List.reverse folded.pairs)
                    , Project
                        { acc =
                            { tables = Dict.insert m.key (Ok folded.table) newP.acc.tables
                            , interfaces = newP.acc.interfaces
                            , sccsInTopoOrder = newP.acc.sccsInTopoOrder
                            , values = newP.acc.values
                            , aliases = newP.acc.aliases
                            }
                        , currentPackage = newP.currentPackage
                        , depEnv = newP.depEnv
                        , moduleMapping = newP.moduleMapping
                        , modulesById = newP.modulesById
                        , importedBy = newP.importedBy
                        }
                    )


{-| Find a project module by name.
-}
moduleData : ModuleName -> Project -> Maybe ProjectModule
moduleData moduleName (Project p) =
    FullModuleName.fromModuleName moduleName
        |> Maybe.andThen (\full -> ModuleIds.getId full p.moduleMapping)
        |> Maybe.andThen (\id -> Dict.get id p.modulesById)


{-| Make sure a module's import closure is inferred - compute it if missing.
-}
ensureInferred : ProjectModule -> Project -> Project
ensureInferred m ((Project p) as proj) =
    if Dict.member m.index.moduleId p.acc.interfaces then
        proj

    else
        let
            closure : Set ModuleId
            closure =
                importClosure (\modId -> firstPartyImportsOf p.modulesById modId) m.index.moduleId
        in
        if Set.foldl (\id acc -> acc && Dict.member id p.acc.interfaces) True closure then
            proj

        else
            inferNodes closure proj


{-| Resolve one `Id` to a `Type` and cache the result.
-}
resolveIdToPublicType : TypeI.Id -> LookupTable -> ( Type, LookupTable )
resolveIdToPublicType id tlt =
    case Dict.get id tlt.declOfId of
        Nothing ->
            resolveSingleIdToPublicType id tlt

        Just declIndex ->
            case Array.get declIndex tlt.declIds of
                Nothing ->
                    resolveSingleIdToPublicType id tlt

                Just ids ->
                    let
                        newTable : LookupTable
                        newTable =
                            resolveDeclaration ids tlt
                    in
                    case Array.get id newTable.cache |> Maybe.andThen identity of
                        Just pubType ->
                            ( pubType, newTable )

                        Nothing ->
                            -- Shouldn't happen: `ids` contains `id`
                            resolveSingleIdToPublicType id newTable


resolveDeclaration : List TypeI.Id -> LookupTable -> LookupTable
resolveDeclaration ids tlt =
    let
        ( monoTypesRev, subst1 ) =
            ids
                |> List.foldl
                    (\id ( acc, subst ) ->
                        let
                            ( monoType, _, newSubst ) =
                                SubstitutionMap.substituteMono subst (TypeI.id_ id)
                        in
                        ( monoType :: acc, newSubst )
                    )
                    ( [], tlt.subst )

        ( cache1, pool1 ) =
            List.map2 Tuple.pair
                ids
                (TypeI.nameVarsTogether (hintFor subst1) (List.reverse monoTypesRev))
                |> List.foldl
                    (\( id, monoType ) ( cache, pool ) ->
                        let
                            key : String
                            key =
                                -- prefixed so as not to clash with `monoPublicKeyAlpha` keys
                                "n" ++ TypeI.monoPublicKey { alreadyNormalized = True } monoType
                        in
                        case Dict.get key pool of
                            Just canonical ->
                                ( arraySetGrowing Nothing id (Just canonical) cache
                                , pool
                                )

                            Nothing ->
                                let
                                    fresh : Type
                                    fresh =
                                        TypeI.toPublicType tlt.moduleMapping { alreadyNormalized = True } monoType
                                in
                                ( arraySetGrowing Nothing id (Just fresh) cache
                                , Dict.insert key fresh pool
                                )
                    )
                    ( tlt.cache, tlt.pool )
    in
    { nodeIds = tlt.nodeIds
    , declOfId = tlt.declOfId
    , declIds = tlt.declIds
    , subst = subst1
    , moduleMapping = tlt.moduleMapping
    , cache = cache1
    , pool = pool1
    }


resolveSingleIdToPublicType : TypeI.Id -> LookupTable -> ( Type, LookupTable )
resolveSingleIdToPublicType id tlt =
    let
        ( monoType0, _, subst1 ) =
            SubstitutionMap.substituteMono tlt.subst (TypeI.id_ id)

        monoType : MonoType
        monoType =
            case TypeI.applyNameHints (hintFor tlt.subst) (TypeI.mono monoType0) of
                TypeI.Forall _ hinted ->
                    hinted

        key : String
        key =
            TypeI.monoPublicKey { alreadyNormalized = False } monoType

        ( pubType, pool1 ) =
            case Dict.get key tlt.pool of
                Just canonical ->
                    ( canonical, tlt.pool )

                Nothing ->
                    let
                        fresh : Type
                        fresh =
                            TypeI.toPublicType tlt.moduleMapping { alreadyNormalized = False } monoType
                    in
                    ( fresh, Dict.insert key fresh tlt.pool )
    in
    ( pubType
    , { nodeIds = tlt.nodeIds
      , declOfId = tlt.declOfId
      , declIds = tlt.declIds
      , subst = subst1
      , moduleMapping = tlt.moduleMapping
      , cache = arraySetGrowing Nothing id (Just pubType) tlt.cache
      , pool = pool1
      }
    )


hintFor : SubstitutionMap.SubstitutionMap -> TypeI.Id -> TypeVar.SuperType -> Maybe String
hintFor subst id super =
    case SubstitutionMap.hintOf id subst of
        Just hint ->
            if hint.super == super then
                Just hint.name

            else
                Nothing

        Nothing ->
            Nothing


{-| Look up one range in an already-inferred project, caching the resolved type.
-}
lookupRange : ModuleName -> Range -> ProjectModule -> Project -> ( Result InferError Type, Project )
lookupRange moduleName range m ((Project p) as proj) =
    case Dict.get m.key p.acc.tables of
        Nothing ->
            ( Err
                { moduleName = moduleName
                , declarationNames = []
                , details = ModuleNotFound
                }
            , proj
            )

        Just (Err err) ->
            ( Err err
            , proj
            )

        Just (Ok tlt) ->
            let
                rangeLike : RangeLike.RangeLike
                rangeLike =
                    RangeLike.fromRange range
            in
            case Dict.get rangeLike tlt.nodeIds of
                Nothing ->
                    ( Err
                        { moduleName = moduleName
                        , declarationNames = []
                        , details = RangeNotFound
                        }
                    , proj
                    )

                Just id ->
                    case Array.get id tlt.cache |> Maybe.andThen identity of
                        Just cached ->
                            ( Ok cached
                            , proj
                            )

                        Nothing ->
                            let
                                ( pubType, newTable ) =
                                    resolveIdToPublicType id tlt
                            in
                            ( Ok pubType
                            , Project
                                { acc =
                                    { tables = Dict.insert m.key (Ok newTable) p.acc.tables
                                    , interfaces = p.acc.interfaces
                                    , sccsInTopoOrder = p.acc.sccsInTopoOrder
                                    , values = p.acc.values
                                    , aliases = p.acc.aliases
                                    }
                                , currentPackage = p.currentPackage
                                , depEnv = p.depEnv
                                , moduleMapping = p.moduleMapping
                                , modulesById = p.modulesById
                                , importedBy = p.importedBy
                                }
                            )


{-| `Array.set` no-ops when the index is out of bounds.
This function grows the array instead.

Kept inline instead of in Array.ExtraExtra: somehow it's ~4% slower there - weird!
This sits on the hottest path in the library.

-}
arraySetGrowing : a -> Int -> a -> Array.Array a -> Array.Array a
arraySetGrowing default index value array =
    let
        indexMinusLength : Int
        indexMinusLength =
            index - Array.length array
    in
    if indexMinusLength < 0 then
        Array.set index value array

    else
        Array.push value (Array.append array (Array.repeat indexMinusLength default))


{-| Remove each module (transitively) reachable from the supplied modules.
-}
invalidate : Dict ModuleId ProjectModule -> Set ModuleId -> Project -> Project
invalidate modulesById directlyAffected (Project p) =
    let
        affected : Set ModuleId
        affected =
            importClosureHelp
                (\id ->
                    case Dict.get id p.importedBy of
                        Just importers ->
                            Set.toList importers

                        Nothing ->
                            []
                )
                (Set.toList directlyAffected)
                Set.empty

        interfaces : Dict ModuleId ModuleInterface
        interfaces =
            Set.foldl Dict.remove p.acc.interfaces affected

        ( valuesWithoutAffected, aliasesWithoutAffected ) =
            affected
                |> Set.foldl
                    (\id ( valuesAcc, aliasesAcc ) ->
                        case Dict.get id p.acc.interfaces of
                            Just interface ->
                                ( Dict.foldl (\name _ inner -> Dict.remove ( id, "", name ) inner) valuesAcc interface.exposedValues
                                , Dict.foldl (\key _ inner -> Dict.remove key inner) aliasesAcc interface.ownTypeAliases
                                )

                            Nothing ->
                                ( valuesAcc, aliasesAcc )
                    )
                    ( p.acc.values, p.acc.aliases )

        tablesWithoutAffected : Dict ModuleName (Result InferError LookupTable)
        tablesWithoutAffected =
            affected
                |> Set.foldl
                    (\id acc ->
                        case Dict.get id modulesById of
                            Just m ->
                                Dict.remove m.key acc

                            Nothing ->
                                acc
                    )
                    p.acc.tables
    in
    Project
        { acc =
            { tables = tablesWithoutAffected
            , interfaces = interfaces
            , sccsInTopoOrder = Nothing
            , values = valuesWithoutAffected
            , aliases = aliasesWithoutAffected
            }
        , currentPackage = p.currentPackage
        , depEnv = p.depEnv
        , moduleMapping = p.moduleMapping
        , modulesById = p.modulesById
        , importedBy = p.importedBy
        }


{-| A [`Project`](#Project) with no dependencies and no files, not even
`elm/core`.

Handy for tests and mocks. Add modules with [`addFile`](#addFile).

-}
empty : Project
empty =
    Project
        { currentPackage = Nothing
        , depEnv =
            { globalEnv = Dict.empty
            , typeAliases = Dict.empty
            , index = Tuple.first (ModuleLookup.buildIndex ModuleIds.empty Dict.empty)
            , moduleMapping = ModuleIds.empty
            }
        , moduleMapping = ModuleIds.empty
        , modulesById = Dict.empty
        , importedBy = Dict.empty
        , acc =
            { tables = Dict.empty
            , interfaces = Dict.empty
            , sccsInTopoOrder = Nothing
            , values = Dict.empty
            , aliases = Dict.empty
            }
        }


{-| Make `Project` aware of a new or changed file.

The module and everything that (transitively) imports it is invalidated
behind the scenes and will be re-inferred lazily by [`getType`](#getType).

-}
addFile : File -> Project -> Result ProjectError Project
addFile file (Project p) =
    let
        moduleName : ModuleName
        moduleName =
            FileExtra.moduleName file
    in
    case moduleName of
        [] ->
            Err ProjectError.MissingModuleName

        _ :: _ ->
            let
                ( newIndex, moduleMapping1 ) =
                    ModuleIndex.fromFile p.moduleMapping file

                id : ModuleId
                id =
                    newIndex.moduleId

                oldImportIds : Set ModuleId
                oldImportIds =
                    case Dict.get id p.modulesById of
                        Just old ->
                            Set.fromList (List.map .moduleId old.index.imports)

                        Nothing ->
                            Set.empty

                newImportIds : Set ModuleId
                newImportIds =
                    Set.fromList (List.map .moduleId newIndex.imports)

                importedByWithoutOldImports : Dict ModuleId (Set ModuleId)
                importedByWithoutOldImports =
                    Set.diff oldImportIds newImportIds
                        |> Set.foldl
                            (\importId acc ->
                                case Dict.get importId acc of
                                    Just by ->
                                        Dict.insert importId (Set.remove id by) acc

                                    Nothing ->
                                        acc
                            )
                            p.importedBy

                importedByWithNewImports : Dict ModuleId (Set ModuleId)
                importedByWithNewImports =
                    Set.diff newImportIds oldImportIds
                        |> Set.foldl
                            (\importId acc ->
                                case Dict.get importId acc of
                                    Nothing ->
                                        Dict.insert importId (Set.singleton id) acc

                                    Just importers ->
                                        Dict.insert importId (Set.insert id importers) acc
                            )
                            importedByWithoutOldImports

                modulesByIdWithFile : Dict ModuleId ProjectModule
                modulesByIdWithFile =
                    Dict.insert id
                        { key = moduleName
                        , index = newIndex
                        , file = file
                        }
                        p.modulesById
            in
            Ok
                (invalidate modulesByIdWithFile
                    (Set.singleton id)
                    (Project
                        { moduleMapping = moduleMapping1
                        , modulesById = modulesByIdWithFile
                        , importedBy = importedByWithNewImports
                        , acc = p.acc
                        , currentPackage = p.currentPackage
                        , depEnv = p.depEnv
                        }
                    )
                )


{-| Remove a module from a `Project`.

Everything that (transitively) imported it is invalidated behind the scenes
and will be re-inferred lazily by [`getType`](#getType).

-}
removeFile : ModuleName -> Project -> Project
removeFile moduleName ((Project p) as proj) =
    case
        FullModuleName.fromModuleName moduleName
            |> Maybe.andThen (\full -> ModuleIds.getId full p.moduleMapping)
            |> Maybe.andThen (\id -> Dict.get id p.modulesById |> Maybe.map (\mod -> ( id, mod )))
    of
        Nothing ->
            proj

        Just ( id, m ) ->
            let
                modulesByIdWithoutFile : Dict ModuleId ProjectModule
                modulesByIdWithoutFile =
                    Dict.remove id p.modulesById

                importedByWithoutFile : Dict ModuleId (Set ModuleId)
                importedByWithoutFile =
                    m.index.imports
                        |> List.foldl
                            (\import_ acc ->
                                case Dict.get import_.moduleId acc of
                                    Just by ->
                                        Dict.insert import_.moduleId (Set.remove id by) acc

                                    Nothing ->
                                        acc
                            )
                            p.importedBy
            in
            invalidate p.modulesById
                (Set.singleton id)
                (Project
                    { modulesById = modulesByIdWithoutFile
                    , importedBy = importedByWithoutFile
                    , acc = p.acc
                    , depEnv = p.depEnv
                    , currentPackage = p.currentPackage
                    , moduleMapping = p.moduleMapping
                    }
                )


{-| A dependency package with its type information, parsed from the dependency's
`elm.json` (via
[`Elm.Project.decoder`](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project#decoder))
and `docs.json` (via `elm/project-metadata-utils`
[`Elm.Docs.decoder`](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Docs#decoder)):

  - **name:** the package identifier (e.g. `"elm/html"`).
  - **dependencies:** names of the package's _immediate_ `elm.json` dependencies (eg. `"elm/virtual-dom"`).
  - **modules:** the decoded `docs.json` modules.

-}
type alias Dependency =
    { name : PackageName
    , dependencies : List PackageName
    , modules : List Elm.Docs.Module
    }


type alias DependencyEnv =
    { globalEnv : Dict GlobalKey TypeI.Type
    , typeAliases : Dict GlobalKey TypeAlias
    , index : ModuleLookup.Index
    , moduleMapping : ModuleIds.Mapping
    }


{-| Returns error `NeedPackageSources needed` when dependencies' `docs.json`
types mention modules whose shapes need the packages' Elm sources. An example
`needed`:

    Dict.fromList
        [ ( "example/css", [ "src/Css/Internal.elm" ] ) ]

-}
buildDependencyEnv :
    List PackageName
    -> List Dependency
    -> Dict PackageName (List File)
    -> Result ProjectError DependencyEnv
buildDependencyEnv directDependencies allDependencies sourcesToResolveAmbiguity =
    let
        deps : Dependencies
        deps =
            Dependencies.fromList allDependencies

        reachable : Set PackageName
        reachable =
            reachablePackages deps directDependencies

        reachableDependencies : List Dependency
        reachableDependencies =
            List.filter (\pkg -> Set.member pkg.name reachable) allDependencies

        reachableDeps : Dependencies
        reachableDeps =
            Dependencies.fromList reachableDependencies

        needed : Dict PackageName (List String)
        needed =
            DependencySources.neededSources reachableDeps sourcesToResolveAmbiguity
                |> Dict.fromList
    in
    if Dict.isEmpty needed then
        buildDependencyEnvHelp
            directDependencies
            sourcesToResolveAmbiguity
            deps
            reachableDependencies
            reachableDeps

    else
        Err (ProjectError.NeedPackageSources needed)


buildDependencyEnvHelp :
    List PackageName
    -> Dict PackageName (List File)
    -> Dependencies
    -> List Dependency
    -> Dependencies
    -> Result ProjectError DependencyEnv
buildDependencyEnvHelp directDependencies sourcesToResolveAmbiguity deps reachableDependencies reachableDeps =
    let
        directVisibleDeps : Dependencies
        directVisibleDeps =
            reachableDependencies
                |> List.filter (\pkg -> List.member pkg.name directDependencies)
                |> Dependencies.fromList

        depModuleNames : List FullModuleName
        depModuleNames =
            (reachableDependencies
                |> List.ExtraExtra.fastConcatMap (\pkg -> List.map (\m -> FullModuleName.fromDotted m.name) pkg.modules)
            )
                ++ (DependencySources.referencedModules reachableDeps
                        |> List.map FullModuleName.fromDotted
                   )

        moduleMappingWithDepNames : ModuleIds.Mapping
        moduleMappingWithDepNames =
            List.foldl (\name acc -> ModuleIds.intern name acc |> Tuple.second) ModuleIds.empty depModuleNames

        ( depIndex, moduleMappingWithDepIndex ) =
            ModuleLookup.buildIndex moduleMappingWithDepNames directVisibleDeps

        baseEnv : Result ProjectError DependencyEnv
        baseEnv =
            Dependencies.register moduleMappingWithDepIndex reachableDeps
                |> Result.map
                    (\registered ->
                        { globalEnv = registered.globalEnv
                        , typeAliases = registered.typeAliases
                        , index = depIndex
                        , moduleMapping = registered.moduleMapping
                        }
                    )
    in
    case baseEnv of
        Err err ->
            Err err

        Ok env ->
            case DependencySources.aliases env.moduleMapping deps sourcesToResolveAmbiguity of
                Err err ->
                    Err err

                Ok ( sourceAliases, moduleMapping2 ) ->
                    Ok
                        { globalEnv = env.globalEnv
                        , index = env.index
                        , typeAliases = Dict.union sourceAliases env.typeAliases
                        , moduleMapping = moduleMapping2
                        }


reachablePackages : Dependencies -> List PackageName -> Set PackageName
reachablePackages deps roots =
    reachablePackagesHelp deps roots Set.empty


reachablePackagesHelp : Dependencies -> List PackageName -> Set PackageName -> Set PackageName
reachablePackagesHelp deps queue seen =
    case queue of
        [] ->
            seen

        name :: rest ->
            if Set.member name seen then
                reachablePackagesHelp deps rest seen

            else
                case Dict.get name deps of
                    Nothing ->
                        reachablePackagesHelp deps rest (Set.insert name seen)

                    Just pkg ->
                        reachablePackagesHelp deps (rest ++ pkg.dependencies) (Set.insert name seen)


{-| What one module contributes to the modules that import it.
-}
type alias ModuleInterface =
    { moduleIndex : ModuleIndex
    , exposedValues : Dict VarName TypeI.Type
    , ownTypeAliases : Dict GlobalKey TypeAlias
    }


type alias ProjectModule =
    { key : ModuleName
    , index : ModuleIndex
    , file : File
    }


type alias ProjectAcc =
    { tables : Dict ModuleName (Result InferError LookupTable)
    , interfaces : Dict ModuleId ModuleInterface
    , sccsInTopoOrder : Maybe (List (List ModuleId))
    , values : Dict GlobalKey TypeI.Type
    , aliases : Dict GlobalKey TypeAlias
    }


inferOne : Maybe PackageName -> DependencyEnv -> ModuleIds.Mapping -> ProjectModule -> ProjectAcc -> ProjectAcc
inferOne currentPackage depEnv moduleMapping m acc =
    let
        imported : Dict ModuleId ModuleInterface
        imported =
            m.index.imports
                |> List.foldl
                    (\import_ inner ->
                        case Dict.get import_.moduleId acc.interfaces of
                            Just interface ->
                                Dict.insert import_.moduleId interface inner

                            Nothing ->
                                inner
                    )
                    Dict.empty
    in
    case
        inferModule_
            currentPackage
            depEnv
            moduleMapping
            acc.values
            acc.aliases
            imported
            m.index
            m.file
    of
        Ok { table, interface } ->
            { tables = Dict.insert m.key (Ok table) acc.tables
            , interfaces = Dict.insert m.index.moduleId interface acc.interfaces
            , sccsInTopoOrder = acc.sccsInTopoOrder
            , values =
                Dict.foldl
                    (\name scheme inner -> Dict.insert ( m.index.moduleId, "", name ) scheme inner)
                    acc.values
                    interface.exposedValues
            , aliases = Dict.union interface.ownTypeAliases acc.aliases
            }

        Err err ->
            { tables = Dict.insert m.key (Err err) acc.tables
            , interfaces =
                Dict.insert m.index.moduleId
                    { moduleIndex = m.index
                    , exposedValues = Dict.empty
                    , ownTypeAliases = Dict.empty
                    }
                    acc.interfaces
            , sccsInTopoOrder = acc.sccsInTopoOrder
            , values = acc.values
            , aliases = acc.aliases
            }


type alias ModuleCtx =
    { thisIndex : ModuleIndex
    , modules : Dict ModuleId ModuleIndex
    , resolver : TypeResolver
    , index : ModuleLookup.Index
    , moduleMapping : ModuleIds.Mapping
    , -- project-wide (see `ProjectAcc`): dependencies + already inferred modules
      aliases : Dict GlobalKey TypeAlias
    , values : Dict GlobalKey TypeI.Type
    , allowKernel : Bool
    }


allowsKernel : Maybe PackageName -> Bool
allowsKernel currentPackage =
    case currentPackage of
        Nothing ->
            True

        Just name ->
            String.startsWith "elm/" name
                || String.startsWith "elm-explorations/" name


moduleCtx :
    Maybe PackageName
    -> DependencyEnv
    -> ModuleIds.Mapping
    -> Dict GlobalKey TypeI.Type
    -> Dict GlobalKey TypeAlias
    -> Dict ModuleId ModuleInterface
    -> ModuleIndex
    -> ModuleCtx
moduleCtx currentPackage depEnv moduleMapping values aliases importedInterfaces thisIndex =
    let
        modules : Dict ModuleId ModuleIndex
        modules =
            importedInterfaces
                |> Dict.map (\_ interface -> interface.moduleIndex)
                |> Dict.insert thisIndex.moduleId thisIndex
    in
    { thisIndex = thisIndex
    , modules = modules
    , resolver =
        ModuleLookup.typeResolverFor
            moduleMapping
            depEnv.index
            modules
            thisIndex
    , index = depEnv.index
    , moduleMapping = moduleMapping
    , aliases = aliases
    , values = values
    , allowKernel = allowsKernel currentPackage
    }


inferModule_ :
    Maybe PackageName
    -> DependencyEnv
    -> ModuleIds.Mapping
    -> Dict GlobalKey TypeI.Type
    -> Dict GlobalKey TypeAlias
    -> Dict ModuleId ModuleInterface
    -> ModuleIndex
    -> File
    -> Result InferError { table : LookupTable, interface : ModuleInterface }
inferModule_ currentPackage depEnv moduleMapping values aliases importedInterfaces thisIndex file =
    let
        ctx : ModuleCtx
        ctx =
            moduleCtx
                currentPackage
                depEnv
                moduleMapping
                values
                aliases
                importedInterfaces
                thisIndex
    in
    (State.do (gatherTypeAliases ctx file) <|
        \outgoingAliases ->
            let
                -- `outgoingAliases` are only this module's own (small)
                typeAliases : Dict GlobalKey TypeAlias
                typeAliases =
                    Dict.union outgoingAliases ctx.aliases
            in
            State.do (registerConstructorsAndPorts ctx file) <|
                \() ->
                    State.do (registerEffectMagic ctx) <|
                        \() ->
                            State.do (solveModule ctx typeAliases file) <|
                                \() ->
                                    State.do (moduleResult ctx outgoingAliases file) <|
                                        \result ->
                                            State.pure result
    )
        |> State.run (State.init ctx.values)
        |> Tuple.first


moduleResult :
    ModuleCtx
    -> Dict GlobalKey TypeAlias
    -> File
    ->
        StateM
            { table : LookupTable
            , interface : ModuleInterface
            }
moduleResult ctx outgoingAliases file =
    State.do State.createdIdCount <|
        \nextId ->
            State.do State.getNodeIds <|
                \nodeIds ->
                    State.do State.getSubst <|
                        \substitutionMap ->
                            State.do State.getGlobalEnv <|
                                \globalEnv ->
                                    let
                                        byDeclaration :
                                            { declOfId : Dict TypeI.Id Int
                                            , declIds : Array.Array (List TypeI.Id)
                                            }
                                        byDeclaration =
                                            groupByDeclaration
                                                (List.map (\(Node range _) -> RangeLike.fromRange range) file.declarations)
                                                nodeIds

                                        exposedValues : Dict VarName TypeI.Type
                                        exposedValues =
                                            ctx.thisIndex.exposedValues
                                                |> Set.foldl
                                                    (\name acc ->
                                                        case Dict.get ( ctx.thisIndex.moduleId, "", name ) globalEnv of
                                                            Just scheme ->
                                                                Dict.insert name
                                                                    (scheme
                                                                        |> TypeI.applyNameHints (hintFor substitutionMap)
                                                                        |> TypeI.normalize
                                                                    )
                                                                    acc

                                                            Nothing ->
                                                                acc
                                                    )
                                                    Dict.empty
                                    in
                                    State.pure
                                        { table =
                                            { nodeIds = nodeIds
                                            , declOfId = byDeclaration.declOfId
                                            , declIds = byDeclaration.declIds
                                            , subst = SubstitutionMap.forLookup substitutionMap
                                            , moduleMapping = ctx.moduleMapping
                                            , -- We preallocate so `getAllTypes` never needs to grow the array.
                                              cache = Array.repeat nextId Nothing
                                            , pool = Dict.empty
                                            }
                                        , interface =
                                            { moduleIndex = ctx.thisIndex
                                            , exposedValues = exposedValues
                                            , ownTypeAliases = outgoingAliases
                                            }
                                        }


groupByDeclaration :
    List RangeLike.RangeLike
    -> Dict RangeLike.RangeLike TypeI.Id
    ->
        { declOfId : Dict TypeI.Id Int
        , declIds : Array.Array (List TypeI.Id)
        }
groupByDeclaration declRanges nodeIds =
    let
        dropFinished : Int -> List ( Int, RangeLike.RangeLike ) -> List ( Int, RangeLike.RangeLike )
        dropFinished start decls =
            case decls of
                ( _, ( _, declEnd ) ) :: rest ->
                    if declEnd < start then
                        dropFinished start rest

                    else
                        decls

                [] ->
                    decls

        grouped :
            { remaining : List ( Int, RangeLike.RangeLike )
            , declOfId : Dict TypeI.Id Int
            , idsRev : Dict Int (List TypeI.Id)
            }
        grouped =
            nodeIds
                |> Dict.foldl
                    (\( start, end ) id acc ->
                        let
                            remaining : List ( Int, RangeLike.RangeLike )
                            remaining =
                                dropFinished start acc.remaining
                        in
                        case remaining of
                            ( declIndex, ( declStart, declEnd ) ) :: _ ->
                                if
                                    (declStart <= start)
                                        && (end <= declEnd)
                                        && not (Dict.member id acc.declOfId)
                                then
                                    { remaining = remaining
                                    , declOfId = Dict.insert id declIndex acc.declOfId
                                    , idsRev = Dict.update declIndex (\ids -> Just (id :: Maybe.withDefault [] ids)) acc.idsRev
                                    }

                                else
                                    { remaining = remaining
                                    , declOfId = acc.declOfId
                                    , idsRev = acc.idsRev
                                    }

                            [] ->
                                { remaining = remaining
                                , declOfId = acc.declOfId
                                , idsRev = acc.idsRev
                                }
                    )
                    { remaining = List.indexedMap Tuple.pair declRanges
                    , declOfId = Dict.empty
                    , idsRev = Dict.empty
                    }
    in
    { declOfId = grouped.declOfId
    , declIds =
        declRanges
            |> List.indexedMap
                (\declIndex declRange ->
                    let
                        ids : List TypeI.Id
                        ids =
                            Dict.get declIndex grouped.idsRev
                                |> Maybe.withDefault []
                                |> List.reverse
                    in
                    -- the declaration's own type first: it gets the nicest names
                    case Dict.get declRange nodeIds of
                        Just ownId ->
                            if List.member ownId ids then
                                ownId :: List.filter (\id -> id /= ownId) ids

                            else
                                ids

                        Nothing ->
                            ids
                )
            |> Array.fromList
    }


solveModule :
    ModuleCtx
    -> Dict GlobalKey TypeAlias
    -> File
    -> StateM ()
solveModule ctx typeAliases file =
    let
        topLevelFunctions : Dict VarName ( Node Declaration, Expression.Function )
        topLevelFunctions =
            file.declarations
                |> List.foldl
                    (\((Node _ decl) as declNode) byName ->
                        case decl of
                            Declaration.FunctionDeclaration fn ->
                                Dict.insert (Elm.Syntax.Expression.Extra.functionName fn)
                                    ( declNode, fn )
                                    byName

                            _ ->
                                byName
                    )
                    Dict.empty

        referencedNamesByFunction : Dict VarName (List ( ModuleName, VarName ))
        referencedNamesByFunction =
            topLevelFunctions
                |> Dict.map
                    (\_ ( _, fn ) ->
                        Elm.Syntax.Expression.Extra.referencedNames (Node.value (Node.value fn.declaration).expression)
                            |> List.map
                                (\( maybeModuleName, varName ) ->
                                    case maybeModuleName of
                                        Just moduleName ->
                                            ( moduleName, varName )

                                        Nothing ->
                                            ( [], varName )
                                )
                    )

        resolvedVars : Dict ( ModuleName, VarName ) (Result InferErrorDetails (Maybe ( PackageName, ModuleId )))
        resolvedVars =
            Dict.foldl
                (\_ refs acc ->
                    List.foldl
                        (\(( qualifier, varName ) as ref) inner ->
                            if Dict.member ref inner then
                                inner

                            else
                                Dict.insert ref
                                    (ModuleLookup.moduleOfVar
                                        ctx.moduleMapping
                                        ctx.index
                                        ctx.modules
                                        ctx.thisIndex
                                        (FullModuleName.fromModuleName qualifier)
                                        varName
                                    )
                                    inner
                        )
                        acc
                        refs
                )
                Dict.empty
                referencedNamesByFunction

        edges : VarName -> List VarName
        edges key =
            case Dict.get key referencedNamesByFunction of
                Nothing ->
                    []

                Just refs ->
                    refs
                        -- Resolve operator aliases to the underlying functions
                        |> List.filterMap
                            (\(( _, varName ) as ref) ->
                                case Dict.get ref resolvedVars of
                                    Just (Ok (Just ( "", moduleId ))) ->
                                        let
                                            ( resolvedModule, resolvedName ) =
                                                case
                                                    ModuleLookup.resolveOperatorFunction
                                                        ctx.moduleMapping
                                                        ctx.modules
                                                        moduleId
                                                        varName
                                                of
                                                    Ok (Just resolved) ->
                                                        resolved

                                                    Ok Nothing ->
                                                        ( moduleId, varName )

                                                    Err _ ->
                                                        ( moduleId, varName )
                                        in
                                        if
                                            (resolvedModule == ctx.thisIndex.moduleId)
                                                && Dict.member resolvedName topLevelFunctions
                                        then
                                            Just resolvedName

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )

        sccs : List (List VarName)
        sccs =
            SCC.stronglyConnectedComponents (Dict.keys topLevelFunctions) edges

        inferCtx : Infer.Ctx
        inferCtx =
            { modules = ctx.modules
            , thisModule = ctx.thisIndex
            , typeAliases = typeAliases
            , index = ctx.index
            , allowKernel = ctx.allowKernel
            , moduleMapping = ctx.moduleMapping
            , resolvedVars = resolvedVars
            , rigidTypeVars = Dict.empty
            }
    in
    sccs
        |> State.traverseUnit
            (\group ->
                group
                    |> List.filterMap (\key -> Dict.get key topLevelFunctions)
                    |> State.traverse (\( declNode, fn ) -> Infer.topLevelMember inferCtx declNode fn)
                    |> State.andThen
                        (\inferredMembers ->
                            inferredMembers
                                |> BindingGroup.solveGroup
                                    (Infer.unifyConfigForGroup inferCtx group)
                        )
            )



-- REGISTERING A MODULE'S DECLARATIONS


gatherTypeAliases : ModuleCtx -> File -> StateM (Dict GlobalKey TypeAlias)
gatherTypeAliases ctx file =
    let
        resolver : TypeResolver
        resolver =
            ctx.resolver

        moduleName : FullModuleName
        moduleName =
            ctx.thisIndex.moduleName

        moduleId : ModuleId
        moduleId =
            ctx.thisIndex.moduleId
    in
    file.declarations
        |> State.foldl
            (\(Node _ declarationNode) accAcrossDeclarations ->
                case declarationNode of
                    Declaration.AliasDeclaration typeAlias ->
                        let
                            toError : InferErrorDetails -> InferError
                            toError details =
                                { moduleName = FullModuleName.toModuleName moduleName
                                , declarationNames = [ Node.value typeAlias.name ]
                                , details = details
                                }

                            type_ : StateM MonoType
                            type_ =
                                case
                                    typeAlias.typeAnnotation
                                        |> Node.value
                                        |> TypeI.fromTypeAnnotation resolver
                                of
                                    Err fromTypeAnnotationError ->
                                        State.error (toError (TypeI.fromTypeAnnotationError fromTypeAnnotationError))

                                    Ok aliasedType ->
                                        State.pure aliasedType

                            -- A record type alias also gets a constructor function
                            -- (eg. `type alias Foo = { a : Int }` lets you write `Foo 1`).
                            registerConstructor : MonoType -> StateM ()
                            registerConstructor aliasMono =
                                case Node.value typeAlias.typeAnnotation of
                                    TypeAnnotation.Record fields ->
                                        fields
                                            |> State.traverse
                                                (\(Node _ ( _, Node _ fieldType )) ->
                                                    case TypeI.fromTypeAnnotation resolver fieldType of
                                                        Err fromTypeAnnotationError ->
                                                            State.error (toError (TypeI.fromTypeAnnotationError fromTypeAnnotationError))

                                                        Ok fieldValueType ->
                                                            State.pure fieldValueType
                                                )
                                            |> State.map
                                                (\fieldTypes ->
                                                    fieldTypes
                                                        |> List.foldr (\fieldT acc -> Function { from = fieldT, to = acc }) aliasMono
                                                )
                                            |> State.andThen
                                                (\ctorType ->
                                                    State.addGlobalBinding
                                                        ( moduleId, "", Node.value typeAlias.name )
                                                        (TypeI.closeOver ctorType)
                                                )

                                    _ ->
                                        State.pureUnit
                        in
                        State.do type_ <|
                            \type__ ->
                                State.do (registerConstructor type__) <|
                                    \() ->
                                        State.pure <|
                                            Dict.insert
                                                ( moduleId, "", Node.value typeAlias.name )
                                                { args = List.map (\(Node.Node _ generic) -> TypeVar.parse generic) typeAlias.generics
                                                , type_ = type__
                                                }
                                                accAcrossDeclarations

                    _ ->
                        State.pure accAcrossDeclarations
            )
            Dict.empty


registerConstructorsAndPorts : ModuleCtx -> File -> StateM ()
registerConstructorsAndPorts ctx file =
    file.declarations
        |> State.traverseUnit
            (\(Node _ declNode) ->
                case declNode of
                    Declaration.CustomTypeDeclaration customType ->
                        registerCustomType
                            ctx.resolver
                            ctx.thisIndex.moduleId
                            ctx.thisIndex.moduleName
                            customType

                    Declaration.PortDeclaration sig ->
                        registerPort
                            ctx.resolver
                            ctx.thisIndex.moduleId
                            ctx.thisIndex.moduleName
                            sig

                    _ ->
                        State.pureUnit
            )


registerCustomType :
    TypeResolver
    -> ModuleId
    -> FullModuleName
    -> SyntaxType.Type
    -> StateM ()
registerCustomType resolver moduleId moduleName customType =
    let
        typeName : String
        typeName =
            Node.value customType.name

        resultType : MonoType
        resultType =
            UserDefinedType
                { package = ""
                , moduleId = moduleId
                , name = typeName
                , args =
                    customType.generics
                        |> List.map
                            (\(Node _ g) ->
                                TypeVar
                                    (TypeVar.parse g)
                            )
                }
    in
    customType.constructors
        |> State.traverseUnit
            (\(Node _ { arguments, name }) ->
                let
                    argTypes : Result FromTypeAnnotationError (List MonoType)
                    argTypes =
                        arguments
                            |> Result.Extra.combineMap
                                (\(Node.Node _ arg) -> TypeI.fromTypeAnnotation resolver arg)
                in
                case argTypes of
                    Err fromTypeAnnotationError ->
                        State.error
                            { moduleName = FullModuleName.toModuleName moduleName
                            , declarationNames = [ typeName ]
                            , details = TypeI.fromTypeAnnotationError fromTypeAnnotationError
                            }

                    Ok args ->
                        let
                            ctorType : MonoType
                            ctorType =
                                List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType args
                        in
                        State.addGlobalBinding ( moduleId, "", Node.value name ) (TypeI.closeOver ctorType)
            )


registerPort : TypeResolver -> ModuleId -> FullModuleName -> Signature -> StateM ()
registerPort resolver moduleId moduleName sig =
    sig.typeAnnotation
        |> Node.value
        |> TypeI.fromTypeAnnotation resolver
        |> Result.mapError
            (\fromTypeAnnotationError ->
                State.error
                    { moduleName = FullModuleName.toModuleName moduleName
                    , declarationNames = [ Node.value sig.name ]
                    , details = TypeI.fromTypeAnnotationError fromTypeAnnotationError
                    }
            )
        |> Result.map
            (\t ->
                State.addGlobalBinding
                    ( moduleId, "", Node.value sig.name )
                    (TypeI.closeOver t)
            )
        |> Result.Extra.merge


{-| Register the magic `command` / `subscription` values for `effect module`s.

The Elm compiler magically provides:

    command : MyCmd msg -> Cmd msg

    subscription : MySub msg -> Sub msg

`MyCmd` / `MySub` are the custom types named in the module header

    effect module Random where { command = MyCmd } exposing (..)

-}
registerEffectMagic : ModuleCtx -> StateM ()
registerEffectMagic ctx =
    if ctx.allowKernel then
        State.do (registerEffectCommand ctx) <|
            \() ->
                registerEffectSubscription ctx

    else
        State.pureUnit


registerEffectCommand : ModuleCtx -> StateM ()
registerEffectCommand ctx =
    case ctx.thisIndex.effectCommand of
        Nothing ->
            State.pureUnit

        Just myCmdName ->
            case ctx.resolver [] "Cmd" of
                Err _ ->
                    State.pureUnit

                Ok ( cmdPackage, cmdModuleId ) ->
                    let
                        msgVar : MonoType
                        msgVar =
                            TypeVar (TypeVar.parse "msg")

                        magicType : MonoType
                        magicType =
                            Function
                                { from =
                                    UserDefinedType
                                        { package = ""
                                        , moduleId = ctx.thisIndex.moduleId
                                        , name = myCmdName
                                        , args = [ msgVar ]
                                        }
                                , to =
                                    UserDefinedType
                                        { package = cmdPackage
                                        , moduleId = cmdModuleId
                                        , name = "Cmd"
                                        , args = [ msgVar ]
                                        }
                                }
                    in
                    State.addGlobalBinding
                        ( ctx.thisIndex.moduleId, "", ModuleIndex.effectCommandVar )
                        (TypeI.closeOver magicType)


registerEffectSubscription : ModuleCtx -> StateM ()
registerEffectSubscription ctx =
    case ctx.thisIndex.effectSubscription of
        Nothing ->
            State.pureUnit

        Just mySubName ->
            case ctx.resolver [] "Sub" of
                Err _ ->
                    State.pureUnit

                Ok ( subPackage, subModuleId ) ->
                    let
                        msgVar : MonoType
                        msgVar =
                            TypeVar (TypeVar.parse "msg")

                        magicType : MonoType
                        magicType =
                            Function
                                { from =
                                    UserDefinedType
                                        { package = ""
                                        , moduleId = ctx.thisIndex.moduleId
                                        , name = mySubName
                                        , args = [ msgVar ]
                                        }
                                , to =
                                    UserDefinedType
                                        { package = subPackage
                                        , moduleId = subModuleId
                                        , name = "Sub"
                                        , args = [ msgVar ]
                                        }
                                }
                    in
                    State.addGlobalBinding
                        ( ctx.thisIndex.moduleId, "", ModuleIndex.effectSubscriptionVar )
                        (TypeI.closeOver magicType)
