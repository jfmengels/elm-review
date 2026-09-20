module Elm.TypeInference exposing
    ( dependencyEnv, DependencyEnvOutcome(..), DependencyEnv, Dependency
    , project, Project
    , inferModule, inferModules
    , ModuleInterface, ProjectAcc, inferModule_, inferOne
    )

{-| Type inference for
[`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
ASTs.

Optimized for lazy queries `Range -> Maybe Type`.

The process:

  - Convert `elm.json` and dependencies' `elm.json` + `docs.json` into
    [`DependencyEnv`](#DependencyEnv).
  - Load the
    [`File`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-File#File)s
    into a [`Project`](#Project).
  - (When it's clear you need it) Infer a module with
    [`inferModule`](#inferModule), producing [`TypeLookupTable`](TypeLookupTable#TypeLookupTable).
  - (When it's clear you need it) Get a [`Type`](Elm-TypeInference-Type#Type)
    for a given AST
    [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Node#Node)'s
    [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#Range) with [`get`](TypeLookupTable#get).

@docs dependencyEnv, DependencyEnvOutcome, DependencyEnv, Dependency

@docs project, Project

@docs inferModule, inferModules

-}

import Array
import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as SyntaxType
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Dependencies as Dependencies exposing (Dependencies)
import Elm.TypeInference.DependencySources as DependencySources
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError)
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (GlobalKey, StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType(..), TypeResolver)
import Elm.TypeInference.TypeVar as TypeVar
import Elm.TypeInference.Unify exposing (TypeAlias)
import List.ExtraExtra
import RangeLike
import Result.Extra
import Set exposing (Set)
import TypeLookupTable exposing (TypeLookupTable)
import TypeLookupTable.Internal



-- PROJECT INDEXING


{-| An indexed project: every file has been assigned a `ModuleId` and its
imports resolved, but nothing has been solved yet. Cheap to build.
-}
type Project
    = Project
        { currentPackage : Maybe PackageName
        , depEnv : DependencyEnv
        , moduleMapping : ModuleIds.Mapping
        , byName : Dict ModuleId ProjectModule
        , acc : ProjectAcc
        }


{-| Analyze the import graph of the project source code, producing a [`Project`](#Project).
-}
project : Maybe PackageName -> DependencyEnv -> Dict ModuleName File -> Result Error Project
project currentPackage depEnv files =
    -- Index files, assign ModuleIds, build the import graph.
    let
        (DependencyEnv dep) =
            depEnv

        ( modules, moduleMapping ) =
            Dict.foldl
                (\key file ( acc, accModuleMapping ) ->
                    case FullModuleName.fromModuleName key of
                        Nothing ->
                            ( acc, accModuleMapping )

                        Just _ ->
                            let
                                ( index, newModuleMapping ) =
                                    ModuleIndex.fromFile accModuleMapping file
                            in
                            ( { key = key, index = index, file = file } :: acc, newModuleMapping )
                )
                ( [], dep.moduleMapping )
                files
                |> (\( reversed, finalModuleMapping ) -> ( List.reverse reversed, finalModuleMapping ))

        missingModuleName : Bool
        missingModuleName =
            List.length modules /= Dict.size files
    in
    if missingModuleName then
        Err
            { moduleName = [ "<Missing>" ]
            , declarationNames = []
            , details = MissingModuleName
            }

    else
        let
            byName : Dict ModuleId ProjectModule
            byName =
                modules
                    |> List.foldl
                        (\m acc -> Dict.insert m.index.moduleId m acc)
                        Dict.empty
        in
        Ok
            (Project
                { currentPackage = currentPackage
                , depEnv = depEnv
                , moduleMapping = moduleMapping
                , byName = byName
                , acc =
                    { tables = Dict.empty
                    , interfaces = Dict.empty
                    }
                }
            )


firstPartyImportsOf : Dict ModuleId ProjectModule -> ModuleId -> List ModuleId
firstPartyImportsOf byName moduleId =
    case Dict.get moduleId byName of
        Nothing ->
            []

        Just m ->
            m.index.imports
                |> List.filterMap
                    (\import_ ->
                        if Dict.member import_.moduleId byName then
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


inferNodes : List ModuleId -> Project -> Project
inferNodes nodes (Project p) =
    let
        toPrepare : List ProjectModule
        toPrepare =
            SCC.stronglyConnectedComponents nodes (firstPartyImportsOf p.byName)
                |> List.ExtraExtra.fastConcatMap
                    (\component ->
                        component
                            |> List.filterMap
                                (\id ->
                                    case Dict.get id p.byName of
                                        Nothing ->
                                            Nothing

                                        (Just m) as justM ->
                                            if Dict.member m.index.moduleId p.acc.interfaces then
                                                Nothing

                                            else
                                                justM
                                )
                    )

        newAcc : ProjectAcc
        newAcc =
            List.foldl (inferOne p.currentPackage p.depEnv p.moduleMapping) p.acc toPrepare
    in
    Project
        { acc = newAcc
        , currentPackage = p.currentPackage
        , depEnv = p.depEnv
        , moduleMapping = p.moduleMapping
        , byName = p.byName
        }


{-| Infer types in the given module.
-}
inferModule : ModuleName -> Project -> ( Result Error TypeLookupTable, Project )
inferModule moduleName ((Project p) as proj) =
    let
        target : Maybe ProjectModule
        target =
            FullModuleName.fromModuleName moduleName
                |> Maybe.andThen (\full -> ModuleIds.getId full p.moduleMapping)
                |> Maybe.andThen (\id -> Dict.get id p.byName)
    in
    case target of
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
                    inferNodes
                        (Set.toList (importClosure (firstPartyImportsOf p.byName) m.index.moduleId))
                        proj
            in
            ( case Dict.get m.key newP.acc.tables of
                Just result ->
                    result

                Nothing ->
                    Err
                        { moduleName = moduleName
                        , declarationNames = []
                        , details = ModuleNotFound
                        }
            , Project newP
            )


{-| Helper. Run [`inferModule`](#inferModules) for each of the given modules,
collecting successes and errors into separate `Dict`s.
-}
inferModules :
    Dict ModuleName File
    -> Project
    ->
        ( { tables : Dict ModuleName TypeLookupTable
          , errors : Dict ModuleName Error
          }
        , Project
        )
inferModules files proj0 =
    files
        |> Dict.foldl
            (\moduleName _ ( acc, proj ) ->
                case inferModule moduleName proj of
                    ( Ok table, newProj ) ->
                        ( { errors = acc.errors
                          , tables = Dict.insert moduleName table acc.tables
                          }
                        , newProj
                        )

                    ( Err err, newProj ) ->
                        ( { tables = acc.tables
                          , errors = Dict.insert moduleName err acc.errors
                          }
                        , newProj
                        )
            )
            ( { tables = Dict.empty
              , errors = Dict.empty
              }
            , proj0
            )



-- DEPENDENCIES


{-| Input to [`dependencyEnv`](#dependencyEnv).

A dependency package with its type information, parsed from the dependency's
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


{-| Data parsed from dependencies' `docs.json` files.

This cache doesn't change as user's project code changes - only invalidate it
and [`Project`](#Project) when `elm.json` changes.

-}
type DependencyEnv
    = DependencyEnv
        { globalEnv : Dict GlobalKey TypeI.Type
        , typeAliases : Dict GlobalKey TypeAlias
        , index : ModuleLookup.Index
        , moduleMapping : ModuleIds.Mapping
        }


{-| Possible outcomes of running [`dependencyEnv`](#dependencyEnv).

An example `NeedPackageSources`:

    Dict.fromList
        [ ( "example/css", [ "src/Css/Internal.elm" ] ) ]

-}
type DependencyEnvOutcome
    = Ready DependencyEnv
    | NeedPackageSources (Dict PackageName (List String))
    | Failed Error


{-| Build a [`DependencyEnv`](#DependencyEnv).

Initially you can run with `sourcesToResolveAmbiguity = Dict.empty`. If you get
`NeedPackageSources` back, read and parse those Elm files from the dependencies
in your `ELM_HOME` (usually `~/.elm`) and supply them in
`sourcesToResolveAmbiguity` in the next call.

-}
dependencyEnv :
    { directDependencies : List PackageName
    , allDependencies : List Dependency
    , sourcesToResolveAmbiguity : Dict PackageName (List File)
    }
    -> DependencyEnvOutcome
dependencyEnv { directDependencies, allDependencies, sourcesToResolveAmbiguity } =
    let
        deps : Dependencies
        deps =
            Dependencies.fromList allDependencies

        directVisibleDeps : Dependencies
        directVisibleDeps =
            allDependencies
                |> List.filter (\pkg -> List.member pkg.name directDependencies)
                |> Dependencies.fromList

        depModuleNames : List FullModuleName
        depModuleNames =
            (allDependencies
                |> List.ExtraExtra.fastConcatMap (\pkg -> List.map (\m -> FullModuleName.fromDotted m.name) pkg.modules)
            )
                ++ (DependencySources.referencedModules deps
                        |> List.map FullModuleName.fromDotted
                   )

        moduleMapping0 : ModuleIds.Mapping
        moduleMapping0 =
            List.foldl (\name acc -> ModuleIds.intern name acc |> Tuple.second) ModuleIds.empty depModuleNames

        ( depIndex, moduleMapping1 ) =
            ModuleLookup.buildIndex moduleMapping0 directVisibleDeps

        baseEnv : Result Error DependencyEnv
        baseEnv =
            (State.do (Dependencies.register moduleMapping1 deps) <|
                \( depAliases, moduleMapping2 ) ->
                    State.do State.getGlobalEnv <|
                        \globalEnv ->
                            State.pure <|
                                DependencyEnv
                                    { globalEnv = globalEnv
                                    , typeAliases = depAliases
                                    , index = depIndex
                                    , moduleMapping = moduleMapping2
                                    }
            )
                |> State.run State.empty
                |> Tuple.first
    in
    case baseEnv of
        Err err ->
            Failed err

        Ok (DependencyEnv env) ->
            let
                reachable : Set PackageName
                reachable =
                    reachablePackages deps directDependencies

                needed : Dict PackageName (List String)
                needed =
                    DependencySources.neededSources deps sourcesToResolveAmbiguity
                        |> List.foldl
                            (\( pkg, names ) acc ->
                                if Set.member pkg reachable then
                                    Dict.insert pkg names acc

                                else
                                    acc
                            )
                            Dict.empty
            in
            if Dict.isEmpty needed then
                case DependencySources.aliases env.moduleMapping deps sourcesToResolveAmbiguity of
                    Err err ->
                        Failed err

                    Ok ( sourceAliases, moduleMapping2 ) ->
                        Ready
                            (DependencyEnv
                                { globalEnv = env.globalEnv
                                , index = env.index
                                , typeAliases = Dict.union sourceAliases env.typeAliases
                                , moduleMapping = moduleMapping2
                                }
                            )

            else
                NeedPackageSources needed


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



-- PER-MODULE INFERENCE (internal)


{-| What one module contributes to the modules that import it.
-}
type alias ModuleInterface =
    { moduleIndex : ModuleIndex
    , values : Dict VarName TypeI.Type
    , typeAliases : Dict GlobalKey TypeAlias
    }


type alias ProjectModule =
    { key : ModuleName
    , index : ModuleIndex
    , file : File
    }


type alias ProjectAcc =
    { tables : Dict ModuleName (Result Error TypeLookupTable)
    , interfaces : Dict ModuleId ModuleInterface
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
    case inferModule_ currentPackage depEnv moduleMapping imported m.index m.file of
        Ok { table, interface } ->
            { tables = Dict.insert m.key (Ok table) acc.tables
            , interfaces = Dict.insert m.index.moduleId interface acc.interfaces
            }

        Err err ->
            { tables = Dict.insert m.key (Err err) acc.tables
            , interfaces =
                Dict.insert m.index.moduleId
                    { moduleIndex = m.index
                    , values = Dict.empty
                    , typeAliases = Dict.empty
                    }
                    acc.interfaces
            }



-- THE CORE


{-| Everything a single module's inference needs, derived once from the
`DependencyEnv` and the imported interfaces.
-}
type alias ModuleCtx =
    { thisIndex : ModuleIndex
    , modules : Dict ModuleId ModuleIndex
    , resolver : TypeResolver
    , index : ModuleLookup.Index
    , moduleMapping : ModuleIds.Mapping
    , -- what this module passes on to its own importers
      inheritedAliases : Dict GlobalKey TypeAlias
    , depTypeAliases : Dict GlobalKey TypeAlias
    , globalEnv : Dict GlobalKey TypeI.Type
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


moduleCtx : Maybe PackageName -> DependencyEnv -> ModuleIds.Mapping -> Dict ModuleId ModuleInterface -> ModuleIndex -> ModuleCtx
moduleCtx currentPackage (DependencyEnv depEnv) moduleMapping importedInterfaces thisIndex =
    let
        modules : Dict ModuleId ModuleIndex
        modules =
            importedInterfaces
                |> Dict.map (\_ interface -> interface.moduleIndex)
                |> Dict.insert thisIndex.moduleId thisIndex

        imported :
            { inheritedAliases : Dict GlobalKey TypeAlias
            , globalEnv : Dict GlobalKey TypeI.Type
            }
        imported =
            Dict.foldl
                (\moduleId interface acc ->
                    { inheritedAliases = Dict.union interface.typeAliases acc.inheritedAliases
                    , globalEnv =
                        Dict.foldl
                            (\name scheme inner -> Dict.insert ( moduleId, "", name ) scheme inner)
                            acc.globalEnv
                            interface.values
                    }
                )
                { inheritedAliases = Dict.empty
                , globalEnv = depEnv.globalEnv
                }
                importedInterfaces
    in
    { thisIndex = thisIndex
    , modules = modules
    , resolver = ModuleLookup.typeResolverFor moduleMapping depEnv.index modules thisIndex
    , index = depEnv.index
    , moduleMapping = moduleMapping
    , inheritedAliases = imported.inheritedAliases
    , depTypeAliases = depEnv.typeAliases
    , globalEnv = imported.globalEnv
    , allowKernel = allowsKernel currentPackage
    }


inferModule_ :
    Maybe PackageName
    -> DependencyEnv
    -> ModuleIds.Mapping
    -> Dict ModuleId ModuleInterface
    -> ModuleIndex
    -> File
    -> Result Error { table : TypeLookupTable, interface : ModuleInterface }
inferModule_ currentPackage depEnv moduleMapping importedInterfaces thisIndex file =
    let
        ctx : ModuleCtx
        ctx =
            moduleCtx currentPackage depEnv moduleMapping importedInterfaces thisIndex
    in
    (State.do (gatherTypeAliases ctx file) <|
        \ownAliases ->
            let
                outgoingAliases : Dict GlobalKey TypeAlias
                outgoingAliases =
                    Dict.union ownAliases ctx.inheritedAliases

                typeAliases : Dict GlobalKey TypeAlias
                typeAliases =
                    Dict.union outgoingAliases ctx.depTypeAliases
            in
            State.do (registerConstructorsAndPorts ctx file) <|
                \() ->
                    State.do (registerEffectMagic ctx) <|
                        \() ->
                            State.do (solveModule ctx typeAliases file) <|
                                \() ->
                                    State.do (moduleResult ctx file outgoingAliases) <|
                                        \result ->
                                            State.pure result
    )
        |> State.run (State.init ctx.globalEnv)
        |> Tuple.first


moduleResult :
    ModuleCtx
    -> File
    -> Dict GlobalKey TypeAlias
    ->
        StateM
            { table : TypeLookupTable
            , interface : ModuleInterface
            }
moduleResult ctx file outgoingAliases =
    State.do State.getNodeIds <|
        \nodeIds ->
            State.do State.getSubst <|
                \substitutionMap ->
                    State.do State.getGlobalEnv <|
                        \globalEnv ->
                            let
                                exposedValues : Dict VarName TypeI.Type
                                exposedValues =
                                    ctx.thisIndex.exposedValues
                                        |> Set.foldl
                                            (\name acc ->
                                                case Dict.get ( ctx.thisIndex.moduleId, "", name ) globalEnv of
                                                    Just scheme ->
                                                        Dict.insert name scheme acc

                                                    Nothing ->
                                                        acc
                                            )
                                            Dict.empty

                                annotationFor : Dict TypeI.Id TypeI.MonoType
                                annotationFor =
                                    file.declarations
                                        |> List.foldl
                                            (\declNode acc ->
                                                case Node.value declNode of
                                                    Declaration.FunctionDeclaration fn ->
                                                        case fn.signature of
                                                            Nothing ->
                                                                acc

                                                            Just sigNode ->
                                                                case Dict.get (RangeLike.fromRange (Node.range declNode)) nodeIds of
                                                                    Nothing ->
                                                                        acc

                                                                    Just declId ->
                                                                        case TypeI.fromTypeAnnotation ctx.resolver (Node.value (Node.value sigNode).typeAnnotation) of
                                                                            Err _ ->
                                                                                acc

                                                                            Ok annoMono ->
                                                                                Dict.insert declId annoMono acc

                                                    _ ->
                                                        acc
                                            )
                                            Dict.empty
                            in
                            State.pure
                                { table =
                                    TypeLookupTable.Internal.TLT
                                        { nodeIds = nodeIds
                                        , subst = SubstitutionMap.forLookup substitutionMap
                                        , moduleMapping = ctx.moduleMapping
                                        , cache = Array.empty
                                        , pool = Dict.empty
                                        , annotationFor = annotationFor
                                        }
                                , interface =
                                    { moduleIndex = ctx.thisIndex
                                    , values = exposedValues
                                    , typeAliases = outgoingAliases
                                    }
                                }



-- SOLVING ONE MODULE'S TOP-LEVEL DECLARATIONS


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
                    (\declNode byName ->
                        case Node.value declNode of
                            Declaration.FunctionDeclaration fn ->
                                Dict.insert (Elm.Syntax.Expression.Extra.functionName fn)
                                    ( declNode, fn )
                                    byName

                            _ ->
                                byName
                    )
                    Dict.empty

        edges : VarName -> List VarName
        edges key =
            case Dict.get key topLevelFunctions of
                Nothing ->
                    []

                Just ( _, fn ) ->
                    Elm.Syntax.Expression.Extra.referencedNames (Node.value (Node.value fn.declaration).expression)
                        -- Resolve operator aliases to the underlying functions
                        |> List.filterMap
                            (\( maybeModuleName, varName ) ->
                                case ModuleLookup.moduleOfVar ctx.moduleMapping ctx.index ctx.modules ctx.thisIndex (Maybe.andThen FullModuleName.fromModuleName maybeModuleName) varName of
                                    Ok (Just ( "", moduleId )) ->
                                        let
                                            ( resolvedModule, resolvedName ) =
                                                case
                                                    ModuleLookup.resolveOperatorFunction ctx.moduleMapping ctx.modules moduleId varName
                                                        |> Result.withDefault Nothing
                                                of
                                                    Just resolved ->
                                                        resolved

                                                    Nothing ->
                                                        ( moduleId, varName )
                                        in
                                        -- Only this module's own declarations
                                        -- are being ordered here; everything
                                        -- else is already in `globalEnv`.
                                        if resolvedModule == ctx.thisIndex.moduleId && Dict.member resolvedName topLevelFunctions then
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
        |> State.traverse
            (\declarationNode ->
                case Node.value declarationNode of
                    Declaration.AliasDeclaration typeAlias ->
                        let
                            toError : ErrorDetails -> Error
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
                                                (\fieldNode ->
                                                    case
                                                        Tuple.second (Node.value fieldNode)
                                                            |> Node.value
                                                            |> TypeI.fromTypeAnnotation resolver
                                                    of
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
                                        State.pure ()
                        in
                        State.do type_ <|
                            \type__ ->
                                State.do (registerConstructor type__) <|
                                    \() ->
                                        State.pure <|
                                            Just
                                                ( ( moduleId, "", Node.value typeAlias.name )
                                                , { args = List.map (\(Node.Node _ generic) -> TypeVar.parse generic) typeAlias.generics
                                                  , type_ = type__
                                                  }
                                                )

                    _ ->
                        State.pure Nothing
            )
        |> State.map
            (\maybeTypeAliases ->
                maybeTypeAliases
                    |> List.foldl
                        (\maybeTypeAlias acc ->
                            case maybeTypeAlias of
                                Nothing ->
                                    acc

                                Just ( typeAliasKey, typeAlias ) ->
                                    Dict.insert typeAliasKey typeAlias acc
                        )
                        Dict.empty
            )


registerConstructorsAndPorts : ModuleCtx -> File -> StateM ()
registerConstructorsAndPorts ctx file =
    file.declarations
        |> State.traverseUnit
            (\declNode ->
                case Node.value declNode of
                    Declaration.CustomTypeDeclaration customType ->
                        registerCustomType ctx.resolver ctx.thisIndex.moduleId ctx.thisIndex.moduleName customType

                    Declaration.PortDeclaration sig ->
                        registerPort ctx.resolver ctx.thisIndex.moduleId ctx.thisIndex.moduleName sig

                    _ ->
                        State.pure ()
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

        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName moduleName
            , declarationNames = [ typeName ]
            , details = details
            }

        resultType : MonoType
        resultType =
            UserDefinedType
                { package = ""
                , moduleId = moduleId
                , name = typeName
                , args =
                    customType.generics
                        |> List.map
                            (\g ->
                                TypeVar
                                    (TypeVar.parse (Node.value g))
                            )
                }
    in
    customType.constructors
        |> State.traverseUnit
            (\ctorNode ->
                let
                    ctor : SyntaxType.ValueConstructor
                    ctor =
                        Node.value ctorNode

                    argTypes : Result FromTypeAnnotationError (List MonoType)
                    argTypes =
                        ctor.arguments
                            |> Result.Extra.combineMap
                                (\(Node.Node _ arg) -> TypeI.fromTypeAnnotation resolver arg)
                in
                case argTypes of
                    Err fromTypeAnnotationError ->
                        State.error (toError (TypeI.fromTypeAnnotationError fromTypeAnnotationError))

                    Ok args ->
                        let
                            ctorName : String
                            ctorName =
                                Node.value ctor.name

                            ctorType : MonoType
                            ctorType =
                                List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType args
                        in
                        State.addGlobalBinding ( moduleId, "", ctorName ) (TypeI.closeOver ctorType)
            )


registerPort : TypeResolver -> ModuleId -> FullModuleName -> Signature -> StateM ()
registerPort resolver moduleId moduleName sig =
    let
        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName moduleName
            , declarationNames = [ Node.value sig.name ]
            , details = details
            }
    in
    sig.typeAnnotation
        |> Node.value
        |> TypeI.fromTypeAnnotation resolver
        |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
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
        State.pure ()


registerEffectCommand : ModuleCtx -> StateM ()
registerEffectCommand ctx =
    case ctx.thisIndex.effectCommand of
        Nothing ->
            State.pure ()

        Just myCmdName ->
            case ctx.resolver [] "Cmd" of
                Err _ ->
                    State.pure ()

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
            State.pure ()

        Just mySubName ->
            case ctx.resolver [] "Sub" of
                Err _ ->
                    State.pure ()

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
