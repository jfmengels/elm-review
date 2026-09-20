module Review.Types.Compute exposing (compute, computeDeps, computeModule)

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference as TypeInference exposing (DependencyEnv, DependencyEnvOutcome, ModuleInterface)
import Elm.TypeInference.Error exposing (Error)
import Elm.TypeInference.ModuleIndex as ModuleIndex
import Elm.TypeInference.Type exposing (PackageName)
import Review.Project.Dependency as Dependency
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import TypeLookupTable exposing (TypeLookupTable)


compute :
    Dict PackageName Dependency.Dependency
    -> List PackageName
    -> Dict ModuleName File
    -> { tables : Dict ModuleName TypeLookupTable, errors : Dict ModuleName Error }
compute dependencies directDependencies modules =
    case computeDeps dependencies directDependencies of
        TypeInference.Ready dependencyEnv ->
            TypeInference.inferProject dependencyEnv modules

        TypeInference.NeedSources record ->
            Debug.todo ("NeedSources " ++ Debug.toString record)

        TypeInference.Failed error ->
            Debug.todo ("Failed " ++ Debug.toString error)


type Acc
    = Acc (Dict FullModuleName ModuleInterface)


computeModule :
    DependencyEnv
    -> Dict ModuleName ModuleInterface
    -> OpaqueProjectModule
    -> Result Error ( TypeLookupTable, Dict ModuleName ModuleInterface )
computeModule dependencyEnv interfaces mod =
    let
        file : Elm.Syntax.File.File
        file =
            ProjectModule.ast mod

        index : ModuleIndex.ModuleIndex
        index =
            ModuleIndex.fromFile file

        -- Copied from TypeInference.inferOne
        imported : Dict FullModuleName TypeInference.ModuleInterface
        imported =
            index.imports
                |> List.foldl
                    (\import_ inner ->
                        case Dict.get (FullModuleName.toModuleName import_.moduleName) interfaces of
                            Just interface ->
                                Dict.insert import_.moduleName interface inner

                            Nothing ->
                                inner
                    )
                    Dict.empty
    in
    TypeInference.inferModule_ dependencyEnv imported file
        |> Result.map
            (\{ table, interface } ->
                ( table, Dict.insert (FullModuleName.toModuleName index.moduleName) interface interfaces )
            )


computeDeps : Dict PackageName Dependency.Dependency -> List PackageName -> DependencyEnvOutcome
computeDeps dependencies directDependencies =
    let
        allDeps : List TypeInference.Dependency
        allDeps =
            Dict.foldr
                (\name dep list ->
                    { name = name
                    , dependencies = listDependencies (Dependency.elmJson dep)
                    , modules = Dependency.modules dep
                    }
                        :: list
                )
                []
                dependencies
    in
    TypeInference.dependencyEnv
        { directDependencies = directDependencies
        , allDependencies = allDeps
        , sourcesToResolveAmbiguity = Dict.empty
        }


listDependencies : Elm.Project.Project -> List PackageName
listDependencies elmJson =
    case elmJson of
        Elm.Project.Application _ ->
            []

        Elm.Project.Package { deps } ->
            List.map (\( name, _ ) -> Elm.Package.toString name) deps
