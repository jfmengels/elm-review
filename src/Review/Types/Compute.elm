module Review.Types.Compute exposing (computeDeps)

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.TypeInference as TypeInference exposing (DependencyEnv, DependencyEnvOutcome, ModuleInterface)
import Elm.TypeInference.Type exposing (PackageName)
import Review.Project.Dependency as Dependency


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
