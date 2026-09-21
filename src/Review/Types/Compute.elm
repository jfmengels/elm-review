module Review.Types.Compute exposing (computeDeps)

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference as TypeInference
import Elm.TypeInference.ProjectError exposing (ProjectError)
import Elm.TypeInference.Type exposing (PackageName)
import Review.Project.Dependency as Dependency


computeDeps :
    Dict PackageName Dependency.Dependency
    -> List PackageName
    -> Dict PackageName (List File)
    -> Maybe PackageName
    -> Dict ModuleName File
    -> Result (Dict String (List String)) TypeInference.Project
computeDeps dependencies directDependencies sourcesToResolveAmbiguity projectPackageName projectFiles =
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

        projectResult : Result ProjectError TypeInference.Project
        projectResult =
            TypeInference.init
                { directDependencies = directDependencies
                , allDependencies = allDeps
                , sourcesToResolveAmbiguity = sourcesToResolveAmbiguity
                , projectPackageName = projectPackageName
                , projectFiles = projectFiles
                }
    in
    case projectResult of
        Ok project ->
            Ok project

        Err (Elm.TypeInference.ProjectError.NeedPackageSources dict) ->
            Err dict

        Err error ->
            Debug.todo (Debug.toString error)


listDependencies : Elm.Project.Project -> List PackageName
listDependencies elmJson =
    case elmJson of
        Elm.Project.Application _ ->
            []

        Elm.Project.Package { deps } ->
            List.map (\( name, _ ) -> Elm.Package.toString name) deps
