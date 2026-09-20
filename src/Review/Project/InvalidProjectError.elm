module Review.Project.InvalidProjectError exposing (InvalidProjectError(..))

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference.Type exposing (PackageName)


type InvalidProjectError
    = SomeModulesFailedToParse (List String)
    | DuplicateModuleNames { moduleName : ModuleName, paths : List String }
    | ImportCycleError (List String)
    | NoModulesError
    | NeedPackageSources (Dict PackageName (List String))
