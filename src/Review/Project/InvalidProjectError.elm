module Review.Project.InvalidProjectError exposing (InvalidProjectError(..))

import Elm.Syntax.ModuleName exposing (ModuleName)


type InvalidProjectError
    = SomeModulesFailedToParse (List String)
    | DuplicateModuleNames { moduleName : ModuleName, paths : List String }
    | ImportCycleError (List ModuleName)
    | NoModulesError
