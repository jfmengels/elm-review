module Review.ModuleNameLookupTable.Compute exposing (compute)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal exposing (ModuleNameLookupTable)
import Review.Project.Internal exposing (Project)


compute : ModuleName -> Project -> ( ModuleNameLookupTable, Project )
compute moduleName project =
    ( ModuleNameLookupTableInternal.empty moduleName, project )
