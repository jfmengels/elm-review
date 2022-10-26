module Review.Project.ProjectCache exposing (ModuleCacheKey, ProjectCache, empty)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


type alias ProjectCache =
    { dependenciesModules : Maybe { elmJsonRaw : Maybe String, deps : Dict ModuleName Elm.Docs.Module }
    , modules : Dict ModuleName Elm.Docs.Module
    , lookupTables : Dict ModuleName { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable }
    }


type alias ModuleCacheKey =
    { imported : Dict ModuleName Elm.Docs.Module
    , source : String
    }


empty : ProjectCache
empty =
    { dependenciesModules = Nothing
    , modules = Dict.empty
    , lookupTables = Dict.empty
    }
