module Review.Project.ProjectCache exposing (ImportedElementType(..), ModuleCacheKey, ProjectCache, empty)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Cache.ContentHash exposing (ContentHash)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


type alias ProjectCache =
    { dependenciesModules : Maybe { elmJsonContentHash : Maybe ContentHash, deps : Dict ModuleName Elm.Docs.Module }
    , modules : Dict ModuleName Elm.Docs.Module
    , lookupTables : Dict ModuleName { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable }
    }


type alias ModuleCacheKey =
    { implicitImports : Dict String (List ImportedElementType)
    , contentHash : ContentHash
    }


type ImportedElementType
    = Value String
    | Type String


empty : ProjectCache
empty =
    { dependenciesModules = Nothing
    , modules = Dict.empty
    , lookupTables = Dict.empty
    }
