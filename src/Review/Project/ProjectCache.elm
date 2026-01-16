module Review.Project.ProjectCache exposing (ImportedElement, ModuleCacheKey, ProjectCache(..), empty, typeElement, valueElement)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Cache.ContentHash exposing (ContentHash)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


type ProjectCache context
    = ProjectCache
        { elmJsonContentHash : Maybe ContentHash
        , dependencies : Maybe { deps : Dict ModuleName Elm.Docs.Module, baseModuleContext : context }
        , modules : Dict ModuleName Elm.Docs.Module
        , lookupTables : Dict ModuleName { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable }
        }


type alias ModuleCacheKey =
    { implicitImports : Dict String (List ImportedElement)
    , contentHash : ContentHash
    }


empty : ProjectCache context
empty =
    ProjectCache
        { elmJsonContentHash = Nothing
        , dependencies = Nothing
        , modules = Dict.empty
        , lookupTables = Dict.empty
        }


type alias ImportedElement =
    ( String, ImportedElementType )


type ImportedElementType
    = Value
    | Type


valueElement : String -> ImportedElement
valueElement name =
    ( name, Value )


typeElement : String -> ImportedElement
typeElement name =
    ( name, Type )
