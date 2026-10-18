module Review.Project.ProjectCache exposing
    ( ImportedElement
    , ModuleCacheKey
    , ProjectCache
    , mapTypes
    , setTypes
    , typeElement
    , types
    , valueElement
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference as TypeInference
import Review.Cache.ContentHash exposing (ContentHash)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.DocsModule exposing (DocsModule)


type alias ProjectCache =
    { elmJsonContentHash : Maybe ContentHash
    , dependencies : Dict ModuleName Elm.Docs.Module
    , types : Maybe TypeInference.Project
    , modules : Dict ModuleName DocsModule
    , lookupTables : Dict ModuleName { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable }
    }


type alias ModuleCacheKey =
    { implicitImports : Dict String (List ImportedElement)
    , contentHash : ContentHash
    }


type alias ImportedElement =
    ( String, ImportedElementType )


type ImportedElementType
    = Value
    | Type


types : ProjectCache -> TypeInference.Project
types projectCache =
    projectCache.types
        |> Maybe.withDefault TypeInference.empty


setTypes : TypeInference.Project -> ProjectCache -> ProjectCache
setTypes typeInferenceProject_ projectCache =
    { projectCache | types = Just typeInferenceProject_ }


mapTypes : (TypeInference.Project -> TypeInference.Project) -> ProjectCache -> ProjectCache
mapTypes fn projectCache =
    { projectCache | types = Maybe.map fn projectCache.types }


valueElement : String -> ImportedElement
valueElement name =
    ( name, Value )


typeElement : String -> ImportedElement
typeElement name =
    ( name, Type )
