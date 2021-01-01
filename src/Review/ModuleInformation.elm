module Review.ModuleInformation exposing
    ( ModuleInformation
    , aliases
    , binops
    , empty
    , fromDependencies
    , fromElmDocsModule
    , getValueByName
    , toElmDocsModule
    , toElmDocsModuleDict
    , unions
    , values
    , valuesAsDict
    )

-- TODO Expose module, but hide implementation and type inside an "Internal" module

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Project.Dependency
import Review.TypeInference.Value as Value exposing (Value)


type ModuleInformation
    = ModuleInformation
        { name : ModuleName
        , comment : String
        , unions : List Elm.Docs.Union
        , aliases : List Elm.Docs.Alias
        , values : Dict String Value
        , binops : List Elm.Docs.Binop
        }


fromElmDocsModule : Elm.Docs.Module -> ModuleInformation
fromElmDocsModule elmDocsModule =
    let
        moduleName : List String
        moduleName =
            String.split "." elmDocsModule.name
    in
    ModuleInformation
        { name = moduleName
        , comment = elmDocsModule.comment
        , unions = elmDocsModule.unions
        , aliases = elmDocsModule.aliases
        , values =
            List.concat
                [ List.map Value.fromMetadataValue elmDocsModule.values
                , List.concatMap (Value.fromMetadataUnion moduleName) elmDocsModule.unions
                , List.filterMap (Value.fromMetadataAlias moduleName) elmDocsModule.aliases
                ]
                |> List.map (\element -> ( Value.name element, element ))
                |> Dict.fromList
        , binops = elmDocsModule.binops
        }


fromDependencies : Dict String Review.Project.Dependency.Dependency -> Dict ModuleName ModuleInformation
fromDependencies dependencies =
    dependencies
        |> Dict.values
        |> List.concatMap Review.Project.Dependency.modules
        |> List.map (\dependencyModule -> ( String.split "." dependencyModule.name, fromElmDocsModule dependencyModule ))
        |> Dict.fromList


toElmDocsModule : ModuleInformation -> Elm.Docs.Module
toElmDocsModule (ModuleInformation moduleInfo) =
    { name = String.join "." moduleInfo.name
    , comment = moduleInfo.comment
    , unions = moduleInfo.unions
    , aliases = moduleInfo.aliases
    , values =
        moduleInfo.values
            |> Dict.values
            |> List.filterMap Value.toMetadataValue
    , binops = moduleInfo.binops
    }


toElmDocsModuleDict : Dict ModuleName ModuleInformation -> Dict ModuleName Elm.Docs.Module
toElmDocsModuleDict dict =
    Dict.map (always toElmDocsModule) dict


empty : ModuleName -> ModuleInformation
empty moduleName =
    ModuleInformation
        { name = moduleName
        , comment = ""
        , unions = []
        , values = Dict.empty
        , aliases = []
        , binops = []
        }



-- MODULE DATA ACCESS


unions : ModuleInformation -> List Elm.Docs.Union
unions (ModuleInformation m) =
    m.unions


aliases : ModuleInformation -> List Elm.Docs.Alias
aliases (ModuleInformation m) =
    m.aliases


values : ModuleInformation -> List Value
values (ModuleInformation m) =
    Dict.values m.values


valuesAsDict : ModuleInformation -> Dict String Value
valuesAsDict (ModuleInformation m) =
    m.values


getValueByName : String -> ModuleInformation -> Maybe Value
getValueByName name (ModuleInformation m) =
    Dict.get name m.values


binops : ModuleInformation -> List Elm.Docs.Binop
binops (ModuleInformation m) =
    m.binops
