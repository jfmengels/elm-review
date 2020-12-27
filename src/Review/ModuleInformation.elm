module Review.ModuleInformation exposing
    ( ModuleInformation
    , aliases
    , binops
    , empty
    , fromDependencies
    , fromElmDocsModule
    , fromElmDocsModuleDict
    , toElmDocsModule
    , toElmDocsModuleDict
    , unions
    , values
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Project.Dependency


type ModuleInformation
    = ModuleInformation Elm.Docs.Module


fromElmDocsModule : Elm.Docs.Module -> ModuleInformation
fromElmDocsModule elmDocsModule =
    ModuleInformation elmDocsModule


fromElmDocsModuleDict : Dict ModuleName Elm.Docs.Module -> Dict ModuleName ModuleInformation
fromElmDocsModuleDict dict =
    Dict.map (always fromElmDocsModule) dict


fromDependencies : Dict String Review.Project.Dependency.Dependency -> Dict ModuleName ModuleInformation
fromDependencies dependencies =
    dependencies
        |> Dict.values
        |> List.concatMap Review.Project.Dependency.modules
        |> List.map (\dependencyModule -> ( String.split "." dependencyModule.name, fromElmDocsModule dependencyModule ))
        |> Dict.fromList


toElmDocsModule : ModuleInformation -> Elm.Docs.Module
toElmDocsModule (ModuleInformation elmDocsModule) =
    elmDocsModule


toElmDocsModuleDict : Dict ModuleName ModuleInformation -> Dict ModuleName Elm.Docs.Module
toElmDocsModuleDict dict =
    Dict.map (always toElmDocsModule) dict


empty : List String -> ModuleInformation
empty moduleName =
    ModuleInformation
        { name = String.join "." moduleName
        , comment = ""
        , unions = []
        , values = []
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


values : ModuleInformation -> List Elm.Docs.Value
values (ModuleInformation m) =
    m.values


binops : ModuleInformation -> List Elm.Docs.Binop
binops (ModuleInformation m) =
    m.binops
