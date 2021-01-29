module Review.ModuleInformation exposing
    ( ModuleInformation
    , aliases
    , aliasesAsDict
    , binops
    , create
    , empty
    , fromDependencies
    , getAliasByName
    , getUnionByName
    , getValueByName
    , unions
    , unionsAsDict
    , values
    , valuesAsDict
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Internal.Alias
import Review.Internal.Binop
import Review.Internal.ModuleInformation exposing (ModuleInformation(..))
import Review.Internal.Union
import Review.Internal.Value as Value exposing (Value)
import Review.Project.Dependency
import Review.Type.Alias as Alias exposing (Alias)
import Review.Type.Binop exposing (Binop)
import Review.Type.Union as Union exposing (Union)


type alias ModuleInformation =
    Review.Internal.ModuleInformation.ModuleInformation


fromElmDocsModule : Elm.Docs.Module -> ModuleInformation
fromElmDocsModule elmDocsModule =
    let
        moduleName : List String
        moduleName =
            String.split "." elmDocsModule.name

        unions_ : List Union
        unions_ =
            List.map Review.Internal.Union.fromElmDocs elmDocsModule.unions

        aliases_ : List Alias
        aliases_ =
            List.map Review.Internal.Alias.fromElmDocs elmDocsModule.aliases
    in
    ModuleInformation
        { name = moduleName
        , comment = elmDocsModule.comment
        , unions =
            unions_
                |> List.map (\union -> ( Union.name union, union ))
                |> Dict.fromList
        , aliases =
            aliases_
                |> List.map (\alias -> ( Alias.name alias, alias ))
                |> Dict.fromList
        , values =
            List.concat
                [ List.map Value.fromElmDocs elmDocsModule.values
                , List.concatMap (Value.fromUnion moduleName) unions_
                , List.filterMap (Value.fromAlias moduleName) aliases_
                ]
                |> List.map (\element -> ( Value.name element, element ))
                |> Dict.fromList
        , binops = List.map Review.Internal.Binop.fromElmDocs elmDocsModule.binops
        }


create :
    { name : ModuleName
    , comment : String
    , unions : List Union
    , aliases : List Alias
    , values : List Value
    , binops : List Binop
    }
    -> ModuleInformation
create params =
    let
        unions_ : List Union
        unions_ =
            List.map (Review.Internal.Union.relateToModule params.name) params.unions

        aliases_ : List Alias
        aliases_ =
            List.map (Review.Internal.Alias.relateToModule params.name) params.aliases
    in
    ModuleInformation
        { name = params.name
        , comment = params.comment
        , unions =
            unions_
                |> List.map (\element -> ( Union.name element, element ))
                |> Dict.fromList
        , aliases =
            params.aliases
                |> List.map (\element -> ( Alias.name element, element ))
                |> Dict.fromList
        , values =
            List.concat
                [ List.map (Value.relateToModule params.name) params.values
                , List.concatMap (Value.fromUnion params.name) unions_
                , List.filterMap (Value.fromAlias params.name) aliases_
                ]
                |> List.map (\element -> ( Value.name element, element ))
                |> Dict.fromList
        , binops = params.binops
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
    , unions =
        Dict.values moduleInfo.unions
            |> List.map Review.Internal.Union.toElmDocs
    , aliases =
        Dict.values moduleInfo.aliases
            |> List.map Review.Internal.Alias.toElmDocs
    , values =
        moduleInfo.values
            |> Dict.values
            |> List.filterMap Value.toElmDocs
    , binops = List.filterMap Review.Internal.Binop.toElmDocs moduleInfo.binops
    }


toElmDocsModuleDict : Dict ModuleName ModuleInformation -> Dict ModuleName Elm.Docs.Module
toElmDocsModuleDict dict =
    Dict.map (always toElmDocsModule) dict


empty : ModuleName -> ModuleInformation
empty moduleName =
    ModuleInformation
        { name = moduleName
        , comment = ""
        , unions = Dict.empty
        , values = Dict.empty
        , aliases = Dict.empty
        , binops = []
        }



-- MODULE DATA ACCESS


unions : ModuleInformation -> List Union
unions (ModuleInformation m) =
    Dict.values m.unions


unionsAsDict : ModuleInformation -> Dict String Union
unionsAsDict (ModuleInformation m) =
    m.unions


aliases : ModuleInformation -> List Alias
aliases (ModuleInformation m) =
    Dict.values m.aliases


aliasesAsDict : ModuleInformation -> Dict String Alias
aliasesAsDict (ModuleInformation m) =
    m.aliases


getAliasByName : String -> ModuleInformation -> Maybe Alias
getAliasByName name (ModuleInformation m) =
    Dict.get name m.aliases


values : ModuleInformation -> List Value
values (ModuleInformation m) =
    Dict.values m.values


valuesAsDict : ModuleInformation -> Dict String Value
valuesAsDict (ModuleInformation m) =
    m.values


getValueByName : String -> ModuleInformation -> Maybe Value
getValueByName name (ModuleInformation m) =
    Dict.get name m.values


binops : ModuleInformation -> List Binop
binops (ModuleInformation m) =
    m.binops


getUnionByName : String -> ModuleInformation -> Maybe Union
getUnionByName name (ModuleInformation m) =
    Dict.get name m.unions
