module Review.ModuleInformation exposing
    ( ModuleInformation
    , aliases
    , aliasesAsDict
    , binops
    , create
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
import Review.Internal.ModuleInformation
import Review.Internal.Union
import Review.Internal.Value as Value exposing (Value)
import Review.Project.Dependency
import Review.Type.Alias as Alias exposing (Alias)
import Review.Type.Binop exposing (Binop)
import Review.Type.Union as Union exposing (Union)


type alias ModuleInformation =
    Review.Internal.ModuleInformation.ModuleInformation


fromElmDocs : Elm.Docs.Module -> ModuleInformation
fromElmDocs elmDocsModule =
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
    Review.Internal.ModuleInformation.ModuleInformation
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
    Review.Internal.ModuleInformation.ModuleInformation
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
        |> List.map (\dependencyModule -> ( String.split "." dependencyModule.name, fromElmDocs dependencyModule ))
        |> Dict.fromList



-- MODULE DATA ACCESS


unions : ModuleInformation -> List Union
unions (Review.Internal.ModuleInformation.ModuleInformation m) =
    Dict.values m.unions


unionsAsDict : ModuleInformation -> Dict String Union
unionsAsDict (Review.Internal.ModuleInformation.ModuleInformation m) =
    m.unions


aliases : ModuleInformation -> List Alias
aliases (Review.Internal.ModuleInformation.ModuleInformation m) =
    Dict.values m.aliases


aliasesAsDict : ModuleInformation -> Dict String Alias
aliasesAsDict (Review.Internal.ModuleInformation.ModuleInformation m) =
    m.aliases


getAliasByName : String -> ModuleInformation -> Maybe Alias
getAliasByName name (Review.Internal.ModuleInformation.ModuleInformation m) =
    Dict.get name m.aliases


values : ModuleInformation -> List Value
values (Review.Internal.ModuleInformation.ModuleInformation m) =
    Dict.values m.values


valuesAsDict : ModuleInformation -> Dict String Value
valuesAsDict (Review.Internal.ModuleInformation.ModuleInformation m) =
    m.values


getValueByName : String -> ModuleInformation -> Maybe Value
getValueByName name (Review.Internal.ModuleInformation.ModuleInformation m) =
    Dict.get name m.values


binops : ModuleInformation -> List Binop
binops (Review.Internal.ModuleInformation.ModuleInformation m) =
    m.binops


getUnionByName : String -> ModuleInformation -> Maybe Union
getUnionByName name (Review.Internal.ModuleInformation.ModuleInformation m) =
    Dict.get name m.unions
