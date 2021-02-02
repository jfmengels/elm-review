module Review.Api.Module exposing
    ( ModuleApi
    , aliases
    , binops
    , fromDependencies
    , unions
    , values
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Api.Alias as Alias exposing (Alias)
import Review.Api.Binop exposing (Binop)
import Review.Api.Union as Union exposing (Union)
import Review.Internal.Alias
import Review.Internal.Binop
import Review.Internal.Module
import Review.Internal.Union
import Review.Internal.Value as Value exposing (Value)
import Review.Project.Dependency


type alias ModuleApi =
    Review.Internal.Module.Module


fromElmDocs : Elm.Docs.Module -> ModuleApi
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
    Review.Internal.Module.Module
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


fromDependencies : Dict String Review.Project.Dependency.Dependency -> Dict ModuleName ModuleApi
fromDependencies dependencies =
    dependencies
        |> Dict.values
        |> List.concatMap Review.Project.Dependency.modules
        |> List.map (\dependencyModule -> ( String.split "." dependencyModule.name, fromElmDocs dependencyModule ))
        |> Dict.fromList



-- MODULE DATA ACCESS


unions : ModuleApi -> Dict String Union
unions (Review.Internal.Module.Module m) =
    m.unions


aliases : ModuleApi -> Dict String Alias
aliases (Review.Internal.Module.Module m) =
    m.aliases


values : ModuleApi -> Dict String Value
values (Review.Internal.Module.Module m) =
    m.values


binops : ModuleApi -> List Binop
binops (Review.Internal.Module.Module m) =
    m.binops
