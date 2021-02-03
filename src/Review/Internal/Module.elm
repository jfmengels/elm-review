module Review.Internal.Module exposing
    ( Module(..)
    , create
    , fromDependencies
    , fromElmDocsModule
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Api.Alias as Alias exposing (Alias)
import Review.Api.Binop exposing (Binop)
import Review.Api.Port as Port exposing (Port)
import Review.Api.Union as Union exposing (Union)
import Review.Internal.Alias
import Review.Internal.Binop
import Review.Internal.Union
import Review.Internal.Value as Value exposing (Value)
import Review.Project.Dependency


type Module
    = Module
        { name : ModuleName
        , comment : String
        , unions : Dict String Union
        , aliases : Dict String Alias
        , values : Dict String Value
        , ports : Dict String Port
        , binops : List Binop
        }


create :
    { name : ModuleName
    , comment : String
    , unions : List Union
    , aliases : List Alias
    , values : List Value
    , ports : List Port
    , binops : List Binop
    }
    -> Module
create params =
    let
        unions_ : List Union
        unions_ =
            List.map (Review.Internal.Union.relateToModule params.name) params.unions

        aliases_ : List Alias
        aliases_ =
            List.map (Review.Internal.Alias.relateToModule params.name) params.aliases
    in
    Module
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
        , ports =
            params.ports
                |> List.map (\element -> ( Port.name element, element ))
                |> Dict.fromList
        , binops = params.binops
        }


fromDependencies : Dict String Review.Project.Dependency.Dependency -> Dict ModuleName Module
fromDependencies dependencies =
    dependencies
        |> Dict.values
        |> List.concatMap Review.Project.Dependency.modules
        |> List.map (\dependencyModule -> ( String.split "." dependencyModule.name, fromElmDocs dependencyModule ))
        |> Dict.fromList


fromElmDocs : Elm.Docs.Module -> Module
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
    Module
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
        , ports = Dict.empty
        , binops = List.map Review.Internal.Binop.fromElmDocs elmDocsModule.binops
        }


fromElmDocsModule : Elm.Docs.Module -> Module
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
    Module
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
        , ports = Dict.empty
        , binops = List.map Review.Internal.Binop.fromElmDocs elmDocsModule.binops
        }
