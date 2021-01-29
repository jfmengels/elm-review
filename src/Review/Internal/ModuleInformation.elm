module Review.Internal.ModuleInformation exposing
    ( ModuleInformation(..)
    , fromElmDocsModule
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Internal.Alias
import Review.Internal.Binop
import Review.Internal.Union
import Review.Internal.Value as Value exposing (Value)
import Review.Type.Alias as Alias exposing (Alias)
import Review.Type.Binop exposing (Binop)
import Review.Type.Union as Union exposing (Union)


type ModuleInformation
    = ModuleInformation
        { name : ModuleName
        , comment : String
        , unions : Dict String Union
        , aliases : Dict String Alias
        , values : Dict String Value
        , binops : List Binop
        }


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
