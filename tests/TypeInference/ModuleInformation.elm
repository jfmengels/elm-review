module TypeInference.ModuleInformation exposing
    ( ModuleInformation
    , ModuleInformationDict
    , binops
    , empty
    , forModule
    , fromDependencies
    , fromVisitedModule
    , merge
    , values
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Project.Dependency
import TypeInference.Binop as Binop exposing (Binop)
import TypeInference.Value as Value exposing (Value)


type ModuleInformationDict
    = ModuleInformationDict (Dict ModuleName ModuleInformation)


fromDependencies : Dict String Review.Project.Dependency.Dependency -> ModuleInformationDict
fromDependencies dependencies =
    dependencies
        |> Dict.values
        |> List.concatMap Review.Project.Dependency.modules
        |> List.map
            (\module_ ->
                let
                    moduleName : List String
                    moduleName =
                        String.split "." module_.name
                in
                ( moduleName
                , ModuleInformation
                    { values =
                        List.concat
                            [ List.map Value.fromMetadataValue module_.values
                            , List.concatMap (Value.fromMetadataUnion moduleName) module_.unions
                            , List.filterMap (Value.fromMetadataAlias moduleName) module_.aliases
                            ]
                            |> List.map (\element -> ( Value.name element, element ))
                            |> Dict.fromList
                    , binops = dictByName Binop.fromMetadata module_.binops
                    }
                )
            )
        |> Dict.fromList
        |> ModuleInformationDict


dictByName : ({ a | name : String } -> b) -> List { a | name : String } -> Dict String b
dictByName function list =
    list
        |> List.map (\element -> ( element.name, function element ))
        |> Dict.fromList


empty : ModuleInformationDict
empty =
    ModuleInformationDict Dict.empty


fromVisitedModule : ModuleName -> { values : List Value } -> ModuleInformationDict
fromVisitedModule moduleName moduleData =
    ModuleInformationDict
        (Dict.singleton moduleName
            (ModuleInformation
                { values =
                    moduleData.values
                        |> List.map (\value -> ( Value.name value, value ))
                        |> Dict.fromList
                , binops = Dict.empty
                }
            )
        )


merge : ModuleInformationDict -> ModuleInformationDict -> ModuleInformationDict
merge (ModuleInformationDict a) (ModuleInformationDict b) =
    ModuleInformationDict (Dict.union a b)


forModule : ModuleName -> ModuleInformationDict -> Maybe ModuleInformation
forModule moduleName (ModuleInformationDict moduleInformationDict) =
    Dict.get moduleName moduleInformationDict


type ModuleInformation
    = ModuleInformation
        { values : Dict String Value
        , binops : Dict String Binop
        }


values : ModuleInformation -> Dict String Value
values (ModuleInformation moduleInformation) =
    moduleInformation.values


binops : ModuleInformation -> Dict String Binop
binops (ModuleInformation moduleInformation) =
    moduleInformation.binops
