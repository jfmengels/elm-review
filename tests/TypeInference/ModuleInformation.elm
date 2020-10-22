module TypeInference.ModuleInformation exposing
    ( ModuleInformation
    , ModuleInformationDict
    , binops
    , empty
    , forModule
    , fromDependencies
    , merge
    , singleton
    , values
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Project.Dependency
import TypeInference.Binop as Binop exposing (Binop)
import TypeInference.Type as Type
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
                ( String.split "." module_.name
                , ModuleInformation
                    { values = dictByName Value.fromMetadata module_.values
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


singleton : ModuleName -> { values : List Value } -> ModuleInformationDict
singleton moduleName moduleData =
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
