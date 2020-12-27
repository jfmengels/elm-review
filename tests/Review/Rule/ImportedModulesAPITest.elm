module Review.Rule.ImportedModulesAPITest exposing (all)

import Dependencies.ElmCore
import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


rule : (Dict ModuleName Elm.Docs.Module -> String) -> Rule
rule whatToPrint =
    Rule.newModuleRuleSchemaUsingContextCreator "ImportedModulesAPITest" initialContext
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor whatToPrint)
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : (Dict ModuleName Elm.Docs.Module -> String) -> Node Module -> Context -> ( List (Error {}), Context )
moduleDefinitionVisitor whatToPrint node context =
    if Elm.Syntax.Module.moduleName (Node.value node) == [ "Target" ] then
        ( [ Rule.error { message = whatToPrint context, details = [ "details" ] }
                { start = { row = 1, column = 1 }
                , end = { row = 1, column = 7 }
                }
          ]
        , context
        )

    else
        ( [], context )


type alias Context =
    Dict ModuleName Elm.Docs.Module


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\importedModules () ->
            importedModules
        )
        |> Rule.withImportedModulesAPI


all : Test
all =
    Test.describe "Imported modules API"
        [ test "should import the list of imported modules API we have, excluding those from the dependencies if we don't have them" <|
            \() ->
                [ targetModule
                , """module A exposing (..)
b = 1
"""
                ]
                    |> Review.Test.runOnModules (rule (\dict -> Dict.keys dict |> List.map (String.join ".") |> String.join "\n"))
                    |> Review.Test.expectErrorsForModules
                        [ ( "Target"
                          , [ Review.Test.error
                                { message = String.trim """
A
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should import the list of imported modules API we have, including those from the prelude" <|
            \() ->
                [ targetModule
                , """module A exposing (..)
b = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project (rule (\dict -> Dict.keys dict |> List.map (String.join ".") |> String.join "\n"))
                    |> Review.Test.expectErrorsForModules
                        [ ( "Target"
                          , [ Review.Test.error
                                { message = String.trim """
A
Basics
Char
Debug
List
Maybe
Platform
Platform.Cmd
Platform.Sub
Result
String
Tuple
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should be able to list all the exposed functions from a module" <|
            \() ->
                [ targetModule
                , """module A exposing (b, increment, noType)

b : Int
b = 1

hidden = 1

{-| Some comment -}
increment : List Int -> Int
increment int = int + 1

noType = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (rule
                            (\dict ->
                                Dict.get [ "A" ] dict
                                    |> Maybe.map (.values >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Target"
                          , [ Review.Test.error
                                { message = String.trim """
{ comment = "", name = "noType", tipe = Var "unknown" }
{ comment = " Some comment ", name = "increment", tipe = Lambda (Type "List.List" [Type "Basics.Int" []]) (Type "Basics.Int" []) }
{ comment = "", name = "b", tipe = Type "Basics.Int" [] }
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should be able to list all the custom types from a module" <|
            \() ->
                [ targetModule
                , """module A exposing (Opaque, Exposed(..), Complex(..))
type Opaque = Opaque

type Hidden = Hidden
type alias TypeAlias = {}

{-| Some comment -}
type Exposed
    = ExposedConstructor

type Complex a other
    = A Int (List Int)
    | B a
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (rule
                            (\dict ->
                                Dict.get [ "A" ] dict
                                    |> Maybe.map (.unions >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Target"
                          , [ Review.Test.error
                                { message = String.trim """
{ args = ["a","other"], comment = "", name = "Complex", tags = [("A",[Type "Basics.Int" [],Type "List.List" [Type "Basics.Int" []]]),("B",[Var "a"])] }
{ args = [], comment = " Some comment ", name = "Exposed", tags = [("ExposedConstructor",[])] }
{ args = [], comment = "", name = "Opaque", tags = [] }
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should be able to list all the type aliases from a module" <|
            \() ->
                [ targetModule
                , """module A exposing (AliasToInternal, AliasToUnknown, Int, Record)
import B
type alias Record = { a : Int }

type alias Int = Int

type alias Hidden = Int
type CustomType = CustomType

{-| Some comment -}
type alias AliasToInternal = B.Internal

type alias AliasToUnknown = Unknown
"""
                , """module B exposing (Internal)
type Internal = Internal
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (rule
                            (\dict ->
                                Dict.get [ "A" ] dict
                                    |> Maybe.map (.aliases >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Target"
                          , [ Review.Test.error
                                { message = String.trim """
{ args = [], comment = "", name = "AliasToUnknown", tipe = Tuple [] }
{ args = [], comment = " Some comment ", name = "AliasToInternal", tipe = Tuple [] }
{ args = [], comment = "", name = "Int", tipe = Tuple [] }
{ args = [], comment = "", name = "Record", tipe = Tuple [] }
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]


targetModule : String
targetModule =
    """module Target exposing (..)
import A
a = 1
"""


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.ElmCore.dependency
