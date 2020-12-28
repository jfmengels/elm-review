module Review.Rule.ImportedModulesAPITest exposing (all)

import Dependencies.ElmCore
import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Expect exposing (Expectation)
import Review.ModuleInformation as ModuleInformation
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
            ModuleInformation.toElmDocsModuleDict importedModules
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
                    |> expectToFind """
A
"""
        , test "should import the list of imported modules API we have, including those from the prelude" <|
            \() ->
                [ targetModule
                , """module A exposing (..)
b = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project (rule (\dict -> Dict.keys dict |> List.map (String.join ".") |> String.join "\n"))
                    |> expectToFind """
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
        , test "should be able to list all the exposed functions from a module" <|
            \() ->
                [ targetModule
                , """module A exposing (b, increment, noType)

b : Int
b = 1

hidden = 1

{-| Some comment -}
increment : List Int -> Local
increment int = Local

type Local = Local

noType = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (rule
                            (\dict ->
                                Dict.get [ "A" ] dict
                                    |> Maybe.map (.values >> List.sortBy .name >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> expectToFind """
{ comment = "", name = "b", tipe = Type "Basics.Int" [] }
{ comment = " Some comment ", name = "increment", tipe = Lambda (Type "List.List" [Type "Basics.Int" []]) (Type "A.Local" []) }
{ comment = "", name = "noType", tipe = Var "unknown" }
"""
        , test "should be able to list all the custom types from a module" <|
            \() ->
                [ targetModule
                , """module A exposing (Opaque, Exposed(..), Complex(..))
type Opaque = Opaque

type Hidden = Hidden
type alias TypeAlias = {}

{-| Some comment -}
type Exposed
    = ExposedConstructor Opaque

type Complex a other
    = A Int (List Int)
    | B a
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (rule
                            (\dict ->
                                Dict.get [ "A" ] dict
                                    |> Maybe.map (.unions >> List.sortBy .name >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> expectToFind """
{ args = ["a","other"], comment = "", name = "Complex", tags = [("A",[Type "Basics.Int" [],Type "List.List" [Type "Basics.Int" []]]),("B",[Var "a"])] }
{ args = [], comment = " Some comment ", name = "Exposed", tags = [("ExposedConstructor",[Type "A.Opaque" []])] }
{ args = [], comment = "", name = "Opaque", tags = [] }
"""
        , test "should be able to list all the type aliases from a module" <|
            \() ->
                [ targetModule
                , """module A exposing (AliasToInternal, AliasToUnknown, ExtensibleRecord, Int, Record)
import B
type alias Record = { a : Int }
type alias ExtensibleRecord thing = { thing | a : Int }

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
                                    |> Maybe.map (.aliases >> List.sortBy .name >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> expectToFind """
{ args = [], comment = " Some comment ", name = "AliasToInternal", tipe = Type "B.Internal" [] }
{ args = [], comment = "", name = "AliasToUnknown", tipe = Type "A.Unknown" [] }
{ args = ["thing"], comment = "", name = "ExtensibleRecord", tipe = Record [("a",Type "A.Int" [])] (Just "thing") }
{ args = [], comment = "", name = "Int", tipe = Type "A.Int" [] }
{ args = [], comment = "", name = "Record", tipe = Record [("a",Type "A.Int" [])] Nothing }
"""
        ]


expectToFind : String -> Review.Test.ReviewResult -> Expectation
expectToFind message =
    Review.Test.expectErrorsForModules
        [ ( "Target"
          , [ Review.Test.error
                { message = String.trim message
                , details = [ "details" ]
                , under = "module"
                }
            ]
          )
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
