module Review.Rule.ImportedModulesAPITest exposing (all)

import Dependencies.ElmCore
import Dict exposing (Dict)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Expect exposing (Expectation)
import Review.Api.Alias as Alias
import Review.Api.Binop as Binop
import Review.Api.Module as ModuleApi exposing (ModuleApi)
import Review.Api.Union as Union
import Review.Api.Value as Value
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


rule : (Dict ModuleName ModuleApi -> String) -> Rule
rule whatToPrint =
    Rule.newModuleRuleSchemaUsingContextCreator "ImportedModulesAPITest" initialContext
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor whatToPrint)
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : (Dict ModuleName ModuleApi -> String) -> Node Module -> Context -> ( List (Error {}), Context )
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
    Dict ModuleName ModuleApi


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\importedModules () -> importedModules)
        |> Rule.withImportedModulesApi


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
increment : List Int -> Foo -> Local
increment int _ = Local

type Local = Local

noType = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (rule
                            (\dict ->
                                Dict.get [ "A" ] dict
                                    |> Maybe.map (ModuleApi.values >> List.sortBy Value.name >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> expectToFind """
Value { documentation = Nothing, name = "b", tipe = Type ["Basics"] "Int" [] }
Value { documentation = Just " Some comment ", name = "increment", tipe = Function (Type ["List"] "List" [Type ["Basics"] "Int" []]) (Function (Type ["A"] "Foo" []) (Type ["A"] "Local" [])) }
Value { documentation = Nothing, name = "noType", tipe = Unknown }
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
                                    |> Maybe.map (ModuleApi.unions >> List.sortBy Union.name >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> expectToFind """
Union { args = ["a","other"], constructors = Dict.fromList [("A",[Type ["Basics"] "Int" [],Type ["List"] "List" [Type ["Basics"] "Int" []]]),("B",[Generic "a"])], documentation = Nothing, name = "Complex" }
Union { args = [], constructors = Dict.fromList [("ExposedConstructor",[Type ["A"] "Opaque" []])], documentation = Just " Some comment ", name = "Exposed" }
Union { args = [], constructors = Dict.fromList [], documentation = Nothing, name = "Opaque" }
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
                                    |> Maybe.map (ModuleApi.aliases >> Dict.values >> List.sortBy Alias.name >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> expectToFind """
Alias { args = [], documentation = Just " Some comment ", name = "AliasToInternal", tipe = Type ["B"] "Internal" [] }
Alias { args = [], documentation = Nothing, name = "AliasToUnknown", tipe = Type ["A"] "Unknown" [] }
Alias { args = ["thing"], documentation = Nothing, name = "ExtensibleRecord", tipe = Record { fields = [("a",Type ["A"] "Int" [])], generic = Just "thing", mayHaveMoreFields = False } }
Alias { args = [], documentation = Nothing, name = "Int", tipe = Type ["A"] "Int" [] }
Alias { args = [], documentation = Nothing, name = "Record", tipe = Record { fields = [("a",Type ["A"] "Int" [])], generic = Nothing, mayHaveMoreFields = False } }
"""
        , test "should be able to list all the binary operations from a module" <|
            \() ->
                [ targetModule
                , """module A exposing ((+++), (---), (<=>))
{-| Do things with matrices -}
thing1 : Matrix -> Matrix -> Matrix
thing1 a b = b

infix right 0 (+++) = thing1
infix left  99 (---) = thing2
infix non 2 (<=>) = thing3

thing2 a b = b

{-| Some comment -}
thing3 : Int -> Int -> Int
thing3 a b = a + b
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (rule
                            (\dict ->
                                Dict.get [ "A" ] dict
                                    |> Maybe.map (ModuleApi.binops >> List.sortBy Binop.name >> List.map Debug.toString >> String.join "\n")
                                    |> Maybe.withDefault "ERROR: MODULE WAS WAS FOUND"
                            )
                        )
                    |> expectToFind """
Binop { associatedFunction = Just "thing1", associativity = Right, documentation = Just "{-| Do things with matrices -}", name = "+++", precedence = 0, tipe = Just (Function (Type ["A"] "Matrix" []) (Function (Type ["A"] "Matrix" []) (Type ["A"] "Matrix" []))) }
Binop { associatedFunction = Just "thing2", associativity = Left, documentation = Nothing, name = "---", precedence = 99, tipe = Nothing }
Binop { associatedFunction = Just "thing3", associativity = None, documentation = Just "{-| Some comment -}", name = "<=>", precedence = 2, tipe = Just (Function (Type ["Basics"] "Int" []) (Function (Type ["Basics"] "Int" []) (Type ["Basics"] "Int" []))) }
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
