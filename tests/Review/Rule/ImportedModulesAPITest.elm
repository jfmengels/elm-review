module Review.Rule.ImportedModulesAPITest exposing (all)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "ImportedModulesAPITest" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
moduleDefinitionVisitor node context =
    if Elm.Syntax.Module.moduleName (Node.value node) == [ "Target" ] then
        let
            message : String
            message =
                Dict.keys context |> List.map (String.join ".") |> String.join "\n"
        in
        ( [ Rule.error { message = message, details = [ "details" ] }
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
        [ test "should import the list of imported modules API we have, including those from the prelude" <|
            \() ->
                [ """module Target exposing (..)
import A
a = 1
"""
                , """module A exposing (..)
b = 1
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Target"
                          , [ Review.Test.error
                                { message = String.trim """
A
Basics
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]
