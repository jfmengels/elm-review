module Review.Rule.WithModuleNameTest exposing (all)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, test)


type alias Context =
    { moduleName : ModuleName
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" initContext
        |> Rule.withModuleDefinitionVisitor
            (\node context ->
                ( [ Rule.error
                        { message = "ModuleName: " ++ Debug.toString context.moduleName
                        , details = [ "No details" ]
                        }
                        (Node.range node)
                  ]
                , context
                )
            )
        |> Rule.fromModuleRuleSchema


initContext : Rule.ContextCreator () Context
initContext =
    Rule.initContextCreator
        (\moduleName _ ->
            { moduleName = moduleName
            }
        )
        |> Rule.withModuleName


all : Test
all =
    Test.describe "withModuleNameTest"
        [ test "should get the module name" <|
            \() ->
                """module A.B exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "ModuleName: [\"A\",\"B\"]"
                            , details = [ "No details" ]
                            , under = "module A.B exposing (..)"
                            }
                        ]
        ]
