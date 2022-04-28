module Review.Rule.WithFilePathTest exposing (all)

import Elm.Syntax.Node as Node
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, test)


type alias Context =
    { filePath : String
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" initContext
        |> Rule.withModuleDefinitionVisitor
            (\node context ->
                ( [ Rule.error
                        { message = "File path: " ++ context.filePath
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
        (\filePath _ ->
            { filePath = filePath
            }
        )
        |> Rule.withFilePath


all : Test
all =
    Test.describe "withFilePath"
        [ test "should not pass the elmJsonKey if the `elm.json` file does not exist" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "File path: src/File_0.elm"
                            , details = [ "No details" ]
                            , under = "module A exposing (..)"
                            }
                        ]
        ]
