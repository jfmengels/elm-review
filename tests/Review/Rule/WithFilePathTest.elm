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
        [ test "should get the file path of a single module" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "File path: src/A.elm"
                            , details = [ "No details" ]
                            , under = "module A exposing (..)"
                            }
                        ]
        , test "should get the file paths of multiple modules" <|
            \() ->
                [ """module A exposing (..)
a = 1
""", """module A.B.C exposing (..)
a = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "File path: src/A.elm"
                                , details = [ "No details" ]
                                , under = "module A exposing (..)"
                                }
                            ]
                          )
                        , ( "A.B.C"
                          , [ Review.Test.error
                                { message = "File path: src/A/B/C.elm"
                                , details = [ "No details" ]
                                , under = "module A.B.C exposing (..)"
                                }
                            ]
                          )
                        ]
        ]
