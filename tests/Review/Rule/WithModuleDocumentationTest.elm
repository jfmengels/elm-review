module Review.Rule.WithModuleDocumentationTest exposing (all)

import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, test)


type alias Context =
    { moduleDocumentation : Maybe (Node String)
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" initContext
        |> Rule.withModuleDefinitionVisitor
            (\node context ->
                ( [ Rule.error
                        { message =
                            case context.moduleDocumentation of
                                Just moduleDocumentation ->
                                    Node.value moduleDocumentation

                                Nothing ->
                                    "MISSING DOCUMENTATION"
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
        (\moduleDocumentation _ ->
            { moduleDocumentation = moduleDocumentation
            }
        )
        |> Rule.withModuleDocumentation


all : Test
all =
    test "should pass the module documentation if there is one" <|
        \() ->
            """module ModuleName exposing (a)
{-| module documentation
-}

{-| function doc
-}
a = 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "{-| module documentation\n-}"
                        , details = [ "No details" ]
                        , under = "module ModuleName exposing (a)"
                        }
                    ]
