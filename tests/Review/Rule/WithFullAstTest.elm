module Review.Rule.WithFullAstTest exposing (all)

import Elm.Syntax.File
import Elm.Syntax.Node as Node
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, test)


type alias Context =
    { ast : Elm.Syntax.File.File
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" initContext
        |> Rule.withModuleDefinitionVisitor
            (\node context ->
                ( [ Rule.error
                        { message = "AST: " ++ Debug.toString context.ast
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
        (\ast _ ->
            { ast = ast
            }
        )
        |> Rule.withFullAst


all : Test
all =
    Test.describe "withFullAstTest"
        [ test "should get the module name" <|
            \() ->
                """module A.B exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """AST: { comments = [], declarations = [Node { end = { column = 6, row = 2 }, start = { column = 1, row = 2 } } (FunctionDeclaration { declaration = Node { end = { column = 6, row = 2 }, start = { column = 1, row = 2 } } { arguments = [], expression = Node { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } } (Integer 1), name = Node { end = { column = 2, row = 2 }, start = { column = 1, row = 2 } } "a" }, documentation = Nothing, signature = Nothing })], imports = [], moduleDefinition = Node { end = { column = 25, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } (All { end = { column = 24, row = 1 }, start = { column = 22, row = 1 } }), moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } ["A","B"] }) }"""
                            , details = [ "No details" ]
                            , under = "module A.B exposing (..)"
                            }
                        ]
        ]
