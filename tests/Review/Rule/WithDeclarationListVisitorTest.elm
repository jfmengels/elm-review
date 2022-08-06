module Review.Rule.WithDeclarationListVisitorTest exposing (all)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Rule.withDeclarationListVisitor"
        [ test "passes the list of declarations to the rule" <|
            \() ->
                """port module ModuleName exposing (b)
type A = Bar | Baz
a_ = 1
b_ = 2
port output : Json.Encode.Value -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Declarations"
                            , details = [ "A", "a_", "b_", "output", "input" ]
                            , under = "port module"
                            }
                        ]
        ]


rule : Rule
rule =
    Rule.newModuleRuleSchema "WithDeclarationListVisitorTestRule" ()
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.fromModuleRuleSchema


declarationListVisitor : List (Node Declaration) -> () -> ( List (Error {}), () )
declarationListVisitor declarations context =
    let
        namesInOrder : List (Node String)
        namesInOrder =
            List.concatMap
                (\node ->
                    case Node.value node of
                        Declaration.FunctionDeclaration function ->
                            [ function.declaration
                                |> Node.value
                                |> .name
                            ]

                        Declaration.AliasDeclaration aliasDeclaration ->
                            [ aliasDeclaration.name ]

                        Declaration.CustomTypeDeclaration type_ ->
                            [ type_.name ]

                        Declaration.PortDeclaration signature ->
                            [ signature.name ]

                        Declaration.InfixDeclaration _ ->
                            []

                        Declaration.Destructuring _ _ ->
                            []
                )
                declarations
    in
    ( [ Rule.error
            { message = "Declarations"
            , details = List.map Node.value namesInOrder
            }
            { start = { row = 1, column = 1 }
            , end = { row = 1, column = 12 }
            }
      ]
    , context
    )
