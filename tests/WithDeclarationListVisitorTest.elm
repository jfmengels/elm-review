module WithDeclarationListVisitorTest exposing (all)

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
                Review.Test.run rule """module ModuleName exposing (b)
type A = Bar | Baz
a_ = 1
b_ = 2
port output : Json.Encode.Value -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "A"
                            , details = [ "some details" ]
                            , under = "A"
                            }
                        , Review.Test.error
                            { message = "a_"
                            , details = [ "some details" ]
                            , under = "a_"
                            }
                        , Review.Test.error
                            { message = "b_"
                            , details = [ "some details" ]
                            , under = "b_"
                            }
                        , Review.Test.error
                            { message = "output"
                            , details = [ "some details" ]
                            , under = "output"
                            }
                        , Review.Test.error
                            { message = "input"
                            , details = [ "some details" ]
                            , under = "input"
                            }
                        ]
        ]


rule : Rule
rule =
    Rule.newModuleRuleSchema "WithDeclarationListVisitorTestRule" ()
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.fromModuleRuleSchema


declarationListVisitor : List (Node Declaration) -> () -> ( List Error, () )
declarationListVisitor declarations context =
    let
        errors : List Error
        errors =
            List.concatMap
                (\node ->
                    case Node.value node of
                        Declaration.FunctionDeclaration function ->
                            [ function.declaration
                                |> Node.value
                                |> .name
                                |> errorFromNode
                            ]

                        Declaration.AliasDeclaration aliasDeclaration ->
                            [ errorFromNode aliasDeclaration.name
                            ]

                        Declaration.CustomTypeDeclaration type_ ->
                            [ errorFromNode type_.name
                            ]

                        Declaration.PortDeclaration signature ->
                            [ errorFromNode signature.name
                            ]

                        Declaration.InfixDeclaration _ ->
                            []

                        Declaration.Destructuring _ _ ->
                            []
                )
                declarations
    in
    ( errors, context )


errorFromNode : Node String -> Error
errorFromNode nameNode =
    Rule.error
        { message = Node.value nameNode
        , details = details
        }
        (Node.range nameNode)


details : List String
details =
    [ "some details" ]
