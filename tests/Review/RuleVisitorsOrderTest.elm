module Review.RuleVisitorsOrderTest exposing (all)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Review.Rule as Rule exposing (Rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, test)


type alias Context =
    String


testRule : Rule -> String -> ReviewResult
testRule rule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


all : Test
all =
    Test.describe "Visitor order"
        [ test "should call the same type of visitors in order of call on enter, and reverse order on exit (expression)" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        Rule.newSchema "TestRule"
                            |> Rule.withInitialContext ""
                            |> Rule.withExpressionVisitor (declarationVisitor "A")
                            |> Rule.withExpressionVisitor (declarationVisitor "B")
                            |> Rule.withExpressionVisitor (declarationVisitor "C")
                            |> Rule.withFinalEvaluation finalEvaluation
                            |> Rule.fromSchema

                    declarationVisitor : String -> Node Expression -> Rule.Direction -> Context -> ( List Rule.Error, Context )
                    declarationVisitor text node direction context =
                        case direction of
                            Rule.OnEnter ->
                                ( [], context ++ "\nEnter " ++ text )

                            Rule.OnExit ->
                                ( [], context ++ "\nExit " ++ text )

                    finalEvaluation : Context -> List Rule.Error
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                testRule rule """
a = 1
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
Enter A
Enter B
Enter C
Exit C
Exit B
Exit A"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        , test "should call the same type of visitors in order of call on enter, and reverse order on exit (declaration)" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        Rule.newSchema "TestRule"
                            |> Rule.withInitialContext ""
                            |> Rule.withDeclarationVisitor (declarationVisitor "A")
                            |> Rule.withDeclarationVisitor (declarationVisitor "B")
                            |> Rule.withDeclarationVisitor (declarationVisitor "C")
                            |> Rule.withFinalEvaluation finalEvaluation
                            |> Rule.fromSchema

                    declarationVisitor : String -> Node Declaration -> Rule.Direction -> Context -> ( List Rule.Error, Context )
                    declarationVisitor text node direction context =
                        case direction of
                            Rule.OnEnter ->
                                ( [], context ++ "\nEnter " ++ text )

                            Rule.OnExit ->
                                ( [], context ++ "\nExit " ++ text )

                    finalEvaluation : Context -> List Rule.Error
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                testRule rule """
a = 1
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
Enter A
Enter B
Enter C
Exit C
Exit B
Exit A"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        ]
