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
        [ test "should call visitors in a given order" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        Rule.newModuleRuleSchema "TestRule" "\n0 - withInitialContext"
                            |> Rule.withElmJsonVisitor (\_ context -> context ++ "\n1 - withElmJsonVisitor")
                            |> Rule.withDependenciesVisitor (\_ context -> context ++ "\n2 - withDependenciesVisitor")
                            |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ++ "\n3 - withModuleDefinitionVisitor" ))
                            |> Rule.withImportVisitor (\_ context -> ( [], context ++ "\n4 - withImportVisitor" ))
                            |> Rule.withDeclarationListVisitor (\_ context -> ( [], context ++ "\n5 - withDeclarationListVisitor" ))
                            |> Rule.withDeclarationVisitor
                                (\node direction context ->
                                    case direction of
                                        Rule.OnEnter ->
                                            ( [], context ++ "\n6 - withDeclarationVisitor (Enter)" )

                                        Rule.OnExit ->
                                            ( [], context ++ "\n9 - withDeclarationVisitor (Exit)" )
                                )
                            |> Rule.withExpressionVisitor
                                (\node direction context ->
                                    case direction of
                                        Rule.OnEnter ->
                                            ( [], context ++ "\n7 - withExpressionVisitor (Enter)" )

                                        Rule.OnExit ->
                                            ( [], context ++ "\n8 - withExpressionVisitor (Exit)" )
                                )
                            |> Rule.withFinalModuleEvaluation finalEvaluation
                            |> Rule.fromModuleRuleSchema

                    finalEvaluation : Context -> List Rule.Error
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                testRule rule """
import A
a = 1
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
0 - withInitialContext
1 - withElmJsonVisitor
2 - withDependenciesVisitor
3 - withModuleDefinitionVisitor
4 - withImportVisitor
5 - withDeclarationListVisitor
6 - withDeclarationVisitor (Enter)
7 - withExpressionVisitor (Enter)
8 - withExpressionVisitor (Exit)
9 - withDeclarationVisitor (Exit)"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        , test "should call the same type of visitors in order of call on enter, and reverse order on exit (expression)" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        Rule.newModuleRuleSchema "TestRule" ""
                            |> Rule.withExpressionVisitor (declarationVisitor "A")
                            |> Rule.withExpressionVisitor (declarationVisitor "B")
                            |> Rule.withExpressionVisitor (declarationVisitor "C")
                            |> Rule.withFinalModuleEvaluation finalEvaluation
                            |> Rule.fromModuleRuleSchema

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
                        Rule.newModuleRuleSchema "TestRule" ""
                            |> Rule.withDeclarationVisitor (declarationVisitor "A")
                            |> Rule.withDeclarationVisitor (declarationVisitor "B")
                            |> Rule.withDeclarationVisitor (declarationVisitor "C")
                            |> Rule.withFinalModuleEvaluation finalEvaluation
                            |> Rule.fromModuleRuleSchema

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
