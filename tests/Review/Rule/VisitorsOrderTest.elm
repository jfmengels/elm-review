module Review.Rule.VisitorsOrderTest exposing (all)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


type alias Context =
    String


all : Test
all =
    Test.describe "Visitor order"
        [ test "should call visitors in a given order" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        Rule.newModuleRuleSchema "Visitor order" "\n0 - initial context"
                            |> Rule.withElmJsonModuleVisitor (\_ context -> context ++ "\n1.1 - withElmJsonModuleVisitor")
                            |> Rule.withElmJsonModuleVisitor (\_ context -> context ++ "\n1.2 - withElmJsonModuleVisitor")
                            |> Rule.withReadmeModuleVisitor (\_ context -> context ++ "\n2.1 - withReadmeModuleVisitor")
                            |> Rule.withReadmeModuleVisitor (\_ context -> context ++ "\n2.2 - withReadmeModuleVisitor")
                            |> Rule.withDependenciesModuleVisitor (\_ context -> context ++ "\n3.1 - withDependenciesModuleVisitor")
                            |> Rule.withDependenciesModuleVisitor (\_ context -> context ++ "\n3.2 - withDependenciesModuleVisitor")
                            |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ++ "\n4.1 - withModuleDefinitionVisitor" ))
                            |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ++ "\n4.2 - withModuleDefinitionVisitor" ))
                            |> Rule.withModuleDocumentationVisitor (\_ context -> ( [], context ++ "\n5.1 - withModuleDocumentationVisitor" ))
                            |> Rule.withModuleDocumentationVisitor (\_ context -> ( [], context ++ "\n5.2 - withModuleDocumentationVisitor" ))
                            |> Rule.withCommentsVisitor (\_ context -> ( [], context ++ "\n6.1 - withCommentsVisitor" ))
                            |> Rule.withCommentsVisitor (\_ context -> ( [], context ++ "\n6.2 - withCommentsVisitor" ))
                            |> Rule.withImportVisitor (\_ context -> ( [], context ++ "\n7.1 - withImportVisitor" ))
                            |> Rule.withImportVisitor (\_ context -> ( [], context ++ "\n7.2 - withImportVisitor" ))
                            |> Rule.withDeclarationListVisitor (\_ context -> ( [], context ++ "\n8.1 - withDeclarationListVisitor" ))
                            |> Rule.withDeclarationListVisitor (\_ context -> ( [], context ++ "\n8.2 - withDeclarationListVisitor" ))
                            |> Rule.withDeclarationEnterVisitor (\_ context -> ( [], context ++ "\n9.1 - withDeclarationEnterVisitor" ))
                            |> Rule.withDeclarationEnterVisitor (\_ context -> ( [], context ++ "\n9.2 - withDeclarationEnterVisitor" ))
                            |> Rule.withDeclarationExitVisitor (\_ context -> ( [], context ++ "\n12.2 - withDeclarationExitVisitor" ))
                            |> Rule.withDeclarationExitVisitor (\_ context -> ( [], context ++ "\n12.1 - withDeclarationExitVisitor" ))
                            |> Rule.withExpressionEnterVisitor (\_ context -> ( [], context ++ "\n10.1 - withExpressionEnterVisitor" ))
                            |> Rule.withExpressionEnterVisitor (\_ context -> ( [], context ++ "\n10.2 - withExpressionEnterVisitor" ))
                            |> Rule.withExpressionExitVisitor (\_ context -> ( [], context ++ "\n11.2 - withExpressionExitVisitor" ))
                            |> Rule.withExpressionExitVisitor (\_ context -> ( [], context ++ "\n11.1 - withExpressionExitVisitor" ))
                            |> Rule.withFinalModuleEvaluation finalEvaluation
                            |> Rule.fromModuleRuleSchema

                    finalEvaluation : Context -> List (Error {})
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                """module A exposing (..)
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
0 - initial context
1.1 - withElmJsonModuleVisitor
1.2 - withElmJsonModuleVisitor
2.1 - withReadmeModuleVisitor
2.2 - withReadmeModuleVisitor
3.1 - withDependenciesModuleVisitor
3.2 - withDependenciesModuleVisitor
4.1 - withModuleDefinitionVisitor
4.2 - withModuleDefinitionVisitor
5.1 - withModuleDocumentationVisitor
5.2 - withModuleDocumentationVisitor
6.1 - withCommentsVisitor
6.2 - withCommentsVisitor
7.1 - withImportVisitor
7.2 - withImportVisitor
8.1 - withDeclarationListVisitor
8.2 - withDeclarationListVisitor
9.1 - withDeclarationEnterVisitor
9.2 - withDeclarationEnterVisitor
10.1 - withExpressionEnterVisitor
10.2 - withExpressionEnterVisitor
11.1 - withExpressionExitVisitor
11.2 - withExpressionExitVisitor
12.1 - withDeclarationExitVisitor
12.2 - withDeclarationExitVisitor"""
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

                    declarationVisitor : String -> Node Expression -> Rule.Direction -> Context -> ( List (Error {}), Context )
                    declarationVisitor text _ direction context =
                        case direction of
                            Rule.OnEnter ->
                                ( [], context ++ "\nEnter " ++ text )

                            Rule.OnExit ->
                                ( [], context ++ "\nExit " ++ text )

                    finalEvaluation : Context -> List (Error {})
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
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

                    declarationVisitor : String -> Node Declaration -> Rule.Direction -> Context -> ( List (Error {}), Context )
                    declarationVisitor text _ direction context =
                        case direction of
                            Rule.OnEnter ->
                                ( [], context ++ "\nEnter " ++ text )

                            Rule.OnExit ->
                                ( [], context ++ "\nExit " ++ text )

                    finalEvaluation : Context -> List (Error {})
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
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
