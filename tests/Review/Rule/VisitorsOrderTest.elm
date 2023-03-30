module Review.Rule.VisitorsOrderTest exposing (all)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
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
                    importName : Node Import -> String
                    importName (Node _ import_) =
                        String.join "." (Node.value import_.moduleName)

                    rule : Rule
                    rule =
                        Rule.newModuleRuleSchema "Visitor order" "\n0 - initial context"
                            |> Rule.withElmJsonModuleVisitor (\_ context -> context ++ "\n1.1 - withElmJsonModuleVisitor")
                            |> Rule.withElmJsonModuleVisitor (\_ context -> context ++ "\n1.2 - withElmJsonModuleVisitor")
                            |> Rule.withReadmeModuleVisitor (\_ context -> context ++ "\n2.1 - withReadmeModuleVisitor")
                            |> Rule.withReadmeModuleVisitor (\_ context -> context ++ "\n2.2 - withReadmeModuleVisitor")
                            |> Rule.withDirectDependenciesModuleVisitor (\_ context -> context ++ "\n3.1 - withDirectDependenciesModuleVisitor")
                            |> Rule.withDirectDependenciesModuleVisitor (\_ context -> context ++ "\n3.2 - withDirectDependenciesModuleVisitor")
                            |> Rule.withDependenciesModuleVisitor (\_ context -> context ++ "\n3.3 - withDependenciesModuleVisitor")
                            |> Rule.withDependenciesModuleVisitor (\_ context -> context ++ "\n3.4 - withDependenciesModuleVisitor")
                            |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ++ "\n4.1 - withModuleDefinitionVisitor" ))
                            |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ++ "\n4.2 - withModuleDefinitionVisitor" ))
                            |> Rule.withModuleDocumentationVisitor (\_ context -> ( [], context ++ "\n5.1 - withModuleDocumentationVisitor" ))
                            |> Rule.withModuleDocumentationVisitor (\_ context -> ( [], context ++ "\n5.2 - withModuleDocumentationVisitor" ))
                            |> Rule.withCommentsVisitor (\_ context -> ( [], context ++ "\n6.1 - withCommentsVisitor" ))
                            |> Rule.withCommentsVisitor (\_ context -> ( [], context ++ "\n6.2 - withCommentsVisitor" ))
                            |> Rule.withImportVisitor (\import_ context -> ( [], context ++ "\n7.1 - withImportVisitor " ++ importName import_ ))
                            |> Rule.withImportVisitor (\import_ context -> ( [], context ++ "\n7.2 - withImportVisitor " ++ importName import_ ))
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
import C
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
3.1 - withDirectDependenciesModuleVisitor
3.2 - withDirectDependenciesModuleVisitor
3.3 - withDependenciesModuleVisitor
3.4 - withDependenciesModuleVisitor
4.1 - withModuleDefinitionVisitor
4.2 - withModuleDefinitionVisitor
5.1 - withModuleDocumentationVisitor
5.2 - withModuleDocumentationVisitor
6.1 - withCommentsVisitor
6.2 - withCommentsVisitor
7.1 - withImportVisitor B
7.2 - withImportVisitor B
7.1 - withImportVisitor C
7.2 - withImportVisitor C
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
        , test "should call the same type of visitors in order of call on enter, and reverse order on exit (let declaration)" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        Rule.newModuleRuleSchema "TestRule" ""
                            |> Rule.withLetDeclarationEnterVisitor (letDeclarationVisitor "A enter")
                            |> Rule.withLetDeclarationEnterVisitor (letDeclarationVisitor "B enter")
                            |> Rule.withLetDeclarationExitVisitor (letDeclarationVisitor "C exit")
                            |> Rule.withLetDeclarationExitVisitor (letDeclarationVisitor "D exit")
                            |> Rule.withFinalModuleEvaluation finalEvaluation
                            |> Rule.fromModuleRuleSchema

                    letDeclarationName : Node Expression.LetDeclaration -> String
                    letDeclarationName letDeclaration =
                        case Node.value letDeclaration of
                            Expression.LetFunction { declaration } ->
                                declaration |> Node.value |> .name |> Node.value

                            Expression.LetDestructuring _ _ ->
                                "NOT RELEVANT"

                    letDeclarationVisitor : String -> Node Expression.LetBlock -> Node Expression.LetDeclaration -> Context -> ( List (Error {}), Context )
                    letDeclarationVisitor text _ letDeclaration context =
                        ( [], context ++ "\n" ++ text ++ ": " ++ letDeclarationName letDeclaration )

                    finalEvaluation : Context -> List (Error {})
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                """module A exposing (..)
a =
  let
     b = 1
     c n = n
  in
  b + c 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
A enter: b
B enter: b
D exit: b
C exit: b
A enter: c
B enter: c
D exit: c
C exit: c"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        , test "should call the same type of visitors in order of call on enter, and reverse order on exit (case branch)" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        Rule.newModuleRuleSchema "TestRule" ""
                            |> Rule.withCaseBranchEnterVisitor (caseBranchVisitor "A enter")
                            |> Rule.withCaseBranchEnterVisitor (caseBranchVisitor "B enter")
                            |> Rule.withCaseBranchExitVisitor (caseBranchVisitor "C exit")
                            |> Rule.withCaseBranchExitVisitor (caseBranchVisitor "D exit")
                            |> Rule.withFinalModuleEvaluation finalEvaluation
                            |> Rule.fromModuleRuleSchema

                    caseBranchVisitor : String -> Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> Context -> ( List (Error {}), Context )
                    caseBranchVisitor text _ ( pattern, _ ) context =
                        ( [], context ++ "\n" ++ text ++ ": " ++ Debug.toString (Node.value pattern) )

                    finalEvaluation : Context -> List (Error {})
                    finalEvaluation context =
                        [ Rule.error { message = context, details = [ "details" ] }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                        ]
                in
                """module A exposing (..)
a =
  case x of
    1 -> b
    _ -> c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
A enter: IntPattern 1
B enter: IntPattern 1
D exit: IntPattern 1
C exit: IntPattern 1
A enter: AllPattern
B enter: AllPattern
D exit: AllPattern
C exit: AllPattern"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        ]
