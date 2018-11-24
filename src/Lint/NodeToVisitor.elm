module Lint.NodeToVisitor exposing (createVisitorsForFile, expressionToVisitors)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node exposing (Node, value)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Lint.Rule exposing (Direction(..), Visitor, evaluateDeclaration, evaluateExpression, evaluateImport, evaluateModuleDefinition, finalEvaluation)


createExitAndEnterWithChildren : (Direction -> nodeType -> Visitor context) -> nodeType -> List (Visitor context) -> List (Visitor context)
createExitAndEnterWithChildren toVisitor node children =
    List.concat
        [ [ toVisitor Enter node ]
        , children
        , [ toVisitor Exit node ]
        ]


moduleVisitor : Visitor context
moduleVisitor rule context =
    finalEvaluation rule context


moduleDefinitionVisitor : Node Module -> Visitor context
moduleDefinitionVisitor node rule context =
    evaluateModuleDefinition rule context node


importVisitor : Node Import -> Visitor context
importVisitor node rule context =
    evaluateImport rule context node


expressionVisitor : Direction -> Node Expression -> Visitor context
expressionVisitor direction node rule context =
    evaluateExpression rule context direction node


declarationVisitor : Direction -> Node Declaration -> Visitor context
declarationVisitor direction node rule context =
    evaluateDeclaration rule context direction node


functionToExpression : Function -> Node Expression
functionToExpression { documentation, signature, declaration } =
    let
        { name, arguments, expression } =
            value declaration
    in
    expression


expressionToVisitors : Node Expression -> List (Visitor context)
expressionToVisitors node =
    let
        children : List (Node Expression)
        children =
            case value node of
                Application expressions ->
                    expressions

                Literal _ ->
                    []

                Integer _ ->
                    []

                Floatable _ ->
                    []

                UnitExpr ->
                    []

                ListExpr elements ->
                    elements

                FunctionOrValue _ _ ->
                    []

                RecordExpr fields ->
                    List.map (value >> (\( name, expr ) -> expr)) fields

                RecordUpdateExpression name setters ->
                    List.map (value >> (\( field, expr ) -> expr)) setters

                ParenthesizedExpression expr ->
                    [ expr ]

                Operator name ->
                    []

                OperatorApplication operator direction left right ->
                    case direction of
                        Left ->
                            [ left, right ]

                        Right ->
                            [ right, left ]

                        Non ->
                            [ left, right ]

                IfBlock cond then_ else_ ->
                    [ cond, then_, else_ ]

                LetExpression { expression, declarations } ->
                    List.map
                        (\declaration ->
                            case value declaration of
                                LetFunction function ->
                                    functionToExpression function

                                LetDestructuring pattern expr ->
                                    expr
                        )
                        declarations
                        ++ [ expression ]

                CaseExpression { expression, cases } ->
                    [ expression ]
                        ++ List.map (\( pattern, caseExpression ) -> caseExpression) cases

                LambdaExpression { args, expression } ->
                    [ expression ]

                TupledExpression expressions ->
                    expressions

                PrefixOperator name ->
                    []

                Hex _ ->
                    []

                Negation expr ->
                    [ expr ]

                CharLiteral _ ->
                    []

                RecordAccess expr property ->
                    [ expr ]

                RecordAccessFunction name ->
                    []

                GLSLExpression expr ->
                    []

        childrenVisitors =
            List.concatMap expressionToVisitors children
    in
    createExitAndEnterWithChildren expressionVisitor node childrenVisitors


declarationToVisitors : Node Declaration -> List (Visitor context)
declarationToVisitors node =
    let
        childrenVisitors =
            case value node of
                FunctionDeclaration function ->
                    functionToExpression function |> expressionToVisitors

                CustomTypeDeclaration _ ->
                    []

                AliasDeclaration { typeAnnotation } ->
                    []

                Destructuring pattern expr ->
                    expressionToVisitors expr

                PortDeclaration _ ->
                    []

                InfixDeclaration _ ->
                    []
    in
    createExitAndEnterWithChildren declarationVisitor node childrenVisitors


declarationsIntoVisitors : List (Node Declaration) -> List (Visitor context)
declarationsIntoVisitors declarations =
    List.concatMap declarationToVisitors declarations


importsIntoVisitors : List (Node Import) -> List (Visitor context)
importsIntoVisitors imports =
    List.map importVisitor imports


moduleDefinitionIntoVisitor : Node Module -> Visitor context
moduleDefinitionIntoVisitor moduleNode =
    moduleDefinitionVisitor moduleNode


createVisitorsForFile : File -> List (Visitor context)
createVisitorsForFile file =
    [ moduleDefinitionIntoVisitor file.moduleDefinition ]
        ++ importsIntoVisitors file.imports
        ++ declarationsIntoVisitors file.declarations
        ++ [ moduleVisitor ]
