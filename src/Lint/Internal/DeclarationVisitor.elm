module Lint.Internal.DeclarationVisitor exposing (visit)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error exposing (Error)
import Lint.Internal.Accumulate exposing (accumulate, accumulateList)
import Lint.Internal.ExpressionVisitor as ExpressionVisitor


visit :
    (Node Declaration -> Direction -> context -> ( List Error, context ))
    -> (Node Expression -> Direction -> context -> ( List Error, context ))
    -> Node Declaration
    -> context
    -> ( List Error, context )
visit declarationVisitor expressionVisitor node context =
    context
        |> declarationVisitor node Direction.Enter
        |> accumulateList (ExpressionVisitor.visit expressionVisitor) (expressionChildren node)
        |> accumulate (declarationVisitor node Direction.Exit)


expressionChildren : Node Declaration -> List (Node Expression)
expressionChildren node =
    case Node.value node of
        FunctionDeclaration function ->
            [ ExpressionVisitor.functionToExpression function ]

        CustomTypeDeclaration _ ->
            []

        AliasDeclaration { typeAnnotation } ->
            []

        Destructuring pattern expr ->
            [ expr ]

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []
