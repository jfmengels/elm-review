module Lint.Internal.DeclarationVisitor exposing (visit)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error exposing (Error)
import Lint.Internal.Accumulate exposing (accumulate, accumulateList)
import Lint.Internal.ExpressionVisitor as ExpressionVisitor


visit :
    (Direction -> Node Declaration -> context -> ( List Error, context ))
    -> (Direction -> Node Expression -> context -> ( List Error, context ))
    -> Node Declaration
    -> context
    -> ( List Error, context )
visit declarationVisitor expressionVisitor node context =
    context
        |> declarationVisitor Direction.Enter node
        |> accumulateList (ExpressionVisitor.visit expressionVisitor) (expressionChildren node)
        |> accumulate (declarationVisitor Direction.Exit node)


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
