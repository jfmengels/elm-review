module Lint.Internal.ExpressionVisitor exposing (functionToExpression, visit)

import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error exposing (Error)
import Lint.Internal.Accumulate exposing (accumulate, accumulateList)


visit : (Direction -> Node Expression -> context -> ( List Error, context )) -> Node Expression -> context -> ( List Error, context )
visit visitor node context =
    context
        |> visitor Direction.Enter node
        |> accumulateList (visit visitor) (children node)
        |> accumulate (visitor Direction.Exit node)


children : Node Expression -> List (Node Expression)
children node =
    case Node.value node of
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
            List.map (Node.value >> (\( name, expr ) -> expr)) fields

        RecordUpdateExpression name setters ->
            List.map (Node.value >> (\( field, expr ) -> expr)) setters

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
                    case Node.value declaration of
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


functionToExpression : Function -> Node Expression
functionToExpression function =
    Node.value function.declaration
        |> .expression
