module Util exposing (expressionToString)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Writer as Writer


expressionToString : Range -> Expression -> String
expressionToString range expr =
    Node.Node range expr
        |> fixAst
        |> Writer.writeExpression
        |> Writer.write
        -- To make things more parseable by our dear old elm-format, we indent all extra
        -- lines so they appear to the right of where we're starting at
        |> reindent range.start.column


fixAst : Node Expression -> Node Expression
fixAst ((Node.Node range expr) as node) =
    case expr of
        Expression.Application nodes ->
            Node.Node range (Expression.Application (List.map fixAst nodes))

        Expression.OperatorApplication op dir left right ->
            Node.Node range
                (Expression.OperatorApplication op dir (fixAst left) (fixAst right))

        Expression.IfBlock cond trueBranch falseBranch ->
            Node.Node range
                (Expression.IfBlock (fixAst cond) (fixAst trueBranch) (fixAst falseBranch))

        Expression.Negation inner ->
            Node.Node range (Expression.Negation (fixAst inner))

        Expression.TupledExpression nodes ->
            Node.Node range (Expression.TupledExpression (List.map fixAst nodes))

        Expression.ParenthesizedExpression inner ->
            Node.Node range (Expression.ParenthesizedExpression (fixAst inner))

        Expression.LetExpression { declarations, expression } ->
            Node.Node range
                (Expression.LetExpression
                    { declarations = List.map fixLetDeclarationAst declarations
                    , expression = fixAst expression
                    }
                )

        Expression.CaseExpression { expression, cases } ->
            Node.Node range
                (Expression.CaseExpression
                    { expression = fixAst expression
                    , cases = List.map fixCaseAst cases
                    }
                )

        Expression.LambdaExpression { args, expression } ->
            Node.Node range
                (Expression.LambdaExpression
                    { args = args
                    , expression = fixAst expression
                    }
                )

        Expression.RecordExpr setters ->
            Node.Node range
                (Expression.RecordExpr (List.map fixSetterAst setters))

        Expression.ListExpr nodes ->
            Node.Node range (Expression.ListExpr (List.map fixAst nodes))

        Expression.RecordAccess which field ->
            Node.Node range (Expression.RecordAccess (fixAst which) field)

        Expression.RecordAccessFunction field ->
            -- Writer expects a record-access function to be `"foo"` whereas the parser produces `".foo"`
            Node.Node range (Expression.RecordAccessFunction (String.dropLeft 1 field))

        Expression.RecordUpdateExpression record setters ->
            Node.Node range (Expression.RecordUpdateExpression record (List.map fixSetterAst setters))

        _ ->
            node


fixCaseAst : Expression.Case -> Expression.Case
fixCaseAst ( pat, node ) =
    ( pat, fixAst node )


fixLetDeclarationAst : Node Expression.LetDeclaration -> Node Expression.LetDeclaration
fixLetDeclarationAst (Node.Node range decl) =
    case decl of
        Expression.LetFunction ({ declaration } as f) ->
            let
                (Node.Node iRange iDecl) =
                    declaration
            in
            Node.Node range
                (Expression.LetFunction
                    { f
                        | declaration = Node.Node iRange { iDecl | expression = fixAst iDecl.expression }
                    }
                )

        Expression.LetDestructuring pat node ->
            Node.Node range (Expression.LetDestructuring pat (fixAst node))


fixSetterAst : Node Expression.RecordSetter -> Node Expression.RecordSetter
fixSetterAst (Node.Node range ( name, node )) =
    Node.Node range ( name, fixAst node )


reindent : Int -> String -> String
reindent amount =
    String.lines >> String.join ("\n" ++ String.repeat (amount - 1) " ")
