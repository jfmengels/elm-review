module Lint.Visitor exposing (transformStatementsIntoVisitors, expressionToVisitors)

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Lint.Types exposing (Direction(..), Visitor)


createExitAndEnterWithChildren : (Direction nodeType -> Visitor context) -> nodeType -> List (Visitor context) -> List (Visitor context)
createExitAndEnterWithChildren toVisitor node children =
    List.concat
        [ [ toVisitor (Enter node) ]
        , children
        , [ toVisitor (Exit node) ]
        ]


moduleVisitor : Visitor context
moduleVisitor rule context =
    rule.moduleEndFn context


expressionVisitor : Direction Expression -> Visitor context
expressionVisitor node rule context =
    rule.expressionFn context node


statementVisitor : Direction Statement -> Visitor context
statementVisitor node rule context =
    rule.statementFn context node


expressionToVisitors : Expression -> List (Visitor context)
expressionToVisitors node =
    let
        children =
            case node of
                Application expression1 expression2 ->
                    [ expression1, expression2 ]

                Access expression names ->
                    [ expression ]

                Variable _ ->
                    []

                String _ ->
                    []

                Character _ ->
                    []

                Integer _ ->
                    []

                Float _ ->
                    []

                List elements ->
                    elements

                Record pairs ->
                    List.map Tuple.second pairs

                RecordUpdate name updates ->
                    List.map Tuple.second updates

                BinOp operator left right ->
                    [ operator, left, right ]

                If cond then_ else_ ->
                    [ cond, then_, else_ ]

                Let declarations body ->
                    List.append
                        (List.map Tuple.second declarations)
                        [ body ]

                Case target cases ->
                    List.append
                        [ target ]
                        (List.concatMap (\( a, b ) -> [ a, b ]) cases)

                Lambda names expression ->
                    [ expression ]

        childrenVisitors =
            List.concatMap expressionToVisitors children
    in
        createExitAndEnterWithChildren expressionVisitor node childrenVisitors


typeToVisitors : Type -> List (Visitor context)
typeToVisitors node =
    []


statementChildrenToVisitors : List Expression -> List Type -> List (Visitor context)
statementChildrenToVisitors expressions types =
    List.concat
        [ List.concatMap expressionToVisitors expressions
        , List.concatMap typeToVisitors types
        ]


statementToVisitors : Statement -> List (Visitor context)
statementToVisitors node =
    let
        childrenVisitors =
            case node of
                FunctionTypeDeclaration name application ->
                    statementChildrenToVisitors [] [ application ]

                FunctionDeclaration name params body ->
                    statementChildrenToVisitors [ body ] []

                ModuleDeclaration name exportSet ->
                    statementChildrenToVisitors [] []

                _ ->
                    []
    in
        createExitAndEnterWithChildren statementVisitor node childrenVisitors


transformStatementsIntoVisitors : List Statement -> List (Visitor context)
transformStatementsIntoVisitors statements =
    statements
        |> List.concatMap statementToVisitors
        |> (\allVisitors -> List.append allVisitors [ moduleVisitor ])
