module Lint
    exposing
        ( lint
        , LintRule
        , Error
        , doNothing
        )

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Node exposing (..)


type alias Error =
    String


type alias LintImplementation nodeType context =
    context -> Direction nodeType -> ( List Error, context )


type alias LintRule context =
    { statementFn : LintImplementation Statement context
    , typeFn : LintImplementation Type context
    , expressionFn : LintImplementation Expression context
    , context : context
    }


type alias Visitor context =
    LintRule context -> context -> ( List Error, context )


doNothing : LintImplementation a context
doNothing ctx _ =
    ( [], ctx )


createExitAndEnterWithChildren : (Direction nodeType -> Visitor context) -> nodeType -> List (Visitor context) -> List (Visitor context)
createExitAndEnterWithChildren toVisitor node children =
    List.concat
        [ [ toVisitor (Enter node) ]
        , children
        , [ toVisitor (Exit node) ]
        ]


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

                _ ->
                    []

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


visitAndAccumulate : LintRule context -> Visitor context -> ( List Error, context ) -> ( List Error, context )
visitAndAccumulate rule visitor ( errors, ctx ) =
    visitor rule ctx
        |> Tuple.mapFirst (\errors_ -> errors ++ errors_)


lintWithVisitors : List (Visitor context) -> LintRule context -> List Error
lintWithVisitors visitors rule =
    visitors
        |> List.foldl (visitAndAccumulate rule) ( [], rule.context )
        |> Tuple.first


lint : List Statement -> LintRule context -> List Error
lint statements =
    List.concatMap statementToVisitors statements
        |> lintWithVisitors
