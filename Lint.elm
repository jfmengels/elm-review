module Lint exposing (lint, LintRule, Error, doNothing)

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)


type alias Error =
    String


type alias LintImplementation nodeType context =
    context -> nodeType -> ( List Error, context )


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


expressionVisitor : Expression -> Visitor context
expressionVisitor node rule context =
    rule.expressionFn context node


typeVisitor : Type -> Visitor context
typeVisitor node rule context =
    rule.typeFn context node


statementVisitor : Statement -> Visitor context
statementVisitor node rule context =
    rule.statementFn context node


expressionToVisitors : Expression -> List (Visitor context)
expressionToVisitors node =
    let
        visitAndTransformChildren children =
            List.concat
                [ [ expressionVisitor node ]
                , List.concatMap expressionToVisitors children
                ]
    in
        case node of
            Application expression1 expression2 ->
                visitAndTransformChildren [ expression1, expression2 ]

            Access expression names ->
                visitAndTransformChildren [ expression ]

            Variable _ ->
                [ expressionVisitor node ]

            _ ->
                []


typeToVisitors : Type -> List (Visitor context)
typeToVisitors node =
    []


statementToVisitors : Statement -> List (Visitor context)
statementToVisitors node =
    let
        visitAndTransformChildren expressions types =
            List.concat
                [ [ statementVisitor node ]
                , List.concatMap expressionToVisitors expressions
                , List.concatMap typeToVisitors types
                ]
    in
        case node of
            FunctionTypeDeclaration name application ->
                visitAndTransformChildren [] [ application ]

            FunctionDeclaration name params body ->
                visitAndTransformChildren [ body ] []

            ModuleDeclaration name exportSet ->
                visitAndTransformChildren [] []

            _ ->
                []


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
