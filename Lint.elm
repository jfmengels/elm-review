module Lint exposing (lint, LintRule, Error)

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


visitExpression : Expression -> Visitor context
visitExpression node rule context =
    rule.expressionFn context node


visitType : Type -> Visitor context
visitType node rule context =
    rule.typeFn context node


visitStatement : Statement -> Visitor context
visitStatement node rule context =
    rule.statementFn context node


transformExpression : Expression -> List (Visitor context)
transformExpression node =
    let
        visitAndTransformChildren children =
            List.concat
                [ [ visitExpression node ]
                , List.concatMap transformExpression children
                ]
    in
        case node of
            Application expression1 expression2 ->
                visitAndTransformChildren [ expression1, expression2 ]

            Access expression names ->
                visitAndTransformChildren [ expression ]

            Variable _ ->
                [ visitExpression node ]

            _ ->
                []


transformType : Type -> List (Visitor context)
transformType node =
    []


transformStatement : Statement -> List (Visitor context)
transformStatement node =
    let
        visitAndTransformChildren expresssions types =
            List.concat
                [ [ visitStatement node ]
                , List.concatMap transformExpression expresssions
                , List.concatMap transformType types
                ]
    in
        case node of
            FunctionTypeDeclaration name application ->
                visitAndTransformChildren [] [ application ]

            FunctionDeclaration name params body ->
                visitAndTransformChildren [ body ] []

            _ ->
                []


transform : List Statement -> List (Visitor context)
transform statements =
    List.concatMap transformStatement statements


lint : List Statement -> LintRule context -> List Error
lint statements rule =
    let
        validators =
            transform statements

        ( errors, _ ) =
            List.foldl
                (\visitor ( errors, ctx ) ->
                    let
                        ( errors_, ctx_ ) =
                            visitor rule ctx
                    in
                        ( errors ++ errors_, ctx_ )
                )
                ( [], rule.context )
                validators
    in
        errors
