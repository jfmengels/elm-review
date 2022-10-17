module NoNegationInIfCondition exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports when negations are used in

    config =
        [ NoNegationInIfCondition.rule
        ]


## Fail

    a =
        if not condition then
            1

        else
            2


## Success

    a =
        if condition then
            1

        else
            2


## When (not) to enable this rule

This rule is not meant to be published. It is primarily meant to be a test rule for `withSourceCodeExtractor` and
an example for its usage.

I don't think it's a good rule to enforce, and I personally like to put the "happy" path/branch first,
instead of the non-negated one. As such, I have left this rule incomplete meaning there are more cases that should be
handled for this rule to do what it is meant to do.

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoNegationInIfCondition" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { extractSourceCode : Range -> String
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSourceCode () -> { extractSourceCode = extractSourceCode })
        |> Rule.withSourceCodeExtractor


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.IfBlock condition thenBranch elseBranch ->
            case Node.value condition of
                Expression.Application ((Node notRange (Expression.FunctionOrValue [] "not")) :: _) ->
                    if isIfExpr elseBranch then
                        ( [], context )

                    else
                        let
                            elseKeywordString : String
                            elseKeywordString =
                                context.extractSourceCode { start = (Node.range thenBranch).end, end = (Node.range elseBranch).start }
                        in
                        ( [ Rule.errorWithFix
                                { message = "Don't use if expressions with negated conditions"
                                , details = [ "We at fruits.com think that if expressions are more readable when the condition is not negated." ]
                                }
                                notRange
                                [ Fix.removeRange notRange
                                , Fix.removeRange { start = (Node.range thenBranch).start, end = (Node.range elseBranch).start }
                                , Fix.insertAt
                                    (Node.range elseBranch).end
                                    (elseKeywordString ++ context.extractSourceCode (Node.range thenBranch))
                                ]
                          ]
                        , context
                        )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )


isIfExpr : Node Expression -> Bool
isIfExpr node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            isIfExpr expr

        Expression.IfBlock _ _ _ ->
            True

        _ ->
            False
