module NoExtraBooleanComparison exposing (rule)

{-| Forbid the use of boolean comparisons that can be simplified.


# Rule

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of boolean comparisons that can be simplified.

    config =
        [ NoExtraBooleanComparison.rule
        ]


## Fail

    if someBooleanValue == True then
        a

    else
        b

    if someBooleanValue == False then
        a

    else
        b


## Success

    if someBooleanValue then
        a

    else
        b

    if not someBooleanValue then
        a

    else
        b


# When not to use this rule

You should not use this rule if you

  - prefer the explicitness of expressions like `value == True`
  - dislike the use of the `not` function
  - do not see the value in simplifying boolean comparisons

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoExtraBooleanComparison" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


error : Node a -> String -> String -> Error {}
error node operator comparedValue =
    Rule.error
        { message = "Unnecessary comparison with `" ++ comparedValue ++ "`"
        , details = [ "You can simplify this expression by removing the `" ++ operator ++ "` operator and the value `" ++ comparedValue ++ "`." ]
        }
        (Node.range node)


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.OperatorApplication operator _ left right ->
            if isEqualityOperator operator then
                List.filterMap isTrueOrFalse [ left, right ]
                    |> List.map (error node operator)

            else
                []

        _ ->
            []


isEqualityOperator : String -> Bool
isEqualityOperator operator =
    operator == "==" || operator == "/="


isTrueOrFalse : Node Expression -> Maybe String
isTrueOrFalse node =
    case Node.value node of
        FunctionOrValue [] functionOrValue ->
            if functionOrValue == "True" || functionOrValue == "False" then
                Just functionOrValue

            else
                Nothing

        _ ->
            Nothing
