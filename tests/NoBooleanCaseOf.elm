module NoBooleanCaseOf exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when pattern matching is used for a boolean value.

The idiomatic way to check for a condition is to use an `if` expression.
Read more about it at: <https://guide.elm-lang.org/core_language.html#if-expressions>

    config =
        [ NoBooleanCaseOf.rule
        ]

This won't report pattern matching when a boolean is part of the evaluated value.


## Fail

    _ =
        case bool of
            True ->
                expression

            False ->
                otherExpression


## Success

    _ =
        if bool then
            expression

        else
            otherExpression

    _ =
        case ( bool, somethingElse ) of
            ( True, SomeThingElse ) ->
                expression

            _ ->
                otherExpression


# When (not) to use this rule

You should not use this rule if you do not care about how your boolean values are
evaluated.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplification/example --rules NoBooleanCaseOf
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoBooleanCaseOf" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.CaseExpression { expression, cases } ->
            if List.any (Tuple.first >> isBoolConstructor) cases then
                [ error expression ]

            else
                []

        _ ->
            []


error : Node a -> Error {}
error node =
    Rule.error
        { message = "Replace `case..of` by an `if` condition"
        , details =
            [ "The idiomatic way to check for a condition is to use an `if` expression."
            , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
            ]
        }
        (Node.range node)


isBoolConstructor : Node Pattern -> Bool
isBoolConstructor node =
    case Node.value node of
        Pattern.NamedPattern { moduleName, name } _ ->
            (name == "True" || name == "False")
                && (moduleName == [] || moduleName == [ "Basics" ])

        _ ->
            False
