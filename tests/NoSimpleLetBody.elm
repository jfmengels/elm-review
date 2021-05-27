module NoSimpleLetBody exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports when a let expression's body is a simple reference to a value declared in the let expression.

    config =
        [ NoSimpleLetBody.rule
        ]

The reasoning behind this rule is that it is not necessary to assign a name (and type annotation) to the result of a let expression,
since they are redundant with the value or function containing the expression.

If it feels necessary to give a name anyway because it helps clarify the context, then it might be a sign that the computation of that value should be extracted to a function.


## Fail

    a =
        let
            b =
                1

            c =
                b + 1
        in
        c


## Success

Anything that is not simply a reference to a value declared in the let expression is okay.

    a =
        let
            b =
                1
        in
        b + 1

The rule will not report when the referenced value was destructured in the let expression.

    first tuple =
        let
            ( value, _ ) =
                tuple
        in
        value


## When (not) to enable this rule

This rule resolves a minor style issue, and may not be worth enforcing depending on how strongly you feel about this issue.

I recommend having your full team's buy-in before enabling this rule to reduce the amount of frustration it will trigger when
it causes the tests or even the CI pipeline to fail. This rule does not (currently) provide an automatic fix which may increase
your team's frustration if they disagree with or don't care about this rule.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoSimpleLetBody
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoSimpleLetBody" initialContext
        |> Rule.withExpressionEnterVisitor (\node context -> ( expressionVisitor node context, context ))
        |> Rule.fromModuleRuleSchema


type alias Context =
    { getStringAtRange : Range -> String
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\getStringAtRange () ->
            { getStringAtRange = getStringAtRange
            }
        )
        |> Rule.withSourceCodeExtractor


expressionVisitor : Node Expression -> Context -> List (Rule.Error {})
expressionVisitor node { getStringAtRange } =
    case Node.value node of
        Expression.LetExpression { declarations, expression } ->
            case Node.value expression of
                Expression.FunctionOrValue [] name ->
                    let
                        declarationData :
                            { previousEnd : Maybe Location
                            , lastEnd : Maybe Location
                            , last : Maybe { name : String, declarationRange : Range, expressionRange : Range }
                            , foundDeclaredWithName : Maybe { declarationRange : Range, expressionRange : Range, takesArguments : Bool }
                            }
                        declarationData =
                            List.foldl
                                (\declaration { lastEnd, foundDeclaredWithName } ->
                                    case Node.value declaration of
                                        Expression.LetFunction function ->
                                            let
                                                functionDeclaration : Expression.FunctionImplementation
                                                functionDeclaration =
                                                    Node.value function.declaration
                                            in
                                            { previousEnd = lastEnd
                                            , lastEnd = Just (Node.range functionDeclaration.expression).end
                                            , last =
                                                if List.isEmpty functionDeclaration.arguments then
                                                    Just
                                                        { name = Node.value functionDeclaration.name
                                                        , declarationRange = Node.range declaration
                                                        , expressionRange = Node.range functionDeclaration.expression
                                                        }

                                                else
                                                    Nothing
                                            , foundDeclaredWithName =
                                                if Node.value functionDeclaration.name == name then
                                                    Just
                                                        { declarationRange = Node.range declaration
                                                        , expressionRange = Node.range functionDeclaration.expression
                                                        , takesArguments = not (List.isEmpty functionDeclaration.arguments)
                                                        }

                                                else
                                                    foundDeclaredWithName
                                            }

                                        Expression.LetDestructuring _ _ ->
                                            { previousEnd = lastEnd
                                            , lastEnd = Just (Node.range declaration).end
                                            , last = Nothing
                                            , foundDeclaredWithName = foundDeclaredWithName
                                            }
                                )
                                { previousEnd = Nothing
                                , lastEnd = Nothing
                                , last = Nothing
                                , foundDeclaredWithName = Nothing
                                }
                                declarations
                    in
                    case declarationData.foundDeclaredWithName of
                        Just { declarationRange, expressionRange, takesArguments } ->
                            [ Rule.errorWithFix
                                { message = "The referenced value should be inlined."
                                , details =
                                    [ "The name of the value is redundant with the surrounding expression."
                                    , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                    ]
                                }
                                (Node.range expression)
                                (case declarationData.last of
                                    Just last ->
                                        if not takesArguments then
                                            if last.name == name then
                                                case declarationData.previousEnd of
                                                    Nothing ->
                                                        -- It's the only element in the destructuring, we should remove move of the let expression
                                                        [ Fix.removeRange { start = (Node.range node).start, end = last.expressionRange.start }
                                                        , Fix.removeRange { start = last.expressionRange.end, end = (Node.range node).end }
                                                        ]

                                                    Just previousEnd ->
                                                        -- There are other elements in the let body that we need to keep
                                                        let
                                                            indentation : String
                                                            indentation =
                                                                String.repeat ((Node.range node).start.column - 1) " "
                                                        in
                                                        [ Fix.replaceRangeBy { start = previousEnd, end = last.expressionRange.start } ("\n" ++ indentation ++ "in\n" ++ indentation)
                                                        , Fix.removeRange { start = last.expressionRange.end, end = (Node.range node).end }
                                                        ]

                                            else
                                                [ Fix.removeRange declarationRange
                                                , Fix.replaceRangeBy (Node.range expression) (getStringAtRange expressionRange)
                                                ]

                                        else
                                            []

                                    Nothing ->
                                        if not takesArguments then
                                            [ Fix.removeRange declarationRange
                                            , Fix.replaceRangeBy (Node.range expression) (getStringAtRange expressionRange)
                                            ]

                                        else
                                            []
                                )
                            ]

                        Nothing ->
                            []

                _ ->
                    []

        _ ->
            []
