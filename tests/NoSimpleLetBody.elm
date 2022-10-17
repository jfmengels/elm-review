module NoSimpleLetBody exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)


{-| Reports when a let expression's body is a simple reference to a value declared in the let expression.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoSimpleLetBody.rule
        ]

The reasoning is that it is not necessary to assign a name to the result of a let expression,
since they are redundant with the value or function containing the expression.

If it feels necessary to give a name anyway because it helps clarify the context, then it might be a sign that the computation of that value should be extracted to a function.

Let expressions will be reported regardless of whether they're at the root of a function or deeply nested.


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


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoSimpleLetBody
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoSimpleLetBody" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor node =
    case Node.value node of
        Expression.LetExpression letBlock ->
            visitLetExpression (Node.range node) letBlock

        _ ->
            []


visitLetExpression : Range -> Expression.LetBlock -> List (Rule.Error {})
visitLetExpression nodeRange { declarations, expression } =
    case Node.value expression of
        Expression.FunctionOrValue [] name ->
            let
                declarationData :
                    { previousEnd : Maybe Location
                    , lastEnd : Maybe Location
                    , last : Maybe { name : String, expressionRange : Range }
                    , foundDeclaredWithName : Bool
                    }
                declarationData =
                    getDeclarationsData name declarations
            in
            if declarationData.foundDeclaredWithName then
                [ Rule.errorWithFix
                    { message = "The referenced value should be inlined."
                    , details =
                        [ "The name of the value is redundant with the surrounding expression."
                        , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                        ]
                    }
                    (Node.range expression)
                    (fix nodeRange name declarationData)
                ]

            else
                []

        _ ->
            []


getDeclarationsData :
    String
    -> List (Node Expression.LetDeclaration)
    ->
        { previousEnd : Maybe Location
        , lastEnd : Maybe Location
        , last : Maybe { name : String, expressionRange : Range }
        , foundDeclaredWithName : Bool
        }
getDeclarationsData name declarations =
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
                                , expressionRange = Node.range functionDeclaration.expression
                                }

                        else
                            Nothing
                    , foundDeclaredWithName = foundDeclaredWithName || Node.value functionDeclaration.name == name
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
        , foundDeclaredWithName = False
        }
        declarations


fix :
    Range
    -> String
    ->
        { r
            | previousEnd : Maybe Location
            , last : Maybe { name : String, expressionRange : Range }
        }
    -> List Fix
fix nodeRange name declarationData =
    case declarationData.last of
        Just last ->
            if last.name == name then
                case declarationData.previousEnd of
                    Nothing ->
                        -- It's the only element in the destructuring, we should remove move of the let expression
                        [ Fix.removeRange { start = nodeRange.start, end = last.expressionRange.start }
                        , Fix.removeRange { start = last.expressionRange.end, end = nodeRange.end }
                        ]

                    Just previousEnd ->
                        -- There are other elements in the let body that we need to keep
                        let
                            indentation : String
                            indentation =
                                String.repeat (nodeRange.start.column - 1) " "
                        in
                        [ Fix.replaceRangeBy { start = previousEnd, end = last.expressionRange.start } ("\n" ++ indentation ++ "in\n" ++ indentation)
                        , Fix.removeRange { start = last.expressionRange.end, end = nodeRange.end }
                        ]

            else
                []

        Nothing ->
            []
