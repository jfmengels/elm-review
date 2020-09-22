module NoMissingTypeAnnotationInLetIn exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports `let in` declarations that do not have a type annotation.

Type annotations help you understand what happens in the code, and it will help the compiler give better error messages.

    config =
        [ NoMissingTypeAnnotationInLetIn.rule
        ]

This rule does not report top-level declarations without a type annotation inside a `let in`.
For that, enable [`NoMissingTypeAnnotation`](./NoMissingTypeAnnotation).


## Fail

    a : number
    a =
        let
            -- Missing annotation
            b =
                2
        in
        b


## Success

    -- Top-level annotation is not necessary, but good to have!
    a : number
    a =
        let
            b : number
            b =
                2
        in
        b


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoMissingTypeAnnotationInLetIn
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingTypeAnnotationInLetIn" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor expression =
    case Node.value expression of
        Expression.LetExpression { declarations } ->
            List.filterMap
                (\declaration ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            reportFunctionWithoutSignature function

                        _ ->
                            Nothing
                )
                declarations

        _ ->
            []


reportFunctionWithoutSignature : Expression.Function -> Maybe (Error {})
reportFunctionWithoutSignature function =
    case function.signature of
        Just _ ->
            Nothing

        Nothing ->
            let
                name : Node String
                name =
                    function.declaration
                        |> Node.value
                        |> .name
            in
            Rule.error
                { message = "Missing type annotation for `" ++ Node.value name ++ "`"
                , details =
                    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
                    ]
                }
                (Node.range name)
                |> Just
