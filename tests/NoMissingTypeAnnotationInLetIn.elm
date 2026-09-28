module NoMissingTypeAnnotationInLetIn exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import NoMissingTypeAnnotation.Context as Context exposing (Context)
import NoMissingTypeAnnotation.Print as Print
import Review.Fix as Fix
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoMissingTypeAnnotationInLetIn" Context.init
        |> Rule.withImportVisitor Context.importVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor (Node _ expression) context =
    case expression of
        Expression.LetExpression { declarations } ->
            List.foldl
                (\declaration acc ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            case function.signature of
                                Just _ ->
                                    acc

                                Nothing ->
                                    reportFunctionWithoutSignature function acc

                        _ ->
                            acc
                )
                ( [], context )
                declarations

        _ ->
            ( [], context )


reportFunctionWithoutSignature : Expression.Function -> ( List (Error {}), Context ) -> ( List (Error {}), Context )
reportFunctionWithoutSignature function ( errors, context ) =
    let
        (Node range name) =
            function.declaration
                |> Node.value
                |> .name

        fix : List Fix.Edit
        fix =
            case context.getType (Node.range function.declaration) of
                Ok type_ ->
                    Print.insertTypeAnnotation
                        context
                        (Node.range function.declaration).start
                        name
                        type_

                Err _ ->
                    []
    in
    ( Rule.errorWithFix
        { message = "Missing type annotation for `" ++ name ++ "`"
        , details =
            [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
            ]
        }
        range
        fix
        :: errors
    , context
    )
