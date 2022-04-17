module NoRecursiveUpdate exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when the `update` function calls itself.

This is often done in order to have one message (A) trigger (all or some of) the same
model updates and commands as another message (B).

    update msg model =
        case msg of
            Foo ->
                { model | foo = True }

            Bar ->
                update Foo { model | bar = True }

This is advised against, because if the way that message B is handled changes,
that will implicitly change how message A is handled in ways that may not have
been foreseen.

A better solution is to move the common handling into a different function and
have it called in the handling of both messages.

    update msg model =
        case msg of
            Foo ->
                commonOperationOnModel model

            Bar ->
                commonOperationOnModel { model | bar = True }

    commonOperationOnModel model =
        { model | foo = True }

Calls to other modules' `update` function are allowed.

To add the rule to your configuration:

    config =
        [ NoRecursiveUpdate.rule
        ]


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-the-elm-architecture/example --rules NoRecursiveUpdate
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoRecursiveUpdate" { isInUpdateFunction = False }
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { isInUpdateFunction : Bool
    }


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node _ =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , { isInUpdateFunction =
                    (function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
                    )
                        == "update"
              }
            )

        _ ->
            ( [], { isInUpdateFunction = False } )


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    if context.isInUpdateFunction then
        case Node.value node of
            Expression.FunctionOrValue [] "update" ->
                ( [ Rule.error
                        { message = "`update` shouldn't call itself"
                        , details = [ "If you wish to have the same behavior for different messages, move that behavior into a new function and have it called in the handling of both messages." ]
                        }
                        (Node.range node)
                  ]
                , context
                )

            _ ->
                ( [], context )

    else
        ( [], context )
