module NoNegationInIfCondition exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoNegationInIfCondition.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review/example --rules NoNegationInIfCondition
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoNegationInIfCondition" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema
        |> Rule.ignoreErrorsForFiles [ "src/Colors.elm" ]


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
                    ( [ Rule.errorWithFix
                            { message = "Don't use if expressions with negated conditions"
                            , details = [ "REPLACEME" ]
                            }
                            notRange
                            [ Fix.removeRange notRange
                            , Fix.removeRange { start = (Node.range thenBranch).start, end = (Node.range elseBranch).start }
                            , Fix.insertAt
                                (Node.range elseBranch).end
                                (" else " ++ context.extractSourceCode (Node.range thenBranch))
                            ]
                      ]
                    , context
                    )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )
