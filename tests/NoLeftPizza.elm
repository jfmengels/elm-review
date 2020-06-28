module NoLeftPizza exposing (rule, Strictness(..))

{-|

@docs rule, Strictness

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import NoLeftPizzaUtil
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import Review.Rule3 as Rule3


{-| Specify how strict the rule should be.

Specifying `Any` means that _any_ use of `<|` will be flagged, whereas
`Redundant` limits it to cases where `<|` can be removed - without adding any
parenthesis - without changing the semantics.

-}
type Strictness
    = Any
    | Redundant


{-| Forbids using the left pizza operator (<|) in infix position.

Expressions like `foo <| "hello" ++ world` will be flagged, and a fix will be
proposed to write the expression to `foo ("hello" ++ world)`.

To use this rule, add it to your `elm-review` config like so:

    import NoLeftPizza
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoLeftPizza.rule NoLeftPizza.Any
        ]

The above configuration results in absolutely any use of `<|` being flagged. If
you'd prefer only flagging redundant usage (such as `foo <| bar`), pass
`NoLeftPizza.Redundant` as the configuration option.

If you would prefer to keep writing tests in the more "traditional" style which
uses `<|`, you can disable the rule for `tests/` like so:

    import NoLeftPizza
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoLeftPizza.rule NoLeftPizza.Any
            |> Rule.ignoreErrorsForDirectories
                [ -- Test functions are traditionally built up using a left pizza.
                  -- While we don't want them in our regular code, let's allow them
                  -- just for tests.
                  "tests/"
                ]
        ]

Or pass `NoLeftPizza.Redundant` which will only apply to redundant usage:

    import NoLeftPizza
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoLeftPizza.rule NoLeftPizza.Redundant
        ]

-}
rule : Strictness -> Rule
rule strictness =
    Rule.newModuleRuleSchema "NoLeftPizza" strictness
        |> Rule.withSimpleExpressionVisitor (expressionVisitor strictness)
        |> Rule.fromModuleRuleSchema


expressionVisitor : Strictness -> Node Expression -> List (Error {})
expressionVisitor strictness node =
    case Node.value node of
        Expression.OperatorApplication "<|" _ left right ->
            case makeError strictness node left right of
                Just error ->
                    [ error ]

                Nothing ->
                    []

        _ ->
            []


makeError : Strictness -> Node Expression -> Node Expression -> Node Expression -> Maybe (Error {})
makeError strictness node left right =
    case ( strictness, isSimpleExpression right ) of
        ( Any, False ) ->
            Just (produceError strictness node left (parenthesized right))

        ( Redundant, False ) ->
            Nothing

        _ ->
            Just (produceError strictness node left right)


produceError : Strictness -> Node Expression -> Node Expression -> Node Expression -> Error {}
produceError strictness node left right =
    Rule.errorWithFix (infoFor strictness)
        (Node.range node)
        [ Fix.replaceRangeBy (Node.range node)
            (NoLeftPizzaUtil.expressionToString (Node.range node)
                (Expression.Application [ left, right ])
            )
        ]


infoFor : Strictness -> { message : String, details : List String }
infoFor strictness =
    case strictness of
        Any ->
            { message = "That's a left pizza (<|) operator application there!"
            , details =
                [ "We prefer using either parenthesized function application like `Html.text (context.translate Foo.Bar)` or right pizza's like `foo |> bar`."
                , "The proposed fix rewrites the expression to a simple parenthesized expression, however, this may not always be what you want. Use your best judgement!"
                ]
            }

        Redundant ->
            { message = "Redundant left pizza (<|) operator application"
            , details =
                [ "This left pizza operator can be removed without any further changes, without changing the semantics of your code."
                , "Using `<|` like this adds visual noise to code that can make it harder to read."
                ]
            }


parenthesized : Node Expression -> Node Expression
parenthesized ((Node.Node range _) as node) =
    Node.Node range (Expression.ParenthesizedExpression node)


isSimpleExpression : Node Expression -> Bool
isSimpleExpression (Node.Node _ expr) =
    case expr of
        Expression.Application _ ->
            False

        Expression.OperatorApplication _ _ _ _ ->
            False

        Expression.IfBlock _ _ _ ->
            False

        Expression.Operator _ ->
            False

        Expression.LetExpression _ ->
            False

        Expression.CaseExpression _ ->
            False

        Expression.LambdaExpression _ ->
            False

        Expression.GLSLExpression _ ->
            False

        _ ->
            True
