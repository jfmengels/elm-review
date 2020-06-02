module NoLeftPizza exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import NoLeftPizzaUtil
import Review.Fix as Fix
import Review.Rule as Rule exposing (Direction, Error, Rule)


{-| Forbids using the left pizza operator (<|) in infix position.

Expressions like `foo <| "hello" ++ world` will be flagged, and a fix will be
proposed to write the expression to `foo ("hello" ++ world)`.

To use this rule, add it to your `elm-review` config like so:

    module ReviewConfig exposing (config)

    import NoLeftPizza
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoLeftPizza.rule
        ]

If you would prefer to keep writing tests in the more "traditional" style which
uses `<|`, you can disable the rule for `tests/` like so:

    module ReviewConfig exposing (config)

    import NoLeftPizza
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoLeftPizza.rule
            |> Rule.ignoreErrorsForDirectories
                [ -- Test functions are traditionally built up using a left pizza.
                  -- While we don't want them in our regular code, let's allow them
                  -- just for tests.
                  "tests/"
                ]
        ]

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoLeftPizza" emptyContext
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { pizzaExpression : Maybe PizzaExpression
    }


type alias PizzaExpression =
    { node : Node Expression
    , left : Node Expression
    , right : Node Expression
    }


emptyContext : Context
emptyContext =
    { pizzaExpression = Nothing
    }


declarationVisitor : Node Declaration -> Direction -> Context -> ( List (Error {}), Context )
declarationVisitor _ direction context =
    case direction of
        Rule.OnEnter ->
            ( [], emptyContext )

        Rule.OnExit ->
            ( buildErrors context, emptyContext )


expressionVisitor : Node Expression -> Direction -> Context -> ( List (Error {}), Context )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnExit, Expression.OperatorApplication "<|" _ left right ) ->
            ( buildErrors context
            , { emptyContext
                | pizzaExpression =
                    Just
                        { left = left
                        , right = right
                        , node = node
                        }
              }
            )

        ( Rule.OnExit, Expression.OperatorApplication op dir left right ) ->
            case context.pizzaExpression of
                Just pizza ->
                    if Node.value left == Node.value pizza.node then
                        ( [], extendPizza op dir right node pizza )

                    else
                        ( buildErrors context, emptyContext )

                _ ->
                    ( [], context )

        ( _, _ ) ->
            ( [], context )


extendPizza :
    String
    -> InfixDirection
    -> Node Expression
    -> Node Expression
    -> PizzaExpression
    -> Context
extendPizza op dir right current pizza =
    let
        rightNode =
            Node.Node
                (Range.combine [ Node.range pizza.right, Node.range right ])
                (Expression.OperatorApplication op dir pizza.right right)

        newPizza =
            { left = pizza.left
            , right = rightNode
            , node = current
            }
    in
    { pizzaExpression = Just newPizza }


buildErrors : Context -> List (Error {})
buildErrors { pizzaExpression } =
    pizzaExpression
        |> Maybe.map (makeError >> List.singleton)
        |> Maybe.withDefault []


makeError : PizzaExpression -> Error {}
makeError pizza =
    Rule.errorWithFix
        { message = "That's a left pizza (<|) operator application there!"
        , details =
            [ "We prefer using either parenthesized function application like `Html.text (context.translate Foo.Bar)` or right pizza's like `foo |> bar`."
            , "The proposed fix rewrites the expression to a simple parenthesized expression, however, this may not always be what you want. Use your best judgement!"
            ]
        }
        (Node.range pizza.node)
        [ Fix.replaceRangeBy (Node.range pizza.node)
            (NoLeftPizzaUtil.expressionToString (Node.range pizza.node)
                (Expression.Application
                    [ pizza.left
                    , parenthesize pizza.right
                    ]
                )
            )
        ]


parenthesize : Node Expression -> Node Expression
parenthesize ((Node.Node range value) as node) =
    case value of
        Expression.UnitExpr ->
            node

        Expression.FunctionOrValue _ _ ->
            node

        Expression.Operator _ ->
            node

        Expression.Integer _ ->
            node

        Expression.Hex _ ->
            node

        Expression.Floatable _ ->
            node

        Expression.Literal _ ->
            node

        Expression.CharLiteral _ ->
            node

        Expression.TupledExpression _ ->
            node

        Expression.ParenthesizedExpression _ ->
            node

        Expression.RecordExpr _ ->
            node

        Expression.ListExpr _ ->
            node

        Expression.RecordAccess _ _ ->
            node

        Expression.RecordAccessFunction _ ->
            node

        Expression.RecordUpdateExpression _ _ ->
            node

        _ ->
            Node.Node range (Expression.ParenthesizedExpression node)
