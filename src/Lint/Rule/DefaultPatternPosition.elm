module Lint.Rule.DefaultPatternPosition exposing (rule, Configuration, PatternPosition(..))

{-|

@docs rule, Configuration, PatternPosition


# Fail

    rules =
        [ DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.Last }
        ]

    case value of
      -- LintError, this pattern should appear last
      _ -> result
      Foo -> bar

    -- --------------------

    rules =
        [ DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.First }
        ]

    case value of
      Foo -> bar
      -- LintError, this pattern should appear first
      _ -> result


# Success

    rules =
        [ DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.Last }
        ]

    case value of
      Foo -> bar
      _ -> result

    case value of
      -- No default pattern
      Foo -> bar
      Bar -> foo

    -- --------------------

    rules =
        [ DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.First }
        ]

    case value of
      _ -> result
      Foo -> bar

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Lint exposing (Rule, lint)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error as Error exposing (Error)
import Lint.Rule as Rule
import List.Extra exposing (findIndex)
import Regex


type alias Context =
    ()


{-| Configures whether the default pattern should appear first or last.
-}
type PatternPosition
    = First
    | Last


{-| Configuration for the rule.
-}
type alias Configuration =
    { position : PatternPosition
    }


{-| Enforce the default pattern to always appear first or last.
-}
rule : Configuration -> Rule
rule config =
    Lint.createRule
        "DefaultPatternPosition"
        (lint (implementation config))


implementation : Configuration -> Rule.Implementation Context
implementation configuration =
    Rule.create ()
        |> Rule.withExpressionVisitor (expressionVisitor configuration)


error : Node a -> String -> Error
error node message =
    Error.create message (Node.range node)



{- TODO Share isVariable this in a util file, already defined in NoUselessPatternMatching -}


isVariable : String -> Bool
isVariable =
    Regex.fromString "^[_a-z][\\w\\d]*$"
        |> Maybe.withDefault Regex.never
        |> Regex.contains


isDefaultPattern : Pattern -> Bool
isDefaultPattern pattern =
    case pattern of
        AllPattern ->
            True

        _ ->
            False


findDefaultPattern : List ( Node Pattern, Node Expression ) -> Maybe Int
findDefaultPattern patterns =
    patterns
        |> List.map (Tuple.first >> Node.value)
        |> findIndex isDefaultPattern


expressionVisitor : Configuration -> Context -> Direction -> Node Expression -> ( List Error, Context )
expressionVisitor config ctx direction node =
    case ( direction, Node.value node ) of
        ( Direction.Enter, CaseExpression { cases } ) ->
            case findDefaultPattern cases of
                Nothing ->
                    ( [], ctx )

                Just index ->
                    case config.position of
                        First ->
                            if index /= 0 then
                                ( [ error node "Expected default pattern to appear first in the list of patterns" ], ctx )

                            else
                                ( [], ctx )

                        Last ->
                            if index /= List.length cases - 1 then
                                ( [ error node "Expected default pattern to appear last in the list of patterns" ], ctx )

                            else
                                ( [], ctx )

        _ ->
            ( [], ctx )
