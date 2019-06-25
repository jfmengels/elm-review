module Lint.Rule.DefaultPatternPosition exposing (rule, PatternPosition(..))

{-|

@docs rule, PatternPosition


# Fail

    import Lint.Rules.DefaultPatternPosition as DefaultPatternPosition

    config =
        [ ( Critical, DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeLast )
        ]

    case value of
      -- LintError, this pattern should appear last
      _ -> result
      Foo -> bar

    -- --------------------


    config =
        [ ( Critical, DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeFirst )
        ]

    case value of
      Foo -> bar
      -- LintError, this pattern should appear first
      _ -> result


# Success

    config =
        [ ( Critical, DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeLast )
        ]

    case value of
      Foo -> bar
      _ -> result

    case value of
      -- No default pattern
      Foo -> bar
      Bar -> foo

    -- --------------------

    config =
        [ ( Critical, DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeFirst )
        ]

    case value of
      _ -> result
      Foo -> bar

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Lint.Error as Error exposing (Error)
import Lint.Rule as Rule exposing (Rule)
import List.Extra exposing (findIndex)
import Regex


{-| Configures whether the default pattern should appear first or last.
-}
type PatternPosition
    = ShouldBeFirst
    | ShouldBeLast


{-| Enforce the default pattern to always appear first or last.
-}
rule : PatternPosition -> Rule
rule patternPosition =
    Rule.newSchema "DefaultPatternPosition"
        |> Rule.withSimpleExpressionVisitor (expressionVisitor patternPosition)
        |> Rule.fromSchema


error : Node a -> String -> Error
error node message =
    Error.create message (Node.range node)


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


expressionVisitor : PatternPosition -> Node Expression -> List Error
expressionVisitor patternPosition node =
    case Node.value node of
        CaseExpression { cases } ->
            case findDefaultPattern cases of
                Nothing ->
                    []

                Just index ->
                    case patternPosition of
                        ShouldBeFirst ->
                            if index /= 0 then
                                [ error node "Expected default pattern to appear first in the list of patterns" ]

                            else
                                []

                        ShouldBeLast ->
                            if index /= List.length cases - 1 then
                                [ error node "Expected default pattern to appear last in the list of patterns" ]

                            else
                                []

        _ ->
            []
