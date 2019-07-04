module Lint.Rule.DefaultPatternPosition exposing (rule, PatternPosition(..))

{-| Enforce the default pattern to always appear first or last.


## Fail

    import Lint.Rules.DefaultPatternPosition as DefaultPatternPosition

    config =
        [ ( Critical
          , DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeLast
          )
        ]

    a =
        case value of
            -- Error: "Expected default pattern to appear last in the list of patterns"
            _ ->
                result

            Foo ->
                bar

    ----------------------
    config =
        [ ( Critical
          , DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeFirst
          )
        ]

    a =
        case value of
            Foo ->
                bar

            -- Error: Expected default pattern to appear first in the list of patterns
            _ ->
                result


## Success

    config =
        [ ( Critical
          , DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeLast
          )
        ]

    a =
        case value of
            Foo ->
                bar

            _ ->
                result
    a =
        case value of
            -- No default pattern
            Foo ->
                bar

            Bar ->
                foo

    -- --------------------
    config =
        [ ( Critical
          , DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeFirst
          )
        ]

    a =
        case value of
            _ ->
                result

            Foo ->
                bar


# Rule and configuration

@docs rule, PatternPosition

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Lint.Rule as Rule exposing (Error, Rule)


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
    Rule.error message (Node.range node)


isDefaultPattern : Pattern -> Bool
isDefaultPattern pattern =
    case pattern of
        AllPattern ->
            True

        _ ->
            False


findDefaultPattern : List ( Node Pattern, Node Expression ) -> Maybe ( Int, Node Pattern )
findDefaultPattern patterns =
    findWithIndex
        (Node.value >> isDefaultPattern)
        (List.map Tuple.first patterns)


findWithIndex : (a -> Bool) -> List a -> Maybe ( Int, a )
findWithIndex isMatch list =
    case list of
        [] ->
            Nothing

        elem :: rest ->
            if isMatch elem then
                Just ( 0, elem )

            else
                findWithIndex isMatch rest
                    |> Maybe.map (\( index, elem_ ) -> ( index + 1, elem_ ))


expressionVisitor : PatternPosition -> Node Expression -> List Error
expressionVisitor patternPosition node =
    case Node.value node of
        CaseExpression { cases } ->
            case findDefaultPattern cases of
                Nothing ->
                    []

                Just ( index, patternNode ) ->
                    case patternPosition of
                        ShouldBeFirst ->
                            if index /= 0 then
                                [ error patternNode "Expected default pattern to appear first in the list of patterns" ]

                            else
                                []

                        ShouldBeLast ->
                            if index /= List.length cases - 1 then
                                [ error patternNode "Expected default pattern to appear last in the list of patterns" ]

                            else
                                []

        _ ->
            []
