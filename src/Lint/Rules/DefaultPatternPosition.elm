module Lint.Rules.DefaultPatternPosition exposing (rule, Configuration, PatternPosition(..))

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

import Ast.Expression exposing (..)
import Lint exposing (doNothing, lint)
import Lint.Types exposing (LintRule, Direction(..), LintError, LintRuleImplementation)
import List.Extra exposing (findIndex)
import Regex


type alias Context =
    {}


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
rule : Configuration -> LintRule
rule config input =
    lint input (implementation config)


implementation : Configuration -> LintRuleImplementation Context
implementation configuration =
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = expressionFn configuration
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context
    }


error : String -> LintError
error =
    LintError "DefaultPatternPosition"



{- TODO Share isVariable this in a util file, already defined in NoUselessPatternMatching -}


isVariable : String -> Bool
isVariable =
    Regex.contains (Regex.regex "^[_a-z][\\w\\d]*$")


isDefaultPattern : ( Expression, Expression ) -> Bool
isDefaultPattern pattern =
    case Tuple.first pattern of
        Variable names ->
            if isVariable (String.join "." names) then
                True
            else
                False

        _ ->
            False


findDefaultPattern : List ( Expression, Expression ) -> Maybe Int
findDefaultPattern =
    findIndex isDefaultPattern


expressionFn : Configuration -> Context -> Direction Expression -> ( List LintError, Context )
expressionFn config ctx node =
    case node of
        Enter (Case expr patterns) ->
            case findDefaultPattern patterns of
                Nothing ->
                    ( [], ctx )

                Just index ->
                    case config.position of
                        First ->
                            if index /= 0 then
                                ( [ error "Expected default pattern to appear first in the list of patterns" ], ctx )
                            else
                                ( [], ctx )

                        Last ->
                            if index /= (List.length patterns) - 1 then
                                ( [ error "Expected default pattern to appear last in the list of patterns" ], ctx )
                            else
                                ( [], ctx )

        _ ->
            ( [], ctx )
