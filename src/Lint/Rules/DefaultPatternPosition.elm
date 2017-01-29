module Lint.Rules.DefaultPatternPosition exposing (rule, PatternPosition(..))

import Ast.Expression exposing (..)
import Lint exposing (doNothing, lint)
import Lint.Types exposing (Direction(..), Error, LintRule)
import List.Extra exposing (findIndex)
import Regex


type alias Context =
    {}


type PatternPosition
    = First
    | Last


type alias Configuration =
    { position : PatternPosition
    }


rule : Configuration -> String -> List Error
rule config input =
    lint input (implementation config)


implementation : Configuration -> LintRule Context
implementation configuration =
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = expressionFn configuration
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context
    }


error : String -> Error
error =
    Error "DefaultPatternPosition"


{-| TODO Share this in a util file, already defined in NoUselessPatternMatching
-}
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


expressionFn : Configuration -> Context -> Direction Expression -> ( List Error, Context )
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
