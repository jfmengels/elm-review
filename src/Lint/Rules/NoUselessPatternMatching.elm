module Lint.Rules.NoUselessPatternMatching exposing (rule)

{-|
@docs rule

# Fail

    -- Useless pattern matching
    case value of
        Foo -> 1
        Bar -> 1
        _ -> 1

    -- Useless pattern `Bar`, it's the same as the default pattern
    case value of
        Foo -> 2
        Bar -> 1
        _ -> 1

# Success

    case value of
        Foo -> 1
        Bar -> 2
        _ -> 3

    case value of
        Foo n -> n
        Bar n -> n
-}

import Ast.Expression exposing (..)
import Lint exposing (lint, visitExpression, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))
import Regex
import Set exposing (Set)


type alias Context =
    {}


{-| Reports case expressions that can be simplified. Either when all patterns will lead to the same value, or when a
pattern will lead to the same value as the default pattern.

    rules =
        [ NoUselessPatternMatching.rule
        ]
-}
rule : LintRule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context
    }


variableFinder : LintRuleImplementation (Set String)
variableFinder =
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = findVariable
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Set.empty
    }


findVariable : Set String -> Direction Expression -> ( List LintError, Set String )
findVariable foundVariables node =
    case node of
        Enter (Variable a) ->
            ( [], Set.insert (String.join "." a) foundVariables )

        _ ->
            ( [], foundVariables )


baseError : String -> LintError
baseError =
    LintError "NoUselessPatternMatching"


uselessPatternMatchingError : LintError
uselessPatternMatchingError =
    baseError "Useless case expression: It will always evaluate to the same value"


uselessPatternError : LintError
uselessPatternError =
    baseError "Useless patterns: Some will always evaluate to the same value as the default pattern"


subPatternMatchingVariables : Expression -> List String
subPatternMatchingVariables pattern =
    case pattern of
        Variable a ->
            [ String.join "." a ]

        Application object variable ->
            (subPatternMatchingVariables object) ++ (subPatternMatchingVariables variable)

        _ ->
            []


isVariable : String -> Bool
isVariable =
    Regex.contains (Regex.regex "^[_a-z][\\w\\d]*$")


patternMatchingVariables : Expression -> Set String
patternMatchingVariables pattern =
    case pattern of
        Application _ _ ->
            subPatternMatchingVariables pattern
                |> Set.fromList
                |> Set.filter isVariable

        _ ->
            Set.empty


usesIntroducedVariable : ( Expression, Expression, Set String, Set String ) -> Bool
usesIntroducedVariable ( _, _, used, declared ) =
    Set.intersect used declared
        |> (\set -> Set.size set > 0)


patternsAreAllTheSame : List ( Expression, Expression, Set String, Set String ) -> Bool
patternsAreAllTheSame patterns =
    let
        anyUseVariables =
            List.any usesIntroducedVariable patterns

        bodiesAreIdentical =
            patterns
                |> List.map (\( _, body, _, _ ) -> toString body)
                |> Set.fromList
                |> (\set -> Set.size set == 1)
    in
        (not anyUseVariables) && bodiesAreIdentical


defaultPattern : List ( Expression, Expression, Set String, Set String ) -> Maybe ( Expression, Expression, Set String, Set String )
defaultPattern patterns =
    List.foldl
        (\( pattern, body, used, declared ) res ->
            case res of
                Just a ->
                    res

                Nothing ->
                    case pattern of
                        Variable names ->
                            if isVariable (String.join "." names) then
                                Just ( pattern, body, used, declared )
                            else
                                Nothing

                        _ ->
                            Nothing
        )
        Nothing
        patterns


patternBody : ( Expression, Expression, Set String, Set String ) -> Expression
patternBody ( _, body, _, _ ) =
    body


thereAreUselessPatterns : List ( Expression, Expression, Set String, Set String ) -> Bool
thereAreUselessPatterns patterns =
    let
        default =
            defaultPattern patterns

        hasDefault =
            case default of
                Nothing ->
                    False

                Just a ->
                    True

        justDefault =
            Maybe.withDefault ( Integer 1, Integer 1, Set.empty, Set.empty ) default
    in
        hasDefault
            && (List.foldl
                    (\pattern res ->
                        res
                            || ((pattern /= justDefault)
                                    && (patternBody pattern == patternBody justDefault)
                                    && not (usesIntroducedVariable pattern)
                               )
                    )
                    False
                    patterns
               )


expressionFn : Context -> Direction Expression -> ( List LintError, Context )
expressionFn ctx node =
    case node of
        Enter (Case expr patterns) ->
            let
                analyzedPatterns =
                    patterns
                        |> List.map
                            (\( pattern, body ) ->
                                ( pattern, body, Tuple.second <| visitExpression variableFinder body, patternMatchingVariables pattern )
                            )
            in
                if patternsAreAllTheSame analyzedPatterns then
                    ( [ uselessPatternMatchingError ], ctx )
                else if thereAreUselessPatterns analyzedPatterns then
                    ( [ uselessPatternError ], ctx )
                else
                    ( [], ctx )

        _ ->
            ( [], ctx )
