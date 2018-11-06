module Lint exposing
    ( lintSource
    , lint, visitExpression
    , parseSource
    )

{-| A linter for Elm.

See Lint.Rules for available rules.
To define the rules you wish to use:

    rules =
        [ Lint.Rules.NoDebug.rule
        , Lint.Rules.NoUnusedVariables
        ]

To run the rules on a source code and get a list of errors:

    lint : String -> List String
    lint source =
        let
            errors =
                List.concatMap (\rule -> rule source) rules
        in
        if List.isEmpty errors then
            [ "No errors." ]

        else
            List.map (\err -> err.rule ++ ": " ++ err.message) errors


# Implementation

@docs lintSource


# Rule creation functions

@docs lint, visitExpression


# Internal

@docs parseSource

-}

import Elm.Parser as Parser
import Elm.Processing exposing (addFile, init, process)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Lint.Types exposing (Direction, LintError, LintImplementation, LintRule, LintRuleImplementation, Severity(..), Visitor)
import Lint.Visitor exposing (expressionToVisitors, transformDeclarationsIntoVisitors)


{-| Lints a file and gives back the errors raised by the given rules.

    errors =
        lintSource rules source

-}
lintSource : List ( Severity, LintRule ) -> String -> Result (List String) (List ( Severity, LintError ))
lintSource rules source =
    source
        |> parseSource
        |> Result.map
            (\statements ->
                rules
                    |> List.concatMap
                        (lintSourceWithRule statements)
            )


lintSourceWithRule : File -> ( Severity, LintRule ) -> List ( Severity, LintError )
lintSourceWithRule file ( severity, rule ) =
    rule file
        |> List.map (\b -> ( severity, b ))


{-| Parse source code into a AST
-}
parseSource : String -> Result (List String) File
parseSource source =
    source
        |> Parser.parse
        -- TODO Improve parsing error handling
        |> Result.mapError (\error -> [ "Parsing error" ])
        -- TODO Add all files to have more context https://package.elm-lang.org/packages/stil4m/elm-syntax/7.0.2/Elm-Processing
        |> Result.map (process init)


{-| Lints source code using a given rule implementation, and gives back a list of errors that were found.

    rule : LintRule
    rule input =
        lint input implementation

    implementation : LintRuleImplementation Context
    implementation =
        { typeFn = doNothing
        , expressionFn = expressionFn
        , moduleEndFn = \ctx -> ( [], ctx )
        , initialContext = Context
        }

-}
lint : File -> LintRuleImplementation context -> List LintError
lint file rule =
    file.declarations
        |> transformDeclarationsIntoVisitors
        |> lintWithVisitors rule


{-| Visit an expression using a sub rule implementation. The use of this function is not encouraged, but it can make
part of the implementation of complex rules much easier. It gives back a list of errors and a context.

    expressionFn : Context -> Direction Expression -> ( List LintError, Context )
    expressionFn ctx node =
        case node of
            Enter (Case expr patterns) ->
                visitExpression subimplementation expr

            _ ->
                ( [], ctx )

    subimplementation : LintRuleImplementation Subcontext
    subimplementation =
        { statementFn = doNothing
        , typeFn = doNothing
        , expressionFn = subexpressionFn
        , moduleEndFn = \ctx -> ( [], ctx )
        , initialContext = Subcontext
        }

-}
visitExpression : LintRuleImplementation context -> Node Expression -> ( List LintError, context )
visitExpression rule expression =
    expressionToVisitors expression
        |> List.foldl (visitAndAccumulate rule) ( [], rule.initialContext )


visitAndAccumulate : LintRuleImplementation context -> Visitor context -> ( List LintError, context ) -> ( List LintError, context )
visitAndAccumulate rule visitor ( errors, ctx ) =
    visitor rule ctx
        |> Tuple.mapFirst (\errors_ -> errors ++ errors_)


lintWithVisitors : LintRuleImplementation context -> List (Visitor context) -> List LintError
lintWithVisitors rule visitors =
    visitors
        |> List.foldl (visitAndAccumulate rule) ( [], rule.initialContext )
        |> Tuple.first
