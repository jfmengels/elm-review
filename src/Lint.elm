module Lint exposing
    ( Rule, Severity(..)
    , lintSource
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


# Configuration

@docs Rule, Severity


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
import Lint.Error exposing (Error)
import Lint.Types exposing (Direction, LintRuleImplementation, Visitor, initialContext)
import Lint.Visitor exposing (expressionToVisitors, transformDeclarationsIntoVisitors)


{-| Shortcut to a lint rule
-}
type alias Rule =
    File -> List Error


{-| Severity associated to a rule.

  - Critical: Transgressions reported by the rule will make the linting process fail.
  - Warning: Transgressions reported by the rule will not make the linting process fail.
  - Disabled: Rule will not be enforced.

-}
type Severity
    = Disabled
    | Warning
    | Critical


{-| Lints a file and gives back the errors raised by the given rules.

    errors =
        lintSource rules source

-}
lintSource : List ( Severity, Rule ) -> String -> Result (List String) (List ( Severity, Error ))
lintSource rules source =
    source
        |> parseSource
        |> Result.map
            (\statements ->
                rules
                    |> List.concatMap
                        (lintSourceWithRule statements)
            )


lintSourceWithRule : File -> ( Severity, Rule ) -> List ( Severity, Error )
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

    rule : Rule
    rule input =
        lint input implementation

    implementation : LintRuleImplementation Context
    implementation =
        { typeFn = doNothing
        , visitExpression = visitExpression
        , visitEnd = \ctx -> ( [], ctx )
        , initialContext = Context
        }

-}
lint : File -> LintRuleImplementation context -> List Error
lint file rule =
    file.declarations
        |> transformDeclarationsIntoVisitors
        |> lintWithVisitors rule


{-| Visit an expression using a sub rule implementation. The use of this function is not encouraged, but it can make
part of the implementation of complex rules much easier. It gives back a list of errors and a context.

    visitExpression : Context -> Direction Expression -> ( List Lint.Error.Error, Context )
    visitExpression ctx node =
        case node of
            Enter (Case expr patterns) ->
                visitExpression subimplementation expr

            _ ->
                ( [], ctx )

    subimplementation : LintRuleImplementation Subcontext
    subimplementation =
        { statementFn = doNothing
        , typeFn = doNothing
        , visitExpression = subvisitExpression
        , visitEnd = \ctx -> ( [], ctx )
        , initialContext = Subcontext
        }

-}
visitExpression : LintRuleImplementation context -> Node Expression -> ( List Error, context )
visitExpression rule expression =
    expressionToVisitors expression
        |> List.foldl (visitAndAccumulate rule) ( [], initialContext rule )


visitAndAccumulate : LintRuleImplementation context -> Visitor context -> ( List Error, context ) -> ( List Error, context )
visitAndAccumulate rule visitor ( errors, ctx ) =
    visitor rule ctx
        |> Tuple.mapFirst (\errors_ -> errors ++ errors_)


lintWithVisitors : LintRuleImplementation context -> List (Visitor context) -> List Error
lintWithVisitors rule visitors =
    visitors
        |> List.foldl (visitAndAccumulate rule) ( [], initialContext rule )
        |> Tuple.first
