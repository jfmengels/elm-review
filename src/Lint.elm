module Lint exposing
    ( Rule, Severity(..)
    , lintSource
    , lint, expressionVisitor
    , createRule
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

@docs lint, expressionVisitor

-}

import Elm.Parser as Parser
import Elm.Processing exposing (addFile, init, process)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Lint.Error as Error exposing (Error)
import Lint.NodeToVisitor exposing (createVisitorsForFile, expressionToVisitors)
import Lint.Rule as Rule exposing (Direction, Implementation, Visitor)
import Lint.RuleError as RuleError exposing (RuleError)


{-| Shortcut to a lint rule
-}
type alias Rule =
    { name : String
    , analyze : File -> List Error
    }


createRule : String -> (File -> List Error) -> Rule
createRule name analyze =
    { name = name
    , analyze = analyze
    }


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
lintSource : List ( Severity, Rule ) -> String -> Result (List String) (List ( Severity, RuleError ))
lintSource rules source =
    source
        |> parseSource
        |> Result.map
            (\statements ->
                rules
                    |> List.concatMap
                        (lintSourceWithRule statements)
            )


lintSourceWithRule : File -> ( Severity, Rule ) -> List ( Severity, RuleError )
lintSourceWithRule file ( severity, rule ) =
    rule.analyze file
        |> List.map (\error -> ( severity, RuleError.fromError rule.name error ))


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

    implementation : Implementation Context
    implementation =
        { typeFn = doNothing
        , expressionVisitor = expressionVisitor
        , visitEnd = \ctx -> ( [], ctx )
        , initialContext = Context
        }

-}
lint : Implementation context -> File -> List Error
lint rule file =
    createVisitorsForFile file
        |> lintWithVisitors rule


{-| Visit an expression using a sub rule implementation. The use of this function is not encouraged, but it can make
part of the implementation of complex rules much easier. It gives back a list of errors and a context.

    expressionVisitor : Context -> Direction Expression -> ( List Lint.Error.Error, Context )
    expressionVisitor ctx node =
        case node of
            Enter (Case expr patterns) ->
                expressionVisitor subimplementation expr

            _ ->
                ( [], ctx )

    subimplementation : Implementation Subcontext
    subimplementation =
        { statementFn = doNothing
        , typeFn = doNothing
        , expressionVisitor = subvisitExpression
        , visitEnd = \ctx -> ( [], ctx )
        , initialContext = Subcontext
        }

-}
expressionVisitor : Implementation context -> Node Expression -> ( List Error, context )
expressionVisitor rule expression =
    expressionToVisitors expression
        |> List.foldl (visitAndAccumulate rule) ( [], Rule.initialContext rule )


visitAndAccumulate : Implementation context -> Visitor context -> ( List Error, context ) -> ( List Error, context )
visitAndAccumulate rule visitor ( errors, ctx ) =
    let
        ( newErrors, newContext ) =
            visitor rule ctx
    in
    ( List.reverse newErrors ++ errors, newContext )


lintWithVisitors : Implementation context -> List (Visitor context) -> List Error
lintWithVisitors rule visitors =
    visitors
        |> List.foldl (visitAndAccumulate rule) ( [], Rule.initialContext rule )
        |> Tuple.first
        |> List.reverse
