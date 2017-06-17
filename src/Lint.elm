module Lint exposing (lintSource, lint, visitExpression, doNothing)

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
@docs lint, doNothing, visitExpression
-}

import Ast
import Ast.Expression exposing (Expression)
import Ast.Statement exposing (Statement)
import Combine
import Lint.Types exposing (LintRule, LintError, LintImplementation, LintRuleImplementation, Direction, Visitor, Severity, Severity(..))
import Lint.Visitor exposing (transformStatementsIntoVisitors, expressionToVisitors)
import Regex


{-| Lints a file and gives back the errors raised by the given rules.

    errors =
        lintSource rules source
-}
lintSource : List ( Severity, LintRule ) -> String -> Result (List String) (List ( Severity, LintError ))
lintSource rules source =
    source
        |> parseSource
        |> Result.map
            (\( _, _, statements ) ->
                rules
                    |> List.concatMap
                        (lintSourceWithRule statements)
            )


lintSourceWithRule : List Statement -> ( Severity, LintRule ) -> List ( Severity, LintError )
lintSourceWithRule statements ( severity, rule ) =
    rule statements
        |> List.map ((,) severity)


parseSource : String -> Result (List String) (Combine.ParseOk () (List Ast.Statement.Statement))
parseSource source =
    source
        |> removeComments
        |> Ast.parse
        |> Result.mapError (\( _, _, errors ) -> errors)


removeComments : String -> String
removeComments =
    Regex.replace Regex.All (Regex.regex "--.$") (always "")
        >> Regex.replace Regex.All (Regex.regex "\n +\\w+ : .*") (always "")


{-| Lints source code using a given rule implementation, and gives back a list of errors that were found.

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
-}
lint : List Statement -> LintRuleImplementation context -> List LintError
lint statements rule =
    statements
        |> transformStatementsIntoVisitors
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
        , moduleEndFn = (\ctx -> ( [], ctx ))
        , initialContext = Subcontext
        }
-}
visitExpression : LintRuleImplementation context -> Expression -> ( List LintError, context )
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


{-| Basic implementation of a visitor function that does nothing, i.e. return an empty list of errors and an untouched
context. This is used to avoid a bit of boilerplate for visitor functions whose node types we are not interested in.

    implementation : LintRuleImplementation Context
    implementation =
        { statementFn = doNothing
        , typeFn = doNothing
        , expressionFn = expressionFn
        , moduleEndFn = (\ctx -> ( [], ctx ))
        , initialContext = Context
        }
-}
doNothing : LintImplementation a context
doNothing ctx _ =
    ( [], ctx )
