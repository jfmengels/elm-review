module Lint.Types
    exposing
        ( Direction(..)
        , File
        , LintError
        , LintImplementation
        , LintResult
        , LintRule
        , LintRuleImplementation
        , Reporter
        , Severity
        , Severity(..)
        , Visitor
        )

{-| This module contains types that are used for writing rules.

# Elementary types
@docs LintError, Direction

# Configuration
@docs LintRule, Severity, Reporter

# Writing rules
@docs LintRuleImplementation, LintImplementation

# Internal types
@docs Visitor, LintResult, File
-}

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)


{-| Value that describes an error found by a given rule, that contains the name of the rule that raised the error, and
a description of the error.

    error : LintError
    error =
        LintError "NoDebug" "Forbidden use of Debug"
-}
type alias LintError =
    { rule : String
    , message : String
    }


{-| A LintImplementation is a function that takes a given Context object, as defined by each rule, a node (with a
Direction) and returns a list of errors and an updated Context.
Every rule should implement three LintImplementation functions, one for every Node type (Statement, Type and
Expression).
The Context is there to accumulate information about the source code as the AST is being visited and is shared by all
the LintImplementation functions of your rule.
It must return a list of errors which will be reported to the user, that are violations of the thing the rule wants to
enforce.

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
type alias LintImplementation nodeType context =
    context -> Direction nodeType -> ( List LintError, context )


{-| When visiting the AST, nodes are visited twice:
- on Enter, before the children of the node will be visited
- on Exit, after the children of the node have been visited

    expressionFn : Context -> Direction Expression -> ( List LintError, Context )
    expressionFn ctx node =
        case node of
            Enter (Variable names) ->
                ( [], markVariableAsUsed ctx names )

            -- Find variables declared in `let .. in ..` expression
            Enter (Let declarations body) ->
                ( [], registerVariables ctx declarations )

            -- When exiting the `let .. in ..` expression, report the variables that were not used.
            Exit (Let _ _) ->
                ( unusedVariables ctx |> List.map createError, ctx )
-}
type Direction node
    = Enter node
    | Exit node


{-| A LintRuleImplementation is the implementation of a rule. It is a record that contains:
- initialContext: An initial context
- statementFn: A LintImplementation for Statement nodes
- typeFn: A LintImplementation for Type nodes
- expressionFn: A LintImplementation for Expression nodes
- moduleEndFn: A function that takes a context and returns a list of error. Similar to a LintImplementation, but will
be called after visiting the whole AST.

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
type alias LintRuleImplementation context =
    { statementFn : LintImplementation Statement context
    , typeFn : LintImplementation Type context
    , expressionFn : LintImplementation Expression context
    , moduleEndFn : context -> ( List LintError, context )
    , initialContext : context
    }


{-| Shortcut to the result of a lint rule
-}
type alias LintResult =
    Result (List String) (List LintError)


{-| Shortcut to a lint rule
-}
type alias LintRule =
    List Statement -> List LintError


{-| Shorthand for a function that takes a rule's implementation, a context and returns ( List LintError, context ).
A Visitor represents a node and calls the appropriate function for the given node type.

Note: this is internal API, and will be removed in a future version.
-}
type alias Visitor context =
    LintRuleImplementation context -> context -> ( List LintError, context )


{-| Severity associated to a rule.

- Critical: Transgressions reported by the rule will make the linting process fail.
- Warning: Transgressions reported by the rule will not make the linting process fail.
- Disabled: Rule will not be enforced.
-}
type Severity
    = Disabled
    | Warning
    | Critical


{-| Description of an Elm file.
-}
type alias File =
    { filename : String
    , source : String
    }


{-| Function that summarizes the result of the linting process.
-}
type alias Reporter a =
    List ( File, List ( Severity, LintError ) ) -> a
