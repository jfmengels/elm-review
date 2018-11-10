module Lint.Types exposing
    ( LintError, Direction(..)
    , LintRule, Severity(..)
    , LintRuleImplementation, createRule
    , Visitor, LintResult
    , evaluateExpression, finalEvaluation, initialContext
    )

{-| This module contains types that are used for writing rules.


# Elementary types

@docs LintError, Direction


# Configuration

@docs LintRule, Severity


# Writing rules

@docs LintRuleImplementation, createRule


# Internal types

@docs Visitor, LintResult

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range exposing (Range)


{-| Value that describes an error found by a given rule, that contains the name of the rule that raised the error, and
a description of the error.

    error : LintError
    error =
        LintError "NoDebug" "Forbidden use of Debug"

-}
type alias LintError =
    { rule : String
    , message : String
    , range : Range
    }


{-| When visiting the AST, nodes are visited twice:

  - on Enter, before the children of the node will be visited

  - on Exit, after the children of the node have been visited

    expression : Context -> Direction Expression -> ( List LintError, Context )
    expression ctx node =
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
type Direction
    = Enter
    | Exit


{-| A LintRuleImplementation is the implementation of a rule. It is a record that contains:

  - initialContext: An initial context

  - expression: A LintImplementation for Expression nodes

  - moduleEndFn: A function that takes a context and returns a list of error. Similar to a LintImplementation, but will
    be called after visiting the whole AST.

    rule : LintRule
    rule input =
    lint input implementation

    implementation : LintRuleImplementation Context
    implementation =
    { expression = expression
    , moduleEndFn = (\\ctx -> ( [], ctx ))
    , initialContext = Context
    }

-}
type LintRuleImplementation context
    = LintRuleImplementation
        { initContext : context
        , visitors : Visitors context
        }


type alias Visitors context =
    { expressionFn : context -> Direction -> Node Expression -> ( List LintError, context )
    , moduleEndFn : context -> ( List LintError, context )
    }


createRule : context -> (Visitors context -> Visitors context) -> LintRuleImplementation context
createRule initContext createVisitors =
    LintRuleImplementation
        { initContext = initContext
        , visitors =
            createVisitors
                { expressionFn = \ctx direction node -> ( [], ctx )
                , moduleEndFn = \ctx -> ( [], ctx )
                }
        }


initialContext : LintRuleImplementation context -> context
initialContext (LintRuleImplementation { initContext }) =
    initContext


evaluateExpression : LintRuleImplementation context -> context -> Direction -> Node Expression -> ( List LintError, context )
evaluateExpression (LintRuleImplementation { visitors }) =
    visitors.expressionFn


finalEvaluation : LintRuleImplementation context -> context -> ( List LintError, context )
finalEvaluation (LintRuleImplementation { visitors }) =
    visitors.moduleEndFn


{-| Shortcut to the result of a lint rule
-}
type alias LintResult =
    Result (List String) (List LintError)


{-| Shortcut to a lint rule
-}
type alias LintRule =
    File -> List LintError


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
