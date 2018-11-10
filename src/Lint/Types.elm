module Lint.Types exposing
    ( Direction(..)
    , LintRuleImplementation, createRule
    , Visitor, LintResult
    , evaluateExpression, finalEvaluation, initialContext
    )

{-| This module contains functions that are used for writing rules.


# Elementary types

@docs Direction


# Writing rules

@docs LintRuleImplementation, createRule


# Internal types

@docs Visitor, LintResult

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Lint.Error exposing (Error)


{-| When visiting the AST, nodes are visited twice:

  - on Enter, before the children of the node will be visited

  - on Exit, after the children of the node have been visited

    expression : Context -> Direction Expression -> ( List Lint.Error.Error, Context )
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

  - visitEnd: A function that takes a context and returns a list of error. Similar to a LintImplementation, but will
    be called after visiting the whole AST.

    rule : Rule
    rule input =
    lint input implementation

    implementation : LintRuleImplementation Context
    implementation =
    { expression = expression
    , visitEnd = (\\ctx -> ( [], ctx ))
    , initialContext = Context
    }

-}
type LintRuleImplementation context
    = LintRuleImplementation
        { initContext : context
        , visitors : Visitors context
        }


type alias Visitors context =
    { visitExpression : context -> Direction -> Node Expression -> ( List Error, context )
    , visitEnd : context -> ( List Error, context )
    }


createRule : context -> (Visitors context -> Visitors context) -> LintRuleImplementation context
createRule initContext createVisitors =
    LintRuleImplementation
        { initContext = initContext
        , visitors =
            createVisitors
                { visitExpression = \ctx direction node -> ( [], ctx )
                , visitEnd = \ctx -> ( [], ctx )
                }
        }


initialContext : LintRuleImplementation context -> context
initialContext (LintRuleImplementation { initContext }) =
    initContext


evaluateExpression : LintRuleImplementation context -> context -> Direction -> Node Expression -> ( List Error, context )
evaluateExpression (LintRuleImplementation { visitors }) =
    visitors.visitExpression


finalEvaluation : LintRuleImplementation context -> context -> ( List Error, context )
finalEvaluation (LintRuleImplementation { visitors }) =
    visitors.visitEnd


{-| Shortcut to the result of a lint rule
-}
type alias LintResult =
    Result (List String) (List Error)


{-| Shorthand for a function that takes a rule's implementation, a context and returns ( List Lint.Error.Error, context ).
A Visitor represents a node and calls the appropriate function for the given node type.

Note: this is internal API, and will be removed in a future version.

-}
type alias Visitor context =
    LintRuleImplementation context -> context -> ( List Error, context )
