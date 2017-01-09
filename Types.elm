module Types exposing (..)

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)


type alias Error =
    String


type Direction node
    = Enter node
    | Exit node


type alias LintImplementation nodeType context =
    context -> Direction nodeType -> ( List Error, context )


type alias LintRule context =
    { statementFn : LintImplementation Statement context
    , typeFn : LintImplementation Type context
    , expressionFn : LintImplementation Expression context
    , moduleEndFn : context -> ( List Error, context )
    , context : context
    }


type alias Visitor context =
    LintRule context -> context -> ( List Error, context )
