module Lint.Rules.SimplifyPropertyAccess exposing (rule)

{-|

@docs rule


# Fail

    a =
        List.map (\x -> x.foo) values


# Success

    a =
        List.map .foo values

-}

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Lint exposing (doNothing, lint)
import Lint.Types exposing (Direction(..), LintError, LintRule, LintRuleImplementation)


type alias Context =
    {}


{-| Simplify functions that return the property of a record by using an access function

    rules =
        [ SimplifyPropertyAccess.rule
        ]

-}
rule : LintRule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = \ctx -> ( [], ctx )
    , initialContext = Context
    }


error : String -> LintError
error property =
    LintError "SimplifyPropertyAccess" ("Access to property `" ++ property ++ "` could be simplified by using `." ++ property ++ "`")


expressionFn : Context -> Direction Expression -> ( List LintError, Context )
expressionFn ctx node =
    case node of
        Enter (Lambda [ Variable paramNames ] (Access (Variable varName) properties)) ->
            if List.length properties == 1 && varName == paramNames then
                ( [ String.join "" properties |> error ], ctx )

            else
                ( [], ctx )

        _ ->
            ( [], ctx )


statementFn : Context -> Direction Statement -> ( List LintError, Context )
statementFn ctx node =
    case node of
        Enter (FunctionDeclaration _ [ Variable paramNames ] (Access (Variable varName) properties)) ->
            if List.length properties == 1 && varName == paramNames then
                ( [ String.join "" properties |> error ], ctx )

            else
                ( [], ctx )

        _ ->
            ( [], ctx )
