module Lint.Rule.NoDebug exposing (rule)

{-| Forbid the use of `Debug` before it goes into production or fails in the CI.


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of `Debug` before it goes into production or fails in the CI.

    config =
        [ ( Critical, NoDebug.rule )
        ]


## Fail

    if Debug.log "condition" condition then
        a

    else
        b

    if condition then
        Debug.crash "Nooo!"

    else
        value


## Success

    if condition then
        a

    else
        b

-}
rule : Rule
rule =
    Rule.newSchema "NoDebug"
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromSchema


error : Node a -> Error
error node =
    Rule.error "Forbidden use of Debug" (Node.range node)


importVisitor : Node Import -> List Error
importVisitor node =
    let
        moduleNameNode : Node (List String)
        moduleNameNode =
            node |> Node.value |> .moduleName
    in
    if Node.value moduleNameNode == [ "Debug" ] then
        [ error moduleNameNode ]

    else
        []


expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case Node.value node of
        FunctionOrValue moduleName fnName ->
            if moduleName == [ "Debug" ] then
                [ error node ]

            else
                []

        _ ->
            []
