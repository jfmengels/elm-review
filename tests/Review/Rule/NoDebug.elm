module Review.Rule.NoDebug exposing (rule)

{-| Forbid the use of the [`Debug`](https://package.elm-lang.org/packages/elm/core/latest/Debug) module before it goes into production or fails in the CI.


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of the [`Debug`](https://package.elm-lang.org/packages/elm/core/latest/Debug) module before it goes into production or fails in the CI.

    config =
        [ NoDebug.rule
        ]


## Fail

    if Debug.log "condition" condition then
        a

    else
        b

    if condition then
        Debug.todo "Nooo!"

    else
        value


## Success

    if condition then
        a

    else
        b


# When (not) to use this rule

You should use this rule if you're developing a package meant to be published,
or an application that is put into production, and wish to know about the use of
the [`Debug`](https://package.elm-lang.org/packages/elm/core/latest/Debug)
module before committing your changes.

You should not use this rule if you are developing an application that is not
put into production, and you do not care about having stray debug logs or
runtime exceptions caused by [`Debug.todo`](https://package.elm-lang.org/packages/elm/core/latest/Debug#todo),
and you do not ship to production.

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoDebug" ()
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


error : Node a -> Error {}
error node =
    Rule.error
        { message = "Remove the use of `Debug` before shipping to production"
        , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
        }
        (Node.range node)


importVisitor : Node Import -> List (Error {})
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


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        FunctionOrValue moduleName fnName ->
            if moduleName == [ "Debug" ] then
                [ error node ]

            else
                []

        _ ->
            []
