module NoDebug.Log exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of [`Debug.log`](https://package.elm-lang.org/packages/elm/core/latest/Debug) before it goes into production or fails in the CI.

`Debug.log` is useful to debug your code, but should not be pushed to production.

    config =
        [ NoDebug.Log.rule
        ]


## Fail

    if Debug.log "condition" condition then
        a

    else
        b


## Success

    if condition then
        a

    else
        b


# When (not) to use this rule

You should use this rule if you're developing a package meant to be published,
or an application that is put into production, and wish to know about the use of
[`Debug.log`](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)
module before committing your changes.

You should not use this rule if you are developing an application that is not
put into production, and you do not care about having stray debug logs, and you
do not ship to production.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-debug/example --rules NoDebug.Log
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoDebug.Log" { hasLogBeenImported = False }
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { hasLogBeenImported : Bool
    }


error : Node a -> Error {}
error node =
    Rule.error
        { message = "Remove the use of `Debug.log` before shipping to production"
        , details =
            [ "`Debug.log` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
            ]
        }
        (Node.range node)


importVisitor : Node Import -> Context -> ( List nothing, Context )
importVisitor node context =
    let
        moduleName : List String
        moduleName =
            node
                |> Node.value
                |> .moduleName
                |> Node.value
    in
    if moduleName == [ "Debug" ] then
        case node |> Node.value |> .exposingList |> Maybe.map Node.value of
            Just (Exposing.All _) ->
                ( [], { hasLogBeenImported = True } )

            Just (Exposing.Explicit importedNames) ->
                ( [], { hasLogBeenImported = List.any isLog importedNames } )

            Nothing ->
                ( [], context )

    else
        ( [], context )


isLog : Node Exposing.TopLevelExpose -> Bool
isLog node =
    case Node.value node of
        Exposing.FunctionExpose "log" ->
            True

        _ ->
            False


expressionVisitor : Node Expression -> Rule.Direction -> Context -> ( List (Error {}), Context )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue [ "Debug" ] "log" ) ->
            ( [ error node ], context )

        ( Rule.OnEnter, Expression.FunctionOrValue [] "log" ) ->
            if context.hasLogBeenImported then
                ( [ error node ], context )

            else
                ( [], context )

        _ ->
            ( [], context )
