module NoNoOpMsg exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports NoOp messages

    config =
        [ NoNoOpMsg.rule
        ]


## Fail

    type Msg
        = NoOp


## Success

    type Msg
        = DomNodeWasFocused


## When (not) to enable this rule

This rule is not useful when you are working on a package, since you don't have an update function.


## Try it out

You can try this rule out by running the following command:

```bash
elm - review --template jfmengels/elm-review-noop/preview --rules NoNoOpMsg
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoNoOpMsg" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.CustomTypeDeclaration { constructors } ->
            constructors
                |> List.map
                    (\constructor ->
                        constructor
                            |> Node.value
                            |> .name
                    )
                |> List.filter
                    (\constructorName ->
                        (Node.value constructorName == "NoOp")
                            || (Node.value constructorName == "Noop")
                    )
                |> List.map error

        _ ->
            []


error : Node String -> Error {}
error node =
    Rule.error
        { message = "Don't use NoOp, give it a better name"
        , details =
            [ "A Msg name should explain what happened. NoOp means that nothing happened."
            , "Even if you don't care about handling the event, give it a name that describes what happened."
            , "@noahzgordon's talk on it: https://www.youtube.com/watch?v=w6OVDBqergc"
            ]
        }
        (Node.range node)
