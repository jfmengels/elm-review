module NoExposingEverything exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbids exporting everything from a module.

Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way.
The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access its internal details.
Therefore, the API should be explicitly defined and ideally as small as possible.

    config =
        [ NoExposingEverything.rule
        ]

If you would like to expose everything in your tests, you can configure the rule
in the following manner:

    config =
        [ NoExposingEverything.rule
            |> Rule.ignoreErrorsForDirectories [ "tests/" ]
        ]


## Fail

    module A exposing (..)


## Success

    module A exposing (B(..), C, d)


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/review-common/example --rules NoExposingEverything
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoExposingEverything" ()
        |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> List (Error {})
moduleDefinitionVisitor moduleNode =
    case Module.exposingList <| Node.value moduleNode of
        Exposing.All range ->
            [ Rule.error
                { message = "Module exposes everything implicitly \"(..)\""
                , details =
                    [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                    ]
                }
                { start = { row = range.start.row, column = range.start.column - 1 }
                , end = { row = range.end.row, column = range.end.column + 1 }
                }
            ]

        Exposing.Explicit _ ->
            []
