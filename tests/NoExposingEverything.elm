module NoExposingEverything exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbids exporting everything from a module.

🔧 Running with `--fix` will automatically fix all the reported errors.

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
elm-review --template jfmengels/elm-review-common/example --rules NoExposingEverything
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoExposingEverything" ExposingOk
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.fromModuleRuleSchema


type ExposingContext
    = ExposingOk
    | ExposingAll Range


moduleDefinitionVisitor : Node Module -> ExposingContext -> ( List (Error {}), ExposingContext )
moduleDefinitionVisitor moduleNode _ =
    case Module.exposingList <| Node.value moduleNode of
        Exposing.All range ->
            ( [], ExposingAll range )

        Exposing.Explicit _ ->
            ( [], ExposingOk )


declarationListVisitor : List (Node Declaration) -> ExposingContext -> ( List (Error {}), ExposingContext )
declarationListVisitor declarations context =
    case context of
        ExposingAll range ->
            ( [ Rule.errorWithFix
                    { message = "Module exposes everything implicitly \"(..)\""
                    , details =
                        [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access its internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                        ]
                    }
                    { start = { row = range.start.row, column = range.start.column - 1 }
                    , end = { row = range.end.row, column = range.end.column + 1 }
                    }
                    [ exposingDeclarationList declarations
                        |> String.join ", "
                        |> Fix.replaceRangeBy range
                    ]
              ]
            , context
            )

        _ ->
            ( [], context )


exposingDeclarationList : List (Node Declaration) -> List String
exposingDeclarationList declarations =
    List.map exposingDeclarationName declarations


exposingDeclarationName : Node Declaration -> String
exposingDeclarationName (Node _ declaration) =
    case declaration of
        Declaration.AliasDeclaration { name } ->
            Node.value name

        Declaration.CustomTypeDeclaration { name } ->
            Node.value name ++ "(..)"

        Declaration.FunctionDeclaration function ->
            functionDeclarationName function

        Declaration.InfixDeclaration { operator } ->
            "(" ++ Node.value operator ++ ")"

        Declaration.PortDeclaration { name } ->
            Node.value name

        Declaration.Destructuring _ _ ->
            ""


functionDeclarationName : Expression.Function -> String
functionDeclarationName { declaration } =
    declaration |> Node.value |> .name |> Node.value
