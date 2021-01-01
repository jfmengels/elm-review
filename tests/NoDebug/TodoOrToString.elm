module NoDebug.TodoOrToString exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of [`Debug.todo`] and [`Debug.toString`].

    config =
        [ NoDebug.TodoOrToString.rule
        ]

The reason why there is a is separate rule for handling [`Debug.log`] and one for
handling [`Debug.todo`] and [`Debug.toString`], is because these two functions
are reasonable and useful to have in tests.

You can for instance create test data without having to handle the error case
everywhere. If you do enter the error case in the following example, then tests
will fail.

    testEmail : Email
    testEmail =
        case Email.fromString "some.email@domain.com" of
            Just email ->
                email

            Nothing ->
                Debug.todo "Supplied an invalid email in tests"

If you want to allow these functions in tests but not in production code, you
can configure the rule like this.

    import Review.Rule as Rule exposing (Rule)

    config =
        [ NoDebug.TodoOrToString.rule
            |> Rule.ignoreErrorsForDirectories [ "tests/" ]
        ]


## Fail

    _ =
        if condition then
            a

        else
            Debug.todo ""

    _ =
        Debug.toString data


## Success

    if condition then
        a

    else
        b


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-debug/example --rules NoDebug.TodoOrToString
```

[`Debug.log`]: https://package.elm-lang.org/packages/elm/core/latest/Debug#log
[`Debug.todo`]: https://package.elm-lang.org/packages/elm/core/latest/Debug#todo
[`Debug.toString`]: https://package.elm-lang.org/packages/elm/core/latest/Debug#toString

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoDebug.TodoOrToString" init
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    ModuleNameLookupTable


init : Rule.ContextCreator () Context
init =
    Rule.initContextCreator (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue _ name ->
            if name == "todo" || name == "toString" then
                case ModuleNameLookupTable.moduleNameFor context node of
                    Just [ "Debug" ] ->
                        ( [ Rule.error
                                { message = "Remove the use of `Debug." ++ name ++ "` before shipping to production"
                                , details =
                                    [ "`Debug." ++ name ++ "` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
                                    ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

            else
                ( [], context )

        _ ->
            ( [], context )
