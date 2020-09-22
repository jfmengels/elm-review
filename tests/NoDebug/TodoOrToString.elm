module NoDebug.TodoOrToString exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
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
    Rule.newModuleRuleSchema "NoDebug.TodoOrToString" init
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { hasTodoBeenImported : Bool
    , hasToStringBeenImported : Bool
    }


init : Context
init =
    { hasTodoBeenImported = False
    , hasToStringBeenImported = False
    }


error : Node a -> String -> Error {}
error node name =
    Rule.error
        { message = "Remove the use of `Debug." ++ name ++ "` before shipping to production"
        , details =
            [ "`Debug." ++ name ++ "` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
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
                ( [], { hasTodoBeenImported = True, hasToStringBeenImported = True } )

            Just (Exposing.Explicit importedNames) ->
                ( []
                , { context
                    | hasTodoBeenImported = List.any (is "todo") importedNames
                    , hasToStringBeenImported = List.any (is "toString") importedNames
                  }
                )

            Nothing ->
                ( [], context )

    else
        ( [], context )


is : String -> Node Exposing.TopLevelExpose -> Bool
is name node =
    case Node.value node of
        Exposing.FunctionExpose functionName ->
            name == functionName

        _ ->
            False


expressionVisitor : Node Expression -> Rule.Direction -> Context -> ( List (Error {}), Context )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue [ "Debug" ] name ) ->
            if name == "todo" then
                ( [ error node name ], context )

            else if name == "toString" then
                ( [ error node name ], context )

            else
                ( [], context )

        ( Rule.OnEnter, Expression.FunctionOrValue [] name ) ->
            if name == "todo" && context.hasTodoBeenImported then
                ( [ error node name ], context )

            else if name == "toString" && context.hasToStringBeenImported then
                ( [ error node name ], context )

            else
                ( [], context )

        _ ->
            ( [], context )
