module NoMissingSubscriptionsCall exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleInformation as ModuleInformation exposing (ModuleInformation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports likely missing calls to a `subscriptions` function.

    config =
        [ NoMissingSubscriptionsCall.rule
        ]


## Fail

    import SomeModule

    update msg model =
        case msg of
            UsedMsg subMsg ->
                SomeModule.update subMsg model.used

    subscriptions model =
        -- We used `SomeModule.update` but not `SomeModule.subscriptions`
        Sub.none

This won't fail if `SomeModule` does not define a `subscriptions` function.


## Success

    import SomeModule

    update msg model =
        case msg of
            UsedMsg subMsg ->
                SomeModule.update subMsg model.used

    subscriptions model =
        SomeModule.subscriptions


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-the-elm-architecture/example --rules NoMissingSubscriptionsCall
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoMissingSubscriptionsCall" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , importedModulesAPI : Dict ModuleName ModuleInformation
    , usesUpdateOfModule : Dict ModuleName Range
    , usesSubscriptionsOfModule : Set ModuleName
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable importedModulesAPI () ->
            { lookupTable = lookupTable
            , importedModulesAPI = importedModulesAPI
            , usesUpdateOfModule = Dict.empty
            , usesSubscriptionsOfModule = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withImportedModulesAPI


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue _ "update" ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                Just moduleName ->
                    ( [], { context | usesUpdateOfModule = Dict.insert moduleName (Node.range node) context.usesUpdateOfModule } )

                Nothing ->
                    ( [], context )

        Expression.FunctionOrValue _ "subscriptions" ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                Just moduleName ->
                    ( [], { context | usesSubscriptionsOfModule = Set.insert moduleName context.usesSubscriptionsOfModule } )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context.usesUpdateOfModule
        |> Dict.filter
            (\moduleName _ ->
                not (Set.member moduleName context.usesSubscriptionsOfModule)
                    && definesUpdateFunction moduleName context.importedModulesAPI
            )
        |> Dict.toList
        |> List.map
            (\( moduleName, range ) ->
                Rule.error
                    { message = "Missing subscriptions call to " ++ String.join "." moduleName ++ ".subscriptions"
                    , details =
                        [ "The " ++ String.join "." moduleName ++ " module defines a `subscriptions` function, which you are not using even though you are using its `update` function. This makes me think that you are not subscribing to all the things you should."
                        ]
                    }
                    range
            )


definesUpdateFunction : ModuleName -> Dict ModuleName ModuleInformation -> Bool
definesUpdateFunction moduleName dict =
    case Dict.get moduleName dict |> Maybe.andThen (ModuleInformation.getValueByName "subscriptions") of
        Just _ ->
            True

        Nothing ->
            False
