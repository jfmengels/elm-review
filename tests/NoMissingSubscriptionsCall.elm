module NoMissingSubscriptionsCall exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
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
    Rule.newProjectRuleSchema "NoMissingSubscriptionsCall" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ProjectContext =
    { modulesThatExposeSubscriptionsAndUpdate : Set ModuleName
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , modulesThatExposeSubscriptionsAndUpdate : Set ModuleName
    , definesUpdate : Bool
    , definesSubscriptions : Bool
    , usesUpdateOfModule : Dict ModuleName Range
    , usesSubscriptionsOfModule : Set ModuleName
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { modulesThatExposeSubscriptionsAndUpdate = Set.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , modulesThatExposeSubscriptionsAndUpdate = projectContext.modulesThatExposeSubscriptionsAndUpdate
            , definesUpdate = False
            , definesSubscriptions = False
            , usesUpdateOfModule = Dict.empty
            , usesSubscriptionsOfModule = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleContext ->
            { modulesThatExposeSubscriptionsAndUpdate =
                if moduleContext.definesSubscriptions && moduleContext.definesUpdate then
                    Set.singleton moduleName

                else
                    Set.empty
            }
        )
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { modulesThatExposeSubscriptionsAndUpdate =
        Set.union
            newContext.modulesThatExposeSubscriptionsAndUpdate
            previousContext.modulesThatExposeSubscriptionsAndUpdate
    }


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Error nothing), ModuleContext )
declarationVisitor node moduleContext =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            case
                function.declaration
                    |> Node.value
                    |> .name
                    |> Node.value
            of
                "update" ->
                    ( [], { moduleContext | definesUpdate = True } )

                "subscriptions" ->
                    ( [], { moduleContext | definesSubscriptions = True } )

                _ ->
                    ( [], moduleContext )

        _ ->
            ( [], moduleContext )


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node moduleContext =
    case Node.value node of
        Expression.FunctionOrValue _ "update" ->
            ( [], registerUpdateFunction node moduleContext )

        Expression.FunctionOrValue _ "subscriptions" ->
            ( [], registerSubscriptionsFunction node moduleContext )

        _ ->
            ( [], moduleContext )


registerUpdateFunction : Node a -> ModuleContext -> ModuleContext
registerUpdateFunction node moduleContext =
    case ModuleNameLookupTable.moduleNameFor moduleContext.lookupTable node of
        Just moduleName ->
            if Set.member moduleName moduleContext.modulesThatExposeSubscriptionsAndUpdate then
                { moduleContext | usesUpdateOfModule = Dict.insert moduleName (Node.range node) moduleContext.usesUpdateOfModule }

            else
                moduleContext

        Nothing ->
            moduleContext


registerSubscriptionsFunction : Node a -> ModuleContext -> ModuleContext
registerSubscriptionsFunction node moduleContext =
    case ModuleNameLookupTable.moduleNameFor moduleContext.lookupTable node of
        Just moduleName ->
            if Set.member moduleName moduleContext.modulesThatExposeSubscriptionsAndUpdate then
                { moduleContext | usesSubscriptionsOfModule = Set.insert moduleName moduleContext.usesSubscriptionsOfModule }

            else
                moduleContext

        Nothing ->
            moduleContext


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation moduleContext =
    moduleContext.usesUpdateOfModule
        |> Dict.filter (\moduleName _ -> not <| Set.member moduleName moduleContext.usesSubscriptionsOfModule)
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
