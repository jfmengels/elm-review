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
import Review.Rule as Rule exposing (Error, Rule)
import Scope
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

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoMissingSubscriptionsCall" initialProjectContext
        |> Scope.addProjectVisitors
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ProjectContext =
    { scope : Scope.ProjectContext
    , modulesThatExposeSubscriptionsAndUpdate : Set ModuleName
    }


type alias ModuleContext =
    { scope : Scope.ModuleContext
    , modulesThatExposeSubscriptionsAndUpdate : Set ModuleName

    --, usesUpdate : Bool
    --, usesSubscription : Bool
    , definesUpdate : Bool
    , definesSubscriptions : Bool
    , usesUpdateOfModule : Dict ModuleName Range
    , usesSubscriptionsOfModule : Set ModuleName
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { scope = Scope.initialProjectContext
    , modulesThatExposeSubscriptionsAndUpdate = Set.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ projectContext =
    { scope = Scope.fromProjectToModule projectContext.scope
    , modulesThatExposeSubscriptionsAndUpdate = projectContext.modulesThatExposeSubscriptionsAndUpdate
    , definesUpdate = False
    , definesSubscriptions = False
    , usesUpdateOfModule = Dict.empty
    , usesSubscriptionsOfModule = Set.empty
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject _ moduleName moduleContext =
    { scope = Scope.fromModuleToProject moduleName moduleContext.scope
    , modulesThatExposeSubscriptionsAndUpdate =
        if moduleContext.definesSubscriptions && moduleContext.definesUpdate then
            Set.singleton (Node.value moduleName)

        else
            Set.empty
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { scope = Scope.foldProjectContexts newContext.scope previousContext.scope
    , modulesThatExposeSubscriptionsAndUpdate =
        Set.union
            newContext.modulesThatExposeSubscriptionsAndUpdate
            previousContext.modulesThatExposeSubscriptionsAndUpdate
    }


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node direction moduleContext =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue moduleName "update" ) ->
            let
                realModuleName : List String
                realModuleName =
                    Scope.moduleNameForValue moduleContext.scope "update" moduleName
            in
            if Set.member realModuleName moduleContext.modulesThatExposeSubscriptionsAndUpdate then
                ( [], { moduleContext | usesUpdateOfModule = Dict.insert realModuleName (Node.range node) moduleContext.usesUpdateOfModule } )

            else
                ( [], moduleContext )

        ( Rule.OnEnter, Expression.FunctionOrValue moduleName "subscriptions" ) ->
            let
                realModuleName : List String
                realModuleName =
                    Scope.moduleNameForValue moduleContext.scope "subscriptions" moduleName
            in
            if Set.member realModuleName moduleContext.modulesThatExposeSubscriptionsAndUpdate then
                ( [], { moduleContext | usesSubscriptionsOfModule = Set.insert realModuleName moduleContext.usesSubscriptionsOfModule } )

            else
                ( [], moduleContext )

        _ ->
            ( [], moduleContext )


declarationVisitor : Node Declaration -> Rule.Direction -> ModuleContext -> ( List (Error nothing), ModuleContext )
declarationVisitor declaration direction moduleContext =
    case ( direction, Node.value declaration ) of
        ( Rule.OnEnter, Declaration.FunctionDeclaration function ) ->
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
