module NoUnknownCssClasses exposing
    ( rule
    , defaults
    )

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import NoInconsistentAliases exposing (Config)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import String exposing (contains)


{-| Reports... REPLACEME

    config =
        [ NoUnknownCssClasses.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review/example --rules NoUnknownCssClasses
```

-}
rule : Configuration -> Rule
rule (Configuration configuration) =
    Rule.newProjectRuleSchema "NoUnknownCssClasses" (initialProjectContext configuration.knownClasses)
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        -- Enable this if modules need to get information from other modules
        -- |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


type Configuration
    = Configuration
        { knownClasses : Set String
        }


defaults : Configuration
defaults =
    Configuration { knownClasses = Set.empty }


withHardcodedKnownClasses : List String -> Configuration -> Configuration
withHardcodedKnownClasses list (Configuration configuration) =
    Configuration { configuration | knownClasses = List.foldl Set.insert configuration.knownClasses list }


type alias ProjectContext =
    { knownClasses : Set String
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , knownClasses : Set String
    }


moduleVisitor : Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionVisitor


initialProjectContext : Set String -> ProjectContext
initialProjectContext knownClasses =
    { knownClasses = knownClasses
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , knownClasses = projectContext.knownClasses
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { knownClasses = Set.empty
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    { knownClasses = previous.knownClasses
    }


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ name)) :: firstArg :: _) ->
            case ( ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange, name ) of
                ( Just [ "Html", "Attributes" ], "class" ) ->
                    case Node.value firstArg of
                        Expression.Literal str ->
                            ( [ Rule.error
                                    { message = "REPLACEME"
                                    , details = [ "REPLACEME" ]
                                    }
                                    (Node.range firstArg)
                              ]
                            , context
                            )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )
