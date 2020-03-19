module WithModuleContext_AtMostOnce exposing (rule)

{-| We want to forbid `Rule.withModuleContext` from being called twice.
-}

import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "WithModuleContext_AtMostOnce" ()
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = \_ _ () -> ()
            , fromModuleToProject = \_ _ () -> ()
            , foldProjectContexts = \_ () -> ()
            }
        |> Rule.withModuleContext
            { fromProjectToModule = \_ _ () -> ()
            , fromModuleToProject = \_ _ () -> ()
            , foldProjectContexts = \_ () -> ()
            }
        |> Rule.withFinalProjectEvaluation (\_ -> [])
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} () -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ()
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\_ () -> ( [], () ))
