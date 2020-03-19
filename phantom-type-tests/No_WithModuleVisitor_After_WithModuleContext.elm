module No_WithModuleVisitor_After_WithModuleContext exposing (rule)

{-| We want to forbid using `Rule.withModuleVisitor` after having used `withModuleContext`.
-}

import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "No_WithModuleVisitor_After_WithModuleContext" ()
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = \_ _ () -> ()
            , fromModuleToProject = \_ _ () -> ()
            , foldProjectContexts = \_ () -> ()
            }
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withFinalProjectEvaluation (\_ -> [])
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} () -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ()
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\_ () -> ( [], () ))
