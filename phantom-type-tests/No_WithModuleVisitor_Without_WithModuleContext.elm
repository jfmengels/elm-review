module No_WithModuleVisitor_Without_WithModuleContext exposing (rule)

{-| We want to require calling `Rule.withModuleContext` after using `withModuleVisitor`.
-}

import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "No_WithModuleVisitor_Without_WithModuleContext" ()
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} () -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ()
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\_ () -> ( [], () ))

