module No_WithModuleContext_Without_WithModuleVisitor exposing (rule)

{-| We want to forbid using `Rule.withModuleContext` when `withModuleVisitor` has
not been used.
-}

import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "No_WithModuleContext_Without_WithModuleVisitor" ()
        |> Rule.withModuleContext
            { fromProjectToModule = \_ _ () -> ()
            , fromModuleToProject = \_ _ () -> ()
            , foldProjectContexts = \_ () -> ()
            }
        |> Rule.withFinalProjectEvaluation (\_ -> [])
        |> Rule.fromProjectRuleSchema
