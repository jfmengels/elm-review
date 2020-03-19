module NoElmJsonVisitorForModuleVisitorInProjectRule exposing (rule)

{-| We want to forbid module visitors from using `withElmJsonModuleVisitor`.


# Rule

@docs rule

-}

import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "NoElmJsonVisitorForModuleVisitorInProjectRule" ()
        |> Rule.withModuleVisitor moduleVisitor
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
        |> Rule.withElmJsonModuleVisitor (\_ () -> ())
        |> Rule.withModuleDefinitionVisitor (\_ () -> ( [], () ))
