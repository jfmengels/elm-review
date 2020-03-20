module No_Project_Rule_Without_Visitors exposing (rule)

{-| We want to forbid having project rules without any visitors.
-}

import Review.Rule as Rule exposing (Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "No_Project_Rule_Without_Visitors" ()
        |> Rule.fromProjectRuleSchema
