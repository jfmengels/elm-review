module ReportGlobalErrorWithFixesInAllFiles exposing (rule)

import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, FixesV2, ModuleKey, Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "ReportGlobalErrorWithFixesInAllFiles" []
        |> Rule.withModuleVisitor (Rule.withSimpleCommentsVisitor (always []))
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator (\_ -> ())
            , fromModuleToProject =
                Rule.initContextCreator
                    (\moduleKey () ->
                        [ Rule.fixesForModule moduleKey
                            [ Fix.insertAt { row = 1, column = 1 } "-- Hi\n" ]
                        ]
                    )
                    |> Rule.withModuleKey
            , foldProjectContexts = List.append
            }
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema


initialProjectContext : List FixesV2
initialProjectContext =
    []


finalProjectEvaluation : List FixesV2 -> List (Error scope)
finalProjectEvaluation fixes =
    [ Rule.globalError
        { message = "Oh no"
        , details = [ "I'll fix all modules now" ]
        }
        |> Rule.withFixesV2 fixes
    ]


error : Expression.Function -> Error {}
error function =
    Rule.error
        { message = "The `subscription` function never returns any subscriptions"
        , details = [ "The `subscription` function never returns any subscriptions. You might as well remove it." ]
        }
        (Node.value function.declaration |> .expression |> Node.range)
