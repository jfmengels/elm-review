module Review.Test.ArbitraryFixRule exposing
    ( message, details
    , rule
    )

{-| Test helper rule to inject arbitrary fixes into a file.

@docs Rule
@docs message, details

-}

import Dict exposing (Dict)
import Review.Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)


rule : String -> List Fix -> Rule
rule filePathToFix edits =
    Rule.newProjectRuleSchema "TestRule" Dict.empty
        |> Rule.withModuleVisitor (Rule.withSimpleModuleDefinitionVisitor (always []))
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator (\_ -> ())
            , fromModuleToProject =
                Rule.initContextCreator (\filePath moduleKey _ -> Dict.singleton filePath moduleKey)
                    |> Rule.withFilePath
                    |> Rule.withModuleKey
            , foldProjectContexts = Dict.union
            }
        |> Rule.withFinalProjectEvaluation (finalEvaluation filePathToFix edits)
        |> Rule.fromProjectRuleSchema


finalEvaluation : String -> List Fix -> Dict String Rule.ModuleKey -> List (Error { useErrorForModule : () })
finalEvaluation filePathToFix edits dict =
    case Dict.get filePathToFix dict of
        Just moduleKey ->
            [ Rule.globalError
                { message = message
                , details = details
                }
                |> Rule.withFixesV2 [ Rule.editModule moduleKey edits ]
            ]

        Nothing ->
            Debug.todo ("Couldn't find file " ++ filePathToFix ++ " in test project")


message : String
message =
    "Message"


details : List String
details =
    [ "Details" ]
