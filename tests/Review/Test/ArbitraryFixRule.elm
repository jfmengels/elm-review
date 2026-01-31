module Review.Test.ArbitraryFixRule exposing
    ( message, details
    , rule
    )

{-| Test helper rule to inject arbitrary fixes into a file.

@docs Rule
@docs message, details

-}

import Dict exposing (Dict)
import Review.Fix exposing (Edit)
import Review.Rule as Rule exposing (Error, Rule)


rule : List { path : String, edits : List Edit } -> Rule
rule fixes =
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
        |> Rule.withFinalProjectEvaluation (finalEvaluation fixes)
        |> Rule.fromProjectRuleSchema


finalEvaluation : List { path : String, edits : List Edit } -> Dict String Rule.ModuleKey -> List (Error { useErrorForModule : () })
finalEvaluation intendedFixes dict =
    let
        fixes : List Rule.FixV2
        fixes =
            List.map
                (\{ path, edits } ->
                    case Dict.get path dict of
                        Just moduleKey ->
                            Rule.editModule moduleKey edits

                        Nothing ->
                            Debug.todo ("Couldn't find file " ++ path ++ " in test project")
                )
                intendedFixes
    in
    [ Rule.globalError
        { message = message
        , details = details
        }
        |> Rule.withFixesV2 fixes
    ]


message : String
message =
    "Message"


details : List String
details =
    [ "Details" ]
