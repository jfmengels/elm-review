module Review.Rule.ProjectVisitTest exposing (all)

import Review.Rule as Rule exposing (Error)
import Review.Test
import Set exposing (Set)
import Test exposing (Test, test)


type alias ProjectContext =
    { aModuleKey : Maybe Rule.ModuleKey
    , moduleNames : Set String
    }


initialContext : ProjectContext
initialContext =
    { aModuleKey = Nothing
    , moduleNames = Set.empty
    }


baseRule : Rule.ProjectRuleSchema { hasAtLeastOneVisitor : (), withModuleContext : Rule.Forbidden } ProjectContext ()
baseRule =
    Rule.newProjectRuleSchema "Visitor order" initialContext
        |> Rule.withModuleVisitor (Rule.withSimpleModuleDefinitionVisitor (always []))
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator (\_ -> ())
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation


fromModuleToProject : Rule.ContextCreator a ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleName _ ->
            { aModuleKey = Just moduleKey
            , moduleNames = Set.singleton (String.join "." moduleName)
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { aModuleKey = newContext.aModuleKey
    , moduleNames = Set.union newContext.moduleNames previousContext.moduleNames
    }


finalEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation context =
    case context.aModuleKey of
        Just moduleKey ->
            [ Rule.errorForModule moduleKey
                { message = Set.toList context.moduleNames |> String.join " - "
                , details = [ "details" ]
                }
                { start = { row = 1, column = 1 }
                , end = { row = 1, column = 7 }
                }
            ]

        Nothing ->
            []


all : Test
all =
    Test.describe "Project visitor order"
        [ Test.describe "using withContextFromImportedModules"
            [ test "should visit every file in a project rule" <|
                \() ->
                    [ """module A exposing (..)
a = 1"""
                    , """module B exposing (..)
import A
a = 1"""
                    , """module C exposing (..)
import A
a = 1"""
                    ]
                        |> Review.Test.runOnModules
                            (baseRule
                                |> Rule.withContextFromImportedModules
                                |> Rule.fromProjectRuleSchema
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "C"
                              , [ Review.Test.error
                                    { message = "A - B - C"
                                    , details = [ "details" ]
                                    , under = "module"
                                    }
                                ]
                              )
                            ]
            , test "should visit every file in a project rule, even when some of them are ignored" <|
                \() ->
                    [ """module A exposing (..)
a = 1"""
                    , """module B exposing (..)
import A
a = 1"""
                    , """module C exposing (..)
import A
a = 1"""
                    ]
                        |> Review.Test.runOnModules
                            (baseRule
                                |> Rule.withContextFromImportedModules
                                |> Rule.fromProjectRuleSchema
                                |> Rule.ignoreErrorsForFiles [ "src/File_1.elm" ]
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "C"
                              , [ Review.Test.error
                                    { message = "A - B - C"
                                    , details = [ "details" ]
                                    , under = "module"
                                    }
                                ]
                              )
                            ]
            ]
        , Test.describe "without using withContextFromImportedModules"
            [ test "should visit every file in a project rule" <|
                \() ->
                    [ """module A exposing (..)
a = 1"""
                    , """module B exposing (..)
import A
a = 1"""
                    , """module C exposing (..)
import A
a = 1"""
                    ]
                        |> Review.Test.runOnModules (Rule.fromProjectRuleSchema baseRule)
                        |> Review.Test.expectErrorsForModules
                            [ ( "C"
                              , [ Review.Test.error
                                    { message = "A - B - C"
                                    , details = [ "details" ]
                                    , under = "module"
                                    }
                                ]
                              )
                            ]
            , test "should visit every file in a project rule, even when some of them are ignored" <|
                \() ->
                    [ """module A exposing (..)
a = 1"""
                    , """module B exposing (..)
import A
a = 1"""
                    , """module C exposing (..)
import A
a = 1"""
                    ]
                        |> Review.Test.runOnModules
                            (baseRule
                                |> Rule.fromProjectRuleSchema
                                |> Rule.ignoreErrorsForFiles [ "src/File_1.elm" ]
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "C"
                              , [ Review.Test.error
                                    { message = "A - B - C"
                                    , details = [ "details" ]
                                    , under = "module"
                                    }
                                ]
                              )
                            ]
            ]
        ]
