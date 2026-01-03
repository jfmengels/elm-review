module Review.Rule.IgnoredFilesTest exposing (ignoreFilesTests, isFileIgnoredTests)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Json.Encode as Encode
import Review.FilePattern as FilePattern
import Review.Project
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Set exposing (Set)
import Test exposing (Test, describe, test)


moduleRule : Rule
moduleRule =
    Rule.newModuleRuleSchema "TestRule" ()
        |> Rule.withSimpleExpressionVisitor
            (\node ->
                [ Rule.error
                    { message = "Error reported"
                    , details = [ "No details" ]
                    }
                    (Node.range node)
                ]
            )
        |> Rule.fromModuleRuleSchema


projectRule : Rule
projectRule =
    Rule.newProjectRuleSchema "TestRule" ()
        |> Rule.withModuleVisitor
            (\schema ->
                schema
                    |> Rule.withSimpleExpressionVisitor
                        (\node ->
                            [ Rule.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                }
                                (Node.range node)
                            ]
                        )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator identity
            , fromModuleToProject = Rule.initContextCreator identity
            , foldProjectContexts = \_ b -> b
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


ignoreFilesTests : Test
ignoreFilesTests =
    describe "should ignore files"
        [ test "Using ignoreErrorsForFiles" <|
            \() ->
                [ moduleSrc "A", moduleSrc "B", moduleSrc "C", moduleSrc "D" ]
                    |> Review.Test.runOnModules
                        (moduleRule
                            |> Rule.ignoreErrorsForFiles [ "src/A.elm", "src/B.elm" ]
                            |> Rule.ignoreErrorsForFiles [ "src/C.elm" ]
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "D"
                          , [ Review.Test.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                , under = "()"
                                }
                            ]
                          )
                        ]
        , test "Using ignoreErrorsForDirectories" <|
            \() ->
                moduleSrc "A"
                    |> Review.Test.runWithProjectData
                        (Review.Project.new
                            |> Review.Project.addModule { path = "src/B.elm", source = moduleSrc "B" }
                            |> Review.Project.addModule { path = "src-ignored/C.elm", source = moduleSrc "C" }
                            |> Review.Project.addModule { path = "tests/D.elm", source = moduleSrc "D" }
                            |> Review.Project.addModule { path = "tests/E.elm", source = moduleSrc "E" }
                            |> Review.Project.addModule { path = "src-other-ignored/folder/F.elm", source = moduleSrc "F" }
                        )
                        (moduleRule
                            |> Rule.ignoreErrorsForDirectories [ "src-ignored/", "tests" ]
                            |> Rule.ignoreErrorsForDirectories [ "src-other-ignored\\folder\\" ]
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                , under = "()"
                                }
                            ]
                          )
                        , ( "B"
                          , [ Review.Test.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                , under = "()"
                                }
                            ]
                          )
                        ]
        , test "Using ignoreErrorsFor" <|
            \() ->
                moduleSrc "A"
                    |> Review.Test.runWithProjectData
                        (Review.Project.new
                            |> Review.Project.addModule { path = "src/B.elm", source = moduleSrc "B" }
                            |> Review.Project.addModule { path = "src-ignored/C.elm", source = moduleSrc "C" }
                            |> Review.Project.addModule { path = "tests/D.elm", source = moduleSrc "D" }
                            |> Review.Project.addModule { path = "tests/E.elm", source = moduleSrc "E" }
                            |> Review.Project.addModule { path = "src-other-ignored/folder/F.elm", source = moduleSrc "F" }
                        )
                        (moduleRule
                            |> Rule.ignoreErrorsFor
                                [ FilePattern.excludeDirectory "src-ignored/"
                                , FilePattern.excludeDirectory "tests"
                                ]
                            |> Rule.ignoreErrorsFor [ FilePattern.excludeDirectory "src-other-ignored/folder/" ]
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                , under = "()"
                                }
                            ]
                          )
                        , ( "B"
                          , [ Review.Test.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                , under = "()"
                                }
                            ]
                          )
                        ]
        , test "Using filterErrorsForFiles" <|
            \() ->
                [ moduleSrc "A", moduleSrc "B", moduleSrc "C", moduleSrc "D" ]
                    |> Review.Test.runOnModules
                        (moduleRule
                            |> Rule.filterErrorsForFiles (\path -> String.contains "C" path)
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "C"
                          , [ Review.Test.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                , under = "()"
                                }
                            ]
                          )
                        ]
        , test "for a projectRule" <|
            \() ->
                [ moduleSrc "A", moduleSrc "B", moduleSrc "C", moduleSrc "D" ]
                    |> Review.Test.runOnModules
                        (projectRule
                            |> Rule.ignoreErrorsForFiles [ "src/A.elm", "src/B.elm" ]
                            |> Rule.ignoreErrorsForFiles [ "src/C.elm" ]
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "D"
                          , [ Review.Test.error
                                { message = "Error reported"
                                , details = [ "No details" ]
                                , under = "()"
                                }
                            ]
                          )
                        ]
        ]


isFileIgnoredTests : Test
isFileIgnoredTests =
    test "Rule.withIsFileIgnored" <|
        \() ->
            moduleSrc "A"
                |> Review.Test.runWithProjectData
                    (Review.Project.new
                        |> Review.Project.addModule { path = "src/B.elm", source = moduleSrc "B" }
                        |> Review.Project.addModule { path = "src-ignored/C.elm", source = moduleSrc "C" }
                        |> Review.Project.addModule { path = "tests/D.elm", source = moduleSrc "D" }
                        |> Review.Project.addModule { path = "tests/E.elm", source = moduleSrc "E" }
                        |> Review.Project.addModule { path = "src-other-ignored/folder/F.elm", source = moduleSrc "F" }
                    )
                    (ruleThatListsIgnoredFiles
                        |> Rule.ignoreErrorsForDirectories [ "src-ignored/", "tests" ]
                        |> Rule.ignoreErrorsForDirectories [ "src-other-ignored\\folder\\" ]
                    )
                |> Review.Test.expectDataExtract """["C", "D", "E", "F"]"""


ruleThatListsIgnoredFiles : Rule
ruleThatListsIgnoredFiles =
    Rule.newProjectRuleSchema "ListIgnoredFiles" Set.empty
        |> Rule.withModuleVisitor (Rule.withSimpleExpressionVisitor (always []))
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator (\_ -> ())
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = Set.union
            }
        |> Rule.withDataExtractor (\set -> set |> Encode.set (String.join "." >> Encode.string))
        |> Rule.fromProjectRuleSchema


fromModuleToProject : Rule.ContextCreator () (Set ModuleName)
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName isIgnored () ->
            if isIgnored then
                Set.singleton moduleName

            else
                Set.empty
        )
        |> Rule.withModuleName
        |> Rule.withIsFileIgnored


moduleSrc : String -> String
moduleSrc moduleName =
    "module " ++ moduleName ++ """ exposing (a)
a = ()
"""
