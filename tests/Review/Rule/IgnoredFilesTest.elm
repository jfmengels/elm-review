module Review.Rule.IgnoredFilesTest exposing (all)

import Elm.Syntax.Node as Node
import Review.Project
import Review.Rule as Rule exposing (Rule)
import Review.Test
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


all : Test
all =
    describe "should ignore files"
        [ test "Using ignoreErrorsForFiles" <|
            \() ->
                [ """module A exposing (a)
a = ()
""", """module B exposing (a)
a = ()
""", """module C exposing (a)
a = ()
""", """module D exposing (a)
a = ()
""" ]
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
        , test "Using expectErrorsForModules" <|
            \() ->
                [ """module A exposing (a)
a = ()
""" ]
                    |> Review.Test.runOnModulesWithProjectData
                        (Review.Project.new
                            |> Review.Project.addModule { path = "src/B.elm", source = """
module B exposing (a)
a = ()
""" }
                            |> Review.Project.addModule { path = "src-ignored/C.elm", source = """
module C exposing (a)
a = ()
""" }
                            |> Review.Project.addModule { path = "tests/D.elm", source = """
module D exposing (a)
a = ()
""" }
                            |> Review.Project.addModule { path = "tests/E.elm", source = """
module E exposing (a)
a = ()
""" }
                            |> Review.Project.addModule { path = "src-other-ignored/folder/F.elm", source = """
module F exposing (a)
a = ()
""" }
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
        , test "Using filterErrorsForFiles" <|
            \() ->
                [ """module A exposing (a)
a = ()
""", """module B exposing (a)
a = ()
""", """module C exposing (a)
a = ()
""", """module D exposing (a)
a = ()
""" ]
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
                [ """module A exposing (a)
a = ()
""", """module B exposing (a)
a = ()
""", """module C exposing (a)
a = ()
""", """module D exposing (a)
a = ()
""" ]
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
