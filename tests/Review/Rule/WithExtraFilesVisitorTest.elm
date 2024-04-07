module Review.Rule.WithExtraFilesVisitorTest exposing (all)

import Review.FilePattern as FilePattern exposing (FilePattern)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Rule.withExtraFilesVisitor"
        [ test "passes the list of arbitrary files to the rule" <|
            \() ->
                let
                    project : Project
                    project =
                        createProject
                            [ { path = "foo/some-file.css", content = "#thing { color: red; }" }
                            ]

                    rule : Rule
                    rule =
                        createRule (Rule.withExtraFilesModuleVisitor [ FilePattern.include "foo/some-file.css" ] extraFilesModuleVisitor)
                in
                """module A exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Found these files"
                          , details = [ "foo/some-file.css" ]
                          }
                        ]
        , test "filters out files that were not requested" <|
            \() ->
                let
                    project : Project
                    project =
                        createProject
                            [ { path = "foo/some-file.css", content = "#thing { color: red; }" }
                            , { path = "foo/some-other-file.css", content = "#thing { color: red; }" }
                            , { path = "bar/some-file.css", content = "#thing { color: red; }" }
                            ]

                    rule : Rule
                    rule =
                        createRule (Rule.withExtraFilesModuleVisitor [ FilePattern.include "foo/some-file.css" ] extraFilesModuleVisitor)
                in
                """module A exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Found these files"
                          , details = [ "foo/some-file.css" ]
                          }
                        ]
        , test "visitors should only have access to files they requested" <|
            \() ->
                let
                    project : Project
                    project =
                        createProject
                            [ { path = "a.txt", content = "A" }
                            , { path = "b.txt", content = "B" }
                            , { path = "c.txt", content = "C" }
                            ]

                    rule : Rule
                    rule =
                        createRule
                            (Rule.withExtraFilesModuleVisitor [ FilePattern.include "a.txt", FilePattern.include "c.txt" ] (reportsFileNames "A")
                                >> Rule.withExtraFilesModuleVisitor [ FilePattern.include "b.txt" ] (reportsFileNames "B")
                            )
                in
                """module A exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Found these files"
                          , details =
                                [ "Visitor B saw file b.txt"
                                , "Visitor A saw file a.txt"
                                , "Visitor A saw file c.txt"
                                ]
                          }
                        ]
        , test "Requesting invalid globs should result in a configuration error" <|
            \() ->
                createRule
                    (Rule.withExtraFilesModuleVisitor
                        [ FilePattern.include "** " ]
                        (reportsFileNames "A")
                    )
                    |> Review.Test.expectConfigurationError
                        { message = "Invalid globs provided when requesting extra files"
                        , details =
                            [ "This rule requested additional files, but did so by specifying globs that I could not make sense of:"
                            , "  1. ** "
                            ]
                        }
        ]


type alias Context =
    List String


createRule : (Rule.ModuleRuleSchema { canCollectProjectData : () } (List a) -> Rule.ModuleRuleSchema schemaState Context) -> Rule
createRule modifier =
    Rule.newModuleRuleSchema "WithCommentsVisitorTestRule" []
        |> modifier
        |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ))
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


extraFilesModuleVisitor : List { path : String, content : String } -> Context -> Context
extraFilesModuleVisitor files context =
    List.map .path files ++ context


reportsFileNames : String -> List { path : String, content : String } -> Context -> Context
reportsFileNames prefix files context =
    List.map (\file -> "Visitor " ++ prefix ++ " saw file " ++ file.path) files ++ context


finalEvaluation : Context -> List (Error scope)
finalEvaluation context =
    [ Rule.globalError
        { message = "Found these files"
        , details = context
        }
    ]


createProject : List { path : String, content : String } -> Project
createProject extraFiles =
    Project.addExtraFiles extraFiles Project.new
