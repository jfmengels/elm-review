module Review.Rule.WithArbitraryFilesVisitorTest exposing (all)

import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Rule.withArbitraryFilesVisitor"
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
                        createRule (Rule.withArbitraryFilesModuleVisitor [ "foo/some-file.css" ] arbitraryFilesModuleVisitor)
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
                        createRule (Rule.withArbitraryFilesModuleVisitor [ "foo/some-file.css" ] arbitraryFilesModuleVisitor)
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
                            (Rule.withArbitraryFilesModuleVisitor [ "a.txt", "c.txt" ] (reportsFileNames "A")
                                >> Rule.withArbitraryFilesModuleVisitor [ "b.txt" ] (reportsFileNames "B")
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


arbitraryFilesModuleVisitor : List { path : String, content : String } -> Context -> Context
arbitraryFilesModuleVisitor files context =
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
createProject arbitraryFiles =
    Project.addArbitraryFiles arbitraryFiles Project.new
