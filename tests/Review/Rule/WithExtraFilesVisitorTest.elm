module Review.Rule.WithExtraFilesVisitorTest exposing (all)

import Dict exposing (Dict)
import Review.FilePattern as FilePattern
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
                            [ ( "foo/some-file.css", "#thing { color: red; }" )
                            ]

                    rule : Rule
                    rule =
                        createRule (Rule.withExtraFilesModuleVisitor extraFilesModuleVisitor [ FilePattern.include "foo/some-file.css" ])
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
                            [ ( "foo/some-file.css", "#thing { color: red; }" )
                            , ( "foo/some-other-file.css", "#thing { color: red; }" )
                            , ( "bar/some-file.css", "#thing { color: red; }" )
                            ]

                    rule : Rule
                    rule =
                        createRule (Rule.withExtraFilesModuleVisitor extraFilesModuleVisitor [ FilePattern.include "foo/some-file.css" ])
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
                            [ ( "a.txt", "A" )
                            , ( "b.txt", "B" )
                            , ( "c.txt", "C" )
                            ]

                    rule : Rule
                    rule =
                        createRule
                            (Rule.withExtraFilesModuleVisitor (reportsFileNames "A") [ FilePattern.include "a.txt", FilePattern.include "c.txt" ]
                                >> Rule.withExtraFilesModuleVisitor (reportsFileNames "B") [ FilePattern.include "b.txt" ]
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
                        (reportsFileNames "A")
                        [ FilePattern.include "** " ]
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


extraFilesModuleVisitor : Dict String String -> Context -> Context
extraFilesModuleVisitor files context =
    Dict.keys files ++ context


reportsFileNames : String -> Dict String String -> Context -> Context
reportsFileNames prefix files context =
    List.map (\path -> "Visitor " ++ prefix ++ " saw file " ++ path) (Dict.keys files) ++ context


finalEvaluation : Context -> List (Error scope)
finalEvaluation context =
    [ Rule.globalError
        { message = "Found these files"
        , details = context
        }
    ]


createProject : List ( String, String ) -> Project
createProject extraFiles =
    Project.addExtraFiles (Dict.fromList extraFiles) Project.new
