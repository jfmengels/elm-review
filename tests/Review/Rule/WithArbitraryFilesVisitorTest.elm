module Review.Rule.WithArbitraryFilesVisitorTest exposing (all)

import Elm.Syntax.Node as Node exposing (Node)
import Review.Project as Project
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Rule.withArbitraryFilesVisitor"
        [ test "passes the list of arbitrary files to the rule" <|
            \() ->
                let
                    arbitraryFiles : List { path : String, content : String }
                    arbitraryFiles =
                        [ { path = "foo/some-file.css", content = "#thing { color: red; }" }
                        ]
                in
                """module A exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData (Project.addArbitraryFiles arbitraryFiles Project.new) rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Found these files"
                          , details = [ "foo/some-file.css" ]
                          }
                        ]
        ]


type alias Context =
    List String


rule : Rule
rule =
    Rule.newModuleRuleSchema "WithCommentsVisitorTestRule" []
        |> Rule.withArbitraryFilesModuleVisitor arbitraryFilesModuleVisitor
        |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ))
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


arbitraryFilesModuleVisitor : List { path : String, content : String } -> Context -> Context
arbitraryFilesModuleVisitor files context =
    List.map .path files ++ context


finalEvaluation : Context -> List (Error scope)
finalEvaluation context =
    [ Rule.globalError
        { message = "Found these files"
        , details = context
        }
    ]
