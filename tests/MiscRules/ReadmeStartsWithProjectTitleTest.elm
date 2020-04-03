module MiscRules.ReadmeStartsWithProjectTitleTest exposing (all)

import Elm.Project
import Json.Decode as Decode
import MiscRules.ReadmeStartsWithProjectTitle exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : Project -> ReviewResult
testRule project =
    """module SomeModule exposing (a)
a = 1"""
        |> Review.Test.runWithProjectData project rule


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = elmJson
            }

        Err _ ->
            Debug.todo "Invalid elm.json supplied to test"


packageElmJson : String
packageElmJson =
    """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "author/package-with-foo": "1.0.0 <= v < 2.0.0",
        "author/package-with-bar": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


all : Test
all =
    describe "ReadmeStartsWithProjectTitle"
        [ test "should not report an error if there is no elm.json file" <|
            \() ->
                Project.new
                    |> testRule
                    |> Review.Test.expectNoErrors
        , test "should report an error if the README.md file doesn't start with the project name" <|
            \() ->
                Project.new
                    |> Project.addElmJson (createElmJson packageElmJson)
                    |> Project.addReadme { path = "README.md", content = "Hello everybody\nThis is a good project" }
                    |> testRule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "TODO"
                            , details = [ "TODO" ]
                            , under = "Hello everybody"
                            }
                        ]
        , test "should not report an error if the README.md file starts with the project name" <|
            \() ->
                Project.new
                    |> Project.addElmJson (createElmJson packageElmJson)
                    |> Project.addReadme { path = "README.md", content = "# author/packagename\nHello everybody\nThis is a good project" }
                    |> testRule
                    |> Review.Test.expectNoErrors
        ]
