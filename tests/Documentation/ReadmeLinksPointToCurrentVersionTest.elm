module Documentation.ReadmeLinksPointToCurrentVersionTest exposing (all)

import Documentation.ReadmeLinksPointToCurrentVersion exposing (rule)
import Elm.Project
import Json.Decode as Decode
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


packageElmJson : String -> String
packageElmJson name =
    """
{
    "type": "package",
    "name": \""""
        ++ name
        ++ """",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.2.3",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


message : String
message =
    "Link does not point to the current version of the package"


details : List String
details =
    [ "I suggest to run elm-review in fix mode" ]


readmeWithLink : String -> String
readmeWithLink link =
    """
# My project

  - [my project's thing](""" ++ link ++ """)
Don't report:
  - [this](https://package.elm-lang.org/packages/author/other-package/latest/Module-Name)
  - [this](https://package.elm-lang.org/packages/author/other-package/1.2.2/Module-Name)
  - [this](https://package.elm-lang.org/packages/other-author/package/latest/Module-Name)
  - [this](https://package.elm-lang.org/packages/other-author/package/1.2.4/Module-Name)
"""


all : Test
all =
    describe "ReadmeLinksPointToCurrentVersion"
        [ test "should not report an error if there is no elm.json file" <|
            \() ->
                Project.new
                    |> Project.addReadme { path = "README.md", content = readmeWithLink "https://package.elm-lang.org/packages/author/package/1.2.4/Module-Name" }
                    |> testRule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if there is no README file" <|
            \() ->
                Project.new
                    |> Project.addElmJson (createElmJson <| packageElmJson "author/package")
                    |> testRule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if all the links point to the current project use the correct version" <|
            \() ->
                Project.new
                    |> Project.addElmJson (createElmJson <| packageElmJson "author/package")
                    |> Project.addReadme { path = "README.md", content = readmeWithLink "https://package.elm-lang.org/packages/author/package/1.2.3/Module-Name" }
                    |> testRule
                    |> Review.Test.expectNoErrors
        , test "should report an error if a link points to a different version" <|
            \() ->
                Project.new
                    |> Project.addElmJson (createElmJson <| packageElmJson "author/package")
                    |> Project.addReadme { path = "README.md", content = readmeWithLink "https://package.elm-lang.org/packages/author/package/1.2.4/Module-Name" }
                    |> testRule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "https://package.elm-lang.org/packages/author/package/1.2.4/Module-Name"
                            }
                            |> Review.Test.whenFixed (readmeWithLink "https://package.elm-lang.org/packages/author/package/1.2.3/Module-Name")
                        ]
        , test "should report an error if a link points to latest" <|
            \() ->
                Project.new
                    |> Project.addElmJson (createElmJson <| packageElmJson "author/package")
                    |> Project.addReadme { path = "README.md", content = readmeWithLink "https://package.elm-lang.org/packages/author/package/latest/Module-Name" }
                    |> testRule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "https://package.elm-lang.org/packages/author/package/latest/Module-Name"
                            }
                            |> Review.Test.whenFixed (readmeWithLink "https://package.elm-lang.org/packages/author/package/1.2.3/Module-Name")
                        ]
        , test "should report an error even if the author or package name contains a dash or digit" <|
            \() ->
                Project.new
                    |> Project.addElmJson (createElmJson <| packageElmJson "au-tho5r/pack-age1")
                    |> Project.addReadme { path = "README.md", content = readmeWithLink "https://package.elm-lang.org/packages/au-tho5r/pack-age1/latest/Module-Name" }
                    |> testRule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "https://package.elm-lang.org/packages/au-tho5r/pack-age1/latest/Module-Name"
                            }
                            |> Review.Test.whenFixed (readmeWithLink "https://package.elm-lang.org/packages/au-tho5r/pack-age1/1.2.3/Module-Name")
                        ]
        ]
