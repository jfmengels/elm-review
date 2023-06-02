module Docs.NoMissingChangelogEntryTest exposing (all)

import Docs.NoMissingChangelogEntry exposing (defaults, rule, withPathToChangelog)
import Elm.Project
import Json.Decode as Decode
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Docs.NoMissingChangelogEntry"
        [ test "should not report an error when the version of the package is found in the changelog" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.addExtraFiles
                            [ { path = "CHANGELOG.md"
                              , content = """
# Changelog
## 2.13.0
More stuff happened
## 2.12.0
Stuff happened
"""
                              }
                            ]
                            package
                in
                "module A exposing (..)\na = 1"
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should report an error when the version in the elm.json is not found in the changelog" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.addExtraFiles
                            [ { path = "CHANGELOG.md"
                              , content = """
# Changelog
## 1.13.0
More stuff happened
## 1.12.0
Stuff happened
"""
                              }
                            ]
                            package
                in
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Missing entry in CHANGELOG.md for version 2.13.0"
                          , details = [ "It seems you have or are ready to release a new version of your package, but forgot to include releases notes for it in your CHANGELOG.md file." ]
                          }
                        ]
        , test "should report an error when the changelog could not be found (default path)" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData package (rule defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find the CHANGELOG.md file"
                          , details =
                                [ "I was looking for the CHANGELOG.md file next to your project's elm.json file but couldn't find it. Please make sure that the spelling is correct."
                                , "If your changelog is named differently or is in a different location, then you can configure this rule to look for it in a different location:"
                                , """    config =
        [ Docs.NoMissingChangelogEntry.defaults
            |> Docs.NoMissingChangelogEntry.withPathToChangelog "path/to/your/changelog.md"
            |> Docs.NoMissingChangelogEntry.rule
        ]"""
                                , "Note that the path is relative your project's elm.json file."
                                ]
                          }
                        ]
        , test "should report an error when the changelog could not be found (custom path)" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData package (defaults |> withPathToChangelog "path/not-found.md" |> rule)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find the path/not-found.md changelog file"
                          , details =
                                [ "I was looking for the path/not-found.md changelog file but couldn't find it. Please make sure that the path you specified through Docs.NoMissingChangelogEntry.withPathToChangelog is correct."
                                , "Also note that the path you specify has to be relative to your project's elm.json file."
                                ]
                          }
                        ]
        ]


package : Project
package =
    case Decode.decodeString Elm.Project.decoder elmJson of
        Ok project ->
            Project.new
                |> Project.addElmJson
                    { path = "elm.json"
                    , raw = elmJson
                    , project = project
                    }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


elmJson : String
elmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "2.13.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""
