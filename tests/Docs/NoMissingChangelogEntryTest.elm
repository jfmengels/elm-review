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
                            (package "2.13.0")
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
                            (package "2.13.0")
                in
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "CHANGELOG.md"
                          , [ Review.Test.error
                                { message = "Missing entry in CHANGELOG.md for version 2.13.0"
                                , details = [ "It seems you have or are ready to release a new version of your package, but forgot to include releases notes for it in your CHANGELOG.md file." ]
                                , under = "# Cha"
                                }
                            ]
                          )
                        ]
        , test "should report an error when the changelog could not be found (default path)" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData (package "2.13.0") (rule defaults)
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
                    |> Review.Test.runWithProjectData (package "2.13.0") (defaults |> withPathToChangelog "path/not-found.md" |> rule)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find the path/not-found.md changelog file"
                          , details =
                                [ "I was looking for the path/not-found.md changelog file but couldn't find it. Please make sure that the path you specified through Docs.NoMissingChangelogEntry.withPathToChangelog is correct."
                                , "Also note that the path you specify has to be relative to your project's elm.json file."
                                ]
                          }
                        ]
        , test "should report an error when the project is an application" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.addExtraFiles
                            [ { path = "CHANGELOG.md"
                              , content = "# something"
                              }
                            ]
                            application
                in
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "The Elm project is unexpectedly an application"
                          , details = [ "This rule only supports Elm packages, but doesn't support Elm applications as they don't have a version number. I recommend that you remove this rule from your review configuration." ]
                          }
                        ]
        , test "should not report an error when the version is 1.0.0 (no changelog)" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData (package "1.0.0") (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the version is 1.0.0 (empty changelog)" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.addExtraFiles
                            [ { path = "CHANGELOG.md"
                              , content = ""
                              }
                            ]
                            (package "1.0.0")
                in
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


package : String -> Project
package version =
    let
        raw : String
        raw =
            packageElmJson version
    in
    case Decode.decodeString Elm.Project.decoder raw of
        Ok project ->
            Project.new
                |> Project.addElmJson
                    { path = "elm.json"
                    , raw = raw
                    , project = project
                    }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


packageElmJson : String -> String
packageElmJson version =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": \"""" ++ version ++ """",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


application : Project
application =
    case Decode.decodeString Elm.Project.decoder applicationElmJson of
        Ok project ->
            Project.new
                |> Project.addElmJson
                    { path = "elm.json"
                    , raw = applicationElmJson
                    , project = project
                    }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


applicationElmJson : String
applicationElmJson =
    """{
   "type": "application",
   "source-directories": [
       "src"
   ],
   "elm-version": "0.19.1",
   "dependencies": {
       "direct": {
           "elm/core": "1.0.0"
       },
       "indirect": {}
   },
   "test-dependencies": {
       "direct": {},
       "indirect": {}
   }
}"""
