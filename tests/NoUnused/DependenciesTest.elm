module NoUnused.DependenciesTest exposing (all)

import Elm.Docs
import Elm.Project
import Json.Decode as Decode
import NoUnused.Dependencies exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test
import Test exposing (Test, describe, test)


createProject : String -> Project
createProject rawElmJson =
    Project.new
        |> Project.addElmJson (createElmJson rawElmJson)
        |> Project.addDependency packageWithFoo
        |> Project.addDependency packageWithBar


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


applicationElmJson : String
applicationElmJson =
    """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "author/package-with-foo": "1.0.0",
            "author/package-with-bar": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""


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


packageWithFoo : Dependency
packageWithFoo =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "Foo"
              , comment = ""
              , unions = []
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            .project <| createElmJson """
  {
      "type": "package",
      "name": "author/package-with-foo",
      "summary": "Summary",
      "license": "BSD-3-Clause",
      "version": "1.0.0",
      "exposed-modules": [
          "Foo"
      ],
      "elm-version": "0.19.0 <= v < 0.20.0",
      "dependencies": {
          "elm/core": "1.0.0 <= v < 2.0.0"
      },
      "test-dependencies": {}
  }"""
    in
    Dependency.create
        "author/package-with-foo"
        elmJson
        modules


packageWithBar : Dependency
packageWithBar =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "Bar"
              , comment = ""
              , unions = []
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            .project <| createElmJson """
{
  "type": "package",
  "name": "author/package-with-bar",
  "summary": "Summary",
  "license": "BSD-3-Clause",
  "version": "1.0.0",
  "exposed-modules": [
      "Bar"
  ],
  "elm-version": "0.19.0 <= v < 0.20.0",
  "dependencies": {
      "elm/core": "1.0.0 <= v < 2.0.0"
  },
  "test-dependencies": {}
}"""
    in
    Dependency.create
        "author/package-with-bar"
        elmJson
        modules


all : Test
all =
    describe "NoUnused.Dependencies"
        [ test "should not report anything if there is no `elm.json` file" <|
            \() ->
                """
module A exposing (a)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report unused dependencies for an application when none of their modules are imported" <|
            \() ->
                """
module A exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData (createProject applicationElmJson) rule
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "Unused dependency `author/package-with-bar`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-bar"
                                ]
                            , under = "author/package-with-bar"
                            }
                        , Review.Test.error
                            { message = "Unused dependency `author/package-with-foo`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-foo"
                                ]
                            , under = "author/package-with-foo"
                            }
                        ]
        , test "should not report dependencies for an application whose modules are imported" <|
            \() ->
                """
module A exposing (a)
import Foo
import Bar
a = 1
"""
                    |> Review.Test.runWithProjectData (createProject applicationElmJson) rule
                    |> Review.Test.expectNoErrors
        , test "should report unused dependencies for a package when none of their modules are imported" <|
            \() ->
                """
module A exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData (createProject packageElmJson) rule
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "Unused dependency `author/package-with-bar`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-bar"
                                ]
                            , under = "author/package-with-bar"
                            }
                        , Review.Test.error
                            { message = "Unused dependency `author/package-with-foo`"
                            , details =
                                [ "To remove it, I recommend running the following command:"
                                , "    elm-json uninstall author/package-with-foo"
                                ]
                            , under = "author/package-with-foo"
                            }
                        ]
        , test "should not report dependencies for a package whose modules are imported" <|
            \() ->
                """
module A exposing (a)
import Foo
import Bar
a = 1
"""
                    |> Review.Test.runWithProjectData (createProject packageElmJson) rule
                    |> Review.Test.expectNoErrors
        ]
