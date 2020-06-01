module NoUnapprovedLicenseTest exposing (all)

import Elm.Docs
import Elm.Project
import Json.Decode as Decode
import NoUnapprovedLicense exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test
import Test exposing (Test, describe, test)


createProject : String -> Project
createProject license =
    Project.new
        |> Project.addElmJson (createElmJson applicationElmJson)
        |> Project.addDependency (dependency license)


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
            "author/dependency": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""


dependency : String -> Dependency
dependency license =
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

        rawElmJson : String
        rawElmJson =
            """
{
"type": "package",
"name": "author/dependency",
"summary": "Summary",
"license": \"""" ++ license ++ """",
"version": "1.0.0",
"exposed-modules": [
  "Foo"
],
"elm-version": "0.19.0 <= v < 0.20.0",
"dependencies": {
  "elm/core": "1.0.0 <= v < 2.0.0"
},
"test-dependencies": {}
}
"""

        elmJson : Elm.Project.Project
        elmJson =
            createElmJson rawElmJson
                |> .project
    in
    Dependency.create
        "author/dependency"
        elmJson
        modules


sourceCode : String
sourceCode =
    """
module A exposing (a)
a = 1
"""


all : Test
all =
    describe "NoUnapprovedLicense"
        [ test "should not report anything if there is no `elm.json` file" <|
            \() ->
                sourceCode
                    |> Review.Test.run (rule { allowed = [], forbidden = [] })
                    |> Review.Test.expectNoErrors
        , test "should not report anything if all dependencies have a license that is allowed" <|
            \() ->
                sourceCode
                    |> Review.Test.runWithProjectData (createProject "MIT") (rule { allowed = [ "MIT" ], forbidden = [] })
                    |> Review.Test.expectNoErrors
        , test "should report an error if a dependency has an unknown license" <|
            \() ->
                sourceCode
                    |> Review.Test.runWithProjectData (createProject "BSD-3-Clause") (rule { allowed = [ "MIT" ], forbidden = [] })
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "Unknown license `BSD-3-Clause` for dependency `author/dependency`"
                            , details =
                                [ "Talk to your legal team and see if this license is allowed. If it is allowed, add it to the list of allowed licenses. Otherwise, add it to the list of forbidden licenses and remove this dependency."
                                , "More info about licenses at https://spdx.org/licenses."
                                ]
                            , under = "author/dependency"
                            }
                        ]
        , test "should report an error if a dependency has a forbidden license" <|
            \() ->
                sourceCode
                    |> Review.Test.runWithProjectData (createProject "BSD-3-Clause") (rule { allowed = [ "MIT" ], forbidden = [ "BSD-3-Clause" ] })
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "Forbidden license `BSD-3-Clause` for dependency `author/dependency`"
                            , details = [ "This license has been marked as forbidden and you should therefore not use this package." ]
                            , under = "author/dependency"
                            }
                        ]
        ]
