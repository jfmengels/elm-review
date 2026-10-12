module TestProject exposing (application, lamderaApplication, package)

import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version
import Json.Decode as Decode
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test.Dependencies


application : Project
application =
    Project.new
        |> Project.addElmJson applicationElmJson


lamderaApplication : Project
lamderaApplication =
    Project.new
        |> Project.addElmJson lamderaApplicationElmJson


applicationElmJson : { path : String, raw : String, project : Elm.Project.Project }
applicationElmJson =
    { path = "elm.json"
    , raw = """{
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
    , project =
        Elm.Project.Application
            { elm = Elm.Version.one
            , dirs = []
            , depsDirect = [ ( unsafePackageName "elm/core", Elm.Version.one ) ]
            , depsIndirect = []
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }


lamderaApplicationElmJson : { path : String, raw : String, project : Elm.Project.Project }
lamderaApplicationElmJson =
    { path = "elm.json"
    , raw = """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "lamdera/core": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""
    , project =
        Elm.Project.Application
            { elm = Elm.Version.one
            , dirs = []
            , depsDirect =
                [ ( unsafePackageName "elm/core", Elm.Version.one )
                , ( unsafePackageName "lamdera/core", Elm.Version.one )
                ]
            , depsIndirect = []
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            Debug.todo ("Failed to convert " ++ packageName ++ " to a package name")


package : Project
package =
    Review.Test.Dependencies.projectWithElmCore
        |> Project.addElmJson (createPackageElmJson ())
        |> Project.addDependency elmExplorationsTestDependency


createPackageElmJson : () -> { path : String, raw : String, project : Elm.Project.Project }
createPackageElmJson () =
    case Decode.decodeString Elm.Project.decoder rawPackageElmJson of
        Ok elmJson_ ->
            { path = "elm.json"
            , raw = rawPackageElmJson
            , project = elmJson_
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


rawPackageElmJson : String
rawPackageElmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {},
    "test-dependencies": {
        "elm-explorations/test": "2.0.0 <= v < 3.0.0"
    }
}"""


elmExplorationsTestDependency : Dependency
elmExplorationsTestDependency =
    Dependency.create "elm-explorations/test"
        (createElmJson elmExplorationsTestElmJson)
        (case Decode.decodeString (Decode.list Elm.Docs.decoder) simplifiedElmExplorationsTestModules of
            Ok docs ->
                docs

            Err error ->
                Debug.todo ("Invalid docs.json contents in tests for simplifiedElmExplorationsTestModules: " ++ Debug.toString error)
        )


elmExplorationsTestElmJson : String
elmExplorationsTestElmJson =
    """{
           "type": "package",
           "name": "elm-explorations/test",
           "summary": "Write unit and fuzz tests for Elm code.",
           "license": "BSD-3-Clause",
           "version": "2.2.1",
           "exposed-modules": [
               "Test",
               "Test.Runner",
               "Test.Runner.Failure",
               "Test.Distribution",
               "Expect",
               "Fuzz",
               "Test.Html.Event",
               "Test.Html.Query",
               "Test.Html.Selector"
           ],
           "elm-version": "0.19.0 <= v < 0.20.0",
           "dependencies": {
               "elm/bytes": "1.0.8 <= v < 2.0.0",
               "elm/core": "1.0.0 <= v < 2.0.0",
               "elm/html": "1.0.0 <= v < 2.0.0",
               "elm/json": "1.0.0 <= v < 2.0.0",
               "elm/random": "1.0.0 <= v < 2.0.0",
               "elm/virtual-dom": "1.0.0 <= v < 2.0.0"
           },
           "test-dependencies": {}
       }
"""


createElmJson : String -> Elm.Project.Project
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            elmJson

        Err _ ->
            Debug.todo "Invalid elm.json contents in tests"


simplifiedElmExplorationsTestModules : String
simplifiedElmExplorationsTestModules =
    """[
  {
    "name": "Expect",
    "comment": "",
    "unions": [
      {
        "name": "Expectation",
        "comment": "",
        "args": [],
        "cases": [
          [
            "Expectation",
            []
          ]
        ]
      }
    ],
    "aliases": [],
    "values": [],
    "binops": []
  },
  {
    "name": "Test",
    "comment": "",
    "unions": [
      {
        "name": "Test",
        "comment": "",
        "args": [],
        "cases": [
          [
            "Test",
            []
          ]
        ]
      }],
    "aliases": [],
    "values": [
      {
        "name": "describe",
        "comment": "",
        "type": "String.String -> List.List Test.Test -> Test.Test"
      },
      {
        "name": "test",
        "comment": "",
        "type": "String.String -> (() -> Expect.Expectation) -> Test.Test"
      }
    ],
    "binops": []
  }
]
"""
