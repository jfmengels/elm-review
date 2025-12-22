module TestProject exposing (application, lamderaApplication, package)

import Elm.Package
import Elm.Project
import Elm.Version
import Json.Decode as Decode
import Review.Project as Project exposing (Project)


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
    Project.new
        |> Project.addElmJson (createPackageElmJson ())


createPackageElmJson : () -> { path : String, raw : String, project : Elm.Project.Project }
createPackageElmJson () =
    case Decode.decodeString Elm.Project.decoder rawPackageElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawPackageElmJson
            , project = elmJson
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
    "test-dependencies": {}
}"""
