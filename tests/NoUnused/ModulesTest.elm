module NoUnused.ModulesTest exposing (all)

import Elm.Project
import Elm.Version
import Json.Decode as Decode
import NoUnused.Modules exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


application : Project
application =
    Project.new
        |> Project.addElmJson applicationElmJson


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
            "elm/core": "1.0.2"
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
            , depsDirect = []
            , depsIndirect = []
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }


package_ : Project
package_ =
    Project.new
        |> Project.addElmJson (createPackageElmJson ())


createPackageElmJson : () -> { path : String, raw : String, project : Elm.Project.Project }
createPackageElmJson _ =
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


details : List String
details =
    [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
    ]


tests : List Test
tests =
    [ test "should not report a module when all modules are used" <|
        \() ->
            [ """
module NotReported exposing (..)
import OtherModule
main = text ""
"""
            , """
module OtherModule exposing (..)
a = 1
"""
            ]
                |> Review.Test.runOnModulesWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should report a module when it is never used" <|
        \() ->
            [ """
module NotReported exposing (..)
main = text ""
"""
            , """
module Reported exposing (..)
a = 1
"""
            , """
module Other.Reported exposing (..)
a = 1
"""
            ]
                |> Review.Test.runOnModulesWithProjectData application rule
                |> Review.Test.expectErrorsForModules
                    [ ( "Reported"
                      , [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = details
                            , under = "Reported"
                            }
                        ]
                      )
                    , ( "Other.Reported"
                      , [ Review.Test.error
                            { message = "Module `Other.Reported` is never used."
                            , details = details
                            , under = "Other.Reported"
                            }
                        ]
                      )
                    ]
    , test "should report a module even if it is the only module in the project" <|
        \() ->
            """
module Reported exposing (..)
import Something
a = 1
"""
                |> Review.Test.runWithProjectData application rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `Reported` is never used."
                        , details = details
                        , under = "Reported"
                        }
                    ]
    , test "should not report an application module if it exposes a main function" <|
        \() ->
            """
module NotReported exposing (..)
main = text ""
"""
                |> Review.Test.runWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should not report an application module if it contains a main function even if it is not exposed" <|
        \() ->
            """
module NotReported exposing (a)
main = text ""
a = 1
"""
                |> Review.Test.runWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should not report a module with main function if we don't know the project type" <|
        \() ->
            """
module NotReported exposing (a)
main = text ""
a = 1
"""
                |> Review.Test.runWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should not report a module if it imports `Test`" <|
        \() ->
            """
module NotReported exposing (..)
import Test
a = 1
"""
                |> Review.Test.runWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should not report the `ReviewConfig` module" <|
        \() ->
            """
module ReviewConfig exposing (config)
config = []
"""
                |> Review.Test.runWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should not report modules exposed in a package" <|
        \() ->
            """
module Exposed exposing (..)
a = 1
"""
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectNoErrors
    , test "should report non-exposed and non-used modules from a package" <|
        \() ->
            """
module NotExposed exposing (..)
a = 1
"""
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `NotExposed` is never used."
                        , details = details
                        , under = "NotExposed"
                        }
                    ]
    , test "should report non-exposed and non-used package modules that expose a `main` function" <|
        \() ->
            """
module Reported exposing (main)
main = text ""
"""
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `Reported` is never used."
                        , details = details
                        , under = "Reported"
                        }
                    ]
    , test "should report non-exposed and non-used package modules that define a `main` function" <|
        \() ->
            """
module Reported exposing (a)
main = text ""
a = 1
"""
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `Reported` is never used."
                        , details = details
                        , under = "Reported"
                        }
                    ]
    ]


all : Test
all =
    describe "NoUnusedModules" tests
