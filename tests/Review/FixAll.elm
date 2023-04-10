module Review.FixAll exposing (..)

import Dict exposing (Dict)
import Docs.UpToDateReadmeLinks
import Elm.Package
import Elm.Project
import Elm.Version
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import NoUnused.Dependencies
import NoUnused.Variables
import Review.Error exposing (ReviewError(..), Target(..))
import Review.Fix.Internal exposing (Fix(..))
import Review.Options
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Fix all"
        [ test "should not touch the project when fixes are disabled" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (a)
a = 1
b = 1
"""
                                }
                in
                Review.Options.withFixes Review.Options.fixedDisabled
                    |> runWithOptions NoUnused.Variables.rule project
                    |> .project
                    |> Project.modules
                    |> Expect.equal (Project.modules project)
        , test "should touch the project when running with fixes enabled without limit" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (a)
a = 1
b = 1
c = 1
"""
                                }

                    expectedProjectModules : List Project.ProjectModule
                    expectedProjectModules =
                        Project.new
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (a)
a = 1
"""
                                }
                            |> Project.modules

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Encode.Value }
                    results =
                        Review.Options.withFixes Review.Options.fixesEnabledWithoutLimits
                            |> runWithOptions NoUnused.Variables.rule project
                in
                Expect.all
                    [ \() ->
                        results.fixedErrors
                            |> Expect.equal
                                (Dict.fromList
                                    [ ( "A.elm"
                                      , [ ReviewError
                                            { message = "Top-level variable `c` is not used"
                                            , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                                            , filePath = "A.elm"
                                            , fixes = Review.Error.Available [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
                                            , preventsExtract = False
                                            , range = { end = { column = 2, row = 4 }, start = { column = 1, row = 4 } }
                                            , ruleName = "NoUnused.Variables"
                                            , target = Module
                                            }
                                        , ReviewError
                                            { message = "Top-level variable `b` is not used"
                                            , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                                            , filePath = "A.elm"
                                            , fixes = Review.Error.Available [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
                                            , preventsExtract = False
                                            , range = { end = { column = 2, row = 4 }, start = { column = 1, row = 4 } }
                                            , ruleName = "NoUnused.Variables"
                                            , target = Module
                                            }
                                        ]
                                      )
                                    ]
                                )
                    , \() ->
                        Project.modules results.project
                            |> Expect.equal expectedProjectModules
                    ]
                    ()
        , test "should touch the project when running with fixes enabled with a limit" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (a)
a = 1
b = 1
c = 1
d = 1
"""
                                }

                    expectedProjectModules : List Project.ProjectModule
                    expectedProjectModules =
                        Project.new
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (a)
a = 1
d = 1
"""
                                }
                            |> Project.modules

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Encode.Value }
                    results =
                        Review.Options.withFixes (Review.Options.fixesEnabledWithLimit 2)
                            |> runWithOptions NoUnused.Variables.rule project
                in
                Expect.all
                    [ \() ->
                        results.fixedErrors
                            |> Expect.equal
                                (Dict.fromList
                                    [ ( "A.elm"
                                      , [ ReviewError
                                            { message = "Top-level variable `c` is not used"
                                            , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                                            , filePath = "A.elm"
                                            , fixes = Review.Error.Available [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
                                            , preventsExtract = False
                                            , range = { end = { column = 2, row = 4 }, start = { column = 1, row = 4 } }
                                            , ruleName = "NoUnused.Variables"
                                            , target = Module
                                            }
                                        , ReviewError
                                            { message = "Top-level variable `b` is not used"
                                            , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                                            , filePath = "A.elm"
                                            , fixes = Review.Error.Available [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
                                            , preventsExtract = False
                                            , range = { end = { column = 2, row = 4 }, start = { column = 1, row = 4 } }
                                            , ruleName = "NoUnused.Variables"
                                            , target = Module
                                            }
                                        ]
                                      )
                                    ]
                                )
                    , \() ->
                        Project.modules results.project
                            |> Expect.equal expectedProjectModules
                    ]
                    ()
        , test "should apply fixes to elm.json" <|
            \() ->
                let
                    baseProject =
                        Project.new
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (a)
a = 1
"""
                                }

                    inputElmJson : { path : String, raw : String, project : Elm.Project.Project }
                    inputElmJson =
                        applicationElmJson
                            """ "elm/core": "1.0.0", "something/unused": "1.0.0" """

                    expectedElmJson : { path : String, raw : String, project : Elm.Project.Project }
                    expectedElmJson =
                        applicationElmJson
                            """ "elm/core": "1.0.0\""""

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Encode.Value }
                    results =
                        Review.Options.withFixes Review.Options.fixesEnabledWithoutLimits
                            |> runWithOptions NoUnused.Dependencies.rule (baseProject |> Project.addElmJson inputElmJson)
                in
                Expect.all
                    [ \() ->
                        results.fixedErrors
                            |> Expect.equal
                                (Dict.fromList
                                    [ ( "elm.json"
                                      , [ ReviewError
                                            { message = "Unused dependency `something/unused`"
                                            , details = [ "To remove it, I recommend running the following command:", "    elm-json uninstall something/unused" ]
                                            , filePath = "elm.json"
                                            , fixes = Review.Error.Available [ Replacement { end = { column = 1, row = 100000000 }, start = { column = 1, row = 1 } } """{
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
}
""" ]
                                            , preventsExtract = False
                                            , range = { end = { column = 51, row = 9 }, start = { column = 35, row = 9 } }
                                            , ruleName = "NoUnused.Dependencies"
                                            , target = ElmJson
                                            }
                                        ]
                                      )
                                    ]
                                )
                    , \() ->
                        Project.elmJson results.project
                            |> Expect.equal (Just expectedElmJson)
                    ]
                    ()
        , test "should stop applying fixes after the first change to elm.json, even without fix limits" <|
            \() ->
                let
                    baseProject =
                        Project.new
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (a)
a = 1
"""
                                }

                    inputElmJson : { path : String, raw : String, project : Elm.Project.Project }
                    inputElmJson =
                        applicationElmJson
                            """ "elm/core": "1.0.0", "something/unused": "1.0.0", "other/unused": "1.0.0" """

                    expectedElmJson : { path : String, raw : String, project : Elm.Project.Project }
                    expectedElmJson =
                        applicationElmJson
                            """ "elm/core": "1.0.0",
            "other/unused": "1.0.0\""""

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Encode.Value }
                    results =
                        Review.Options.withFixes Review.Options.fixesEnabledWithoutLimits
                            |> runWithOptions NoUnused.Dependencies.rule (baseProject |> Project.addElmJson inputElmJson)
                in
                Expect.all
                    [ \() ->
                        results.fixedErrors
                            |> Expect.equal
                                (Dict.fromList
                                    [ ( "elm.json"
                                      , [ ReviewError
                                            { message = "Unused dependency `something/unused`"
                                            , details =
                                                [ "To remove it, I recommend running the following command:"
                                                , "    elm-json uninstall something/unused"
                                                ]
                                            , filePath = "elm.json"
                                            , fixes = Review.Error.Available [ Replacement { end = { column = 1, row = 100000000 }, start = { column = 1, row = 1 } } """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "other/unused": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
""" ]
                                            , preventsExtract = False
                                            , range = { end = { column = 51, row = 9 }, start = { column = 35, row = 9 } }
                                            , ruleName = "NoUnused.Dependencies"
                                            , target = ElmJson
                                            }
                                        ]
                                      )
                                    ]
                                )
                    , \() ->
                        Project.elmJson results.project
                            |> Expect.equal (Just expectedElmJson)
                    ]
                    ()
        , test "should apply fixes for the README" <|
            \() ->
                let
                    baseProject : Project
                    baseProject =
                        Project.new
                            |> Project.addElmJson packageElmJson
                            |> Project.addModule
                                { path = "A.elm"
                                , source = "module A exposing (a)\na = 1"
                                }
                            |> Project.addReadme
                                { path = "README.md"
                                , content = "[link](https://package.elm-lang.org/packages/author/package/1.0.1/A)"
                                }

                    expectedReadme : { path : String, content : String }
                    expectedReadme =
                        { path = "README.md"
                        , content = "[link](https://package.elm-lang.org/packages/author/package/1.0.0/A/)"
                        }

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Encode.Value }
                    results =
                        Review.Options.withFixes Review.Options.fixesEnabledWithoutLimits
                            |> runWithOptions Docs.UpToDateReadmeLinks.rule baseProject
                in
                Expect.all
                    [ \() ->
                        results.fixedErrors
                            |> Expect.equal
                                (Dict.fromList
                                    [ ( "README.md"
                                      , [ ReviewError
                                            { message = "Link does not point to the current version of the package"
                                            , details = [ "I suggest to run elm-review --fix to get the correct link." ]
                                            , filePath = "README.md"
                                            , fixes = Review.Error.Available [ Replacement { end = { column = 68, row = 1 }, start = { column = 8, row = 1 } } "https://package.elm-lang.org/packages/author/package/1.0.0/A/" ]
                                            , preventsExtract = False
                                            , range = { end = { column = 68, row = 1 }, start = { column = 8, row = 1 } }
                                            , ruleName = "Docs.UpToDateReadmeLinks"
                                            , target = Readme
                                            }
                                        ]
                                      )
                                    ]
                                )
                    , \() ->
                        Project.readme results.project
                            |> Expect.equal (Just expectedReadme)
                    ]
                    ()
        ]


runWithOptions :
    Rule
    -> Project
    -> (Review.Options.ReviewOptions -> Review.Options.ReviewOptions)
    -> { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Encode.Value }
runWithOptions rule project buildOptions =
    Rule.reviewV3 (buildOptions Review.Options.defaults)
        [ rule ]
        project



-- Create elm.json


applicationElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
applicationElmJson depsDirectString =
    parseElmJson ("""{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
           """ ++ depsDirectString ++ """
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
""")


packageElmJson : { path : String, raw : String, project : Elm.Project.Project }
packageElmJson =
    parseElmJson """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "A"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


parseElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
parseElmJson raw =
    case Decode.decodeString Elm.Project.decoder raw of
        Ok project ->
            { path = "elm.json"
            , raw = raw
            , project = project
            }

        Err err ->
            Debug.todo ("Could not decode elm.json: " ++ Debug.toString err)


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            Debug.todo ("Package name `" ++ packageName ++ "` was not valid.")


unsafeElmVersion : String -> Elm.Version.Version
unsafeElmVersion versionString =
    case Elm.Version.fromString versionString of
        Just version ->
            version

        Nothing ->
            Debug.todo ("Package name `" ++ versionString ++ "` was not valid.")
