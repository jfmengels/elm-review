module Review.FixAll exposing (..)

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Version
import Expect
import Json.Encode
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

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Json.Encode.Value }
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
                                            , fixes = Just [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
                                            , preventsExtract = False
                                            , range = { end = { column = 2, row = 4 }, start = { column = 1, row = 4 } }
                                            , ruleName = "NoUnused.Variables"
                                            , target = Module
                                            }
                                        , ReviewError
                                            { message = "Top-level variable `b` is not used"
                                            , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                                            , filePath = "A.elm"
                                            , fixes = Just [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
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

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Json.Encode.Value }
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
                                            , fixes = Just [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
                                            , preventsExtract = False
                                            , range = { end = { column = 2, row = 4 }, start = { column = 1, row = 4 } }
                                            , ruleName = "NoUnused.Variables"
                                            , target = Module
                                            }
                                        , ReviewError
                                            { message = "Top-level variable `b` is not used"
                                            , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
                                            , filePath = "A.elm"
                                            , fixes = Just [ Removal { end = { column = 1, row = 5 }, start = { column = 1, row = 4 } } ]
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
                            """ "elm/core": "1.0.0",
"something/unused": "1.0.0" """
                            [ ( unsafePackageName "elm/core", Elm.Version.one )
                            , ( unsafePackageName "something/unused", Elm.Version.one )
                            ]

                    expectedElmJson : { path : String, raw : String, project : Elm.Project.Project }
                    expectedElmJson =
                        applicationElmJson
                            """ "elm/core": "1.0.0\""""
                            [ ( unsafePackageName "elm/core", Elm.Version.one )
                            ]

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Json.Encode.Value }
                    results =
                        Review.Options.withFixes Review.Options.fixesEnabledWithoutLimits
                            |> runWithOptions NoUnused.Dependencies.rule (baseProject |> Project.addElmJson inputElmJson)
                in
                Expect.all
                    [ \() ->
                        Project.elmJson results.project
                            |> Expect.equal (Just expectedElmJson)
                    ]
                    ()
        ]


runWithOptions :
    Rule
    -> Project
    -> (Review.Options.ReviewOptions -> Review.Options.ReviewOptions)
    -> { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Json.Encode.Value }
runWithOptions rule project buildOptions =
    Rule.reviewV3 (buildOptions Review.Options.defaults)
        [ rule ]
        project



-- Create elm.json


applicationElmJson : String -> List ( Elm.Package.Name, Elm.Version.Version ) -> { path : String, raw : String, project : Elm.Project.Project }
applicationElmJson depsDirectString depsDirect =
    { path = "elm.json"
    , raw = """{
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
"""
    , project =
        Elm.Project.Application
            { elm = unsafeElmVersion "0.19.1"
            , dirs = [ "src" ]
            , depsDirect = depsDirect
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
            Debug.todo ("Package name `" ++ packageName ++ "` was not valid.")


unsafeElmVersion : String -> Elm.Version.Version
unsafeElmVersion versionString =
    case Elm.Version.fromString versionString of
        Just version ->
            version

        Nothing ->
            Debug.todo ("Package name `" ++ versionString ++ "` was not valid.")
