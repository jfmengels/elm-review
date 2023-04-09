module Review.FixAll exposing (..)

import Dict exposing (Dict)
import Expect
import Json.Encode
import NoUnused.Variables
import Review.Options
import Review.Project as Project exposing (Project)
import Review.Rule as Rule
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Fix all"
        [ test "should not touch the project when running with fixAll set to False" <|
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
                    |> runWithOptions project
                    |> .project
                    |> Project.modules
                    |> Expect.equal (Project.modules project)
        , test "should touch the project when running with fixAll set to True" <|
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

                    results : { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Json.Encode.Value }
                    results =
                        Review.Options.withFixes Review.Options.fixesEnabledWithoutLimits
                            |> runWithOptions project
                in
                Expect.all
                    [ \() ->
                        results
                            |> .project
                            |> Project.modules
                            |> Expect.equal
                                (Project.new
                                    |> Project.addModule
                                        { path = "A.elm"
                                        , source = """
module A exposing (a)
a = 1
"""
                                        }
                                    |> Project.modules
                                )
                    ]
                    ()
        ]


runWithOptions :
    Project
    -> (Review.Options.ReviewOptions -> Review.Options.ReviewOptions)
    -> { errors : List Rule.ReviewError, fixedErrors : Dict String (List Rule.ReviewError), rules : List Rule.Rule, project : Project, extracts : Dict String Json.Encode.Value }
runWithOptions project buildOptions =
    Rule.reviewV3 (buildOptions Review.Options.defaults)
        [ NoUnused.Variables.rule ]
        project
