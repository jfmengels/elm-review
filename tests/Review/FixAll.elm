module Review.FixAll exposing (..)

import Expect
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
                Rule.reviewV3 (Review.Options.defaults |> Review.Options.withFixes False)
                    [ NoUnused.Variables.rule ]
                    project
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
"""
                                }
                in
                Rule.reviewV3 (Review.Options.defaults |> Review.Options.withFixes True)
                    [ NoUnused.Variables.rule ]
                    project
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
