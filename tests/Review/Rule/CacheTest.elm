module Review.Rule.CacheTest exposing (all)

import Dict exposing (Dict)
import Expect
import Json.Encode as Encode
import NoUnused.Variables
import Review.Options
import Review.Project as Project exposing (Project)
import Review.Project.Valid as Valid
import Review.Rule as Rule exposing (Rule)
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Cache"
        [ test "Running rules against the same project using the cache should yield the same result" <|
            \() ->
                let
                    project : Project
                    project =
                        Review.Test.Dependencies.projectWithElmCore
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (string)
string = "string"
b = 1
"""
                                }
                            |> Project.addModule
                                { path = "B.elm"
                                , source = """
module B exposing (a)
import A exposing (..)
a = string
"""
                                }

                    reviewResult : { errors : List Rule.ReviewError, rules : List Rule, project : Project, extracts : Dict String Encode.Value, fixedErrors : Dict String (List Rule.ReviewError) }
                    reviewResult =
                        Rule.reviewV3 Review.Options.defaults [ NoUnused.Variables.rule ] project
                in
                Rule.reviewV3 Review.Options.defaults reviewResult.rules reviewResult.project
                    |> .errors
                    |> Expect.equal reviewResult.errors
        , test "Changing a file after a run in an unrelated manner should yield the same results" <|
            \() ->
                let
                    project : Project
                    project =
                        Review.Test.Dependencies.projectWithElmCore
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (string)
string = "string"
"""
                                }
                            |> Project.addModule
                                { path = "B.elm"
                                , source = """
module B exposing (a)
import A exposing (..)
a = string
"""
                                }

                    reviewResult : { errors : List Rule.ReviewError, rules : List Rule, project : Project, extracts : Dict String Encode.Value, fixedErrors : Dict String (List Rule.ReviewError) }
                    reviewResult =
                        Rule.reviewV3 Review.Options.defaults [ NoUnused.Variables.rule ] project

                    updatedProject : Project
                    updatedProject =
                        reviewResult.project
                            |> Project.addModule
                                { path = "B.elm"
                                , source = """
module B exposing (a)
import A exposing (..)
a = string ++ "ok"
"""
                                }
                            |> clearModuleDocsCache
                in
                Rule.reviewV3 Review.Options.defaults reviewResult.rules updatedProject
                    |> .errors
                    |> Expect.equal reviewResult.errors
        ]


clearModuleDocsCache : Project -> Project
clearModuleDocsCache project =
    case Valid.parse project of
        Err error ->
            Debug.todo ("Project could not be parsed: " ++ Debug.toString error)

        Ok valid ->
            valid
                |> Valid.clearElmDocsModuleFromProjectCacheTEST
                |> Valid.toRegularProject
