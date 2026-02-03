module Review.Rule.CacheTest exposing (all)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Expect
import Json.Encode as Encode
import NoUnused.Variables
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
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
module A exposing (value)
value = "string"
b = 1
"""
                                }
                            |> Project.addModule
                                { path = "B.elm"
                                , source = """
module B exposing (a)
import A exposing (..)
a = value
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
                    clearModuleDocsCache : Project -> Project
                    clearModuleDocsCache project_ =
                        case Valid.parse project_ of
                            Err error ->
                                Debug.todo ("Project could not be parsed: " ++ Debug.toString error)

                            Ok valid ->
                                valid
                                    |> Valid.clearElmDocsModuleFromProjectCacheTEST
                                    |> Valid.toRegularProject

                    rule : Rule
                    rule =
                        Rule.newModuleRuleSchemaUsingContextCreator "TestRule"
                            (Rule.initContextCreator (\lookup () -> lookup)
                                |> Rule.withModuleNameLookupTable
                            )
                            |> Rule.withExpressionEnterVisitor (\node lookup -> ( expressionVisitor node lookup, lookup ))
                            |> Rule.fromModuleRuleSchema

                    expressionVisitor : Node Expression -> ModuleNameLookupTable -> List (Rule.Error {})
                    expressionVisitor node lookup =
                        case Node.value node of
                            Expression.FunctionOrValue [] "value" ->
                                case ModuleNameLookupTable.moduleNameFor lookup node of
                                    Just [ "A" ] ->
                                        [ Rule.error
                                            { message = "Message"
                                            , details = [ "Details" ]
                                            }
                                            (Node.range node)
                                        ]

                                    _ ->
                                        []

                            _ ->
                                []

                    project : Project
                    project =
                        Review.Test.Dependencies.projectWithElmCore
                            |> Project.addModule
                                { path = "A.elm"
                                , source = """
module A exposing (value)
value = "string"
"""
                                }
                            |> Project.addModule
                                { path = "B.elm"
                                , source = """
module B exposing (a)
import A exposing (..)
a = value
"""
                                }

                    reviewResult : { errors : List Rule.ReviewError, rules : List Rule, project : Project, extracts : Dict String Encode.Value, fixedErrors : Dict String (List Rule.ReviewError) }
                    reviewResult =
                        Rule.reviewV3 Review.Options.defaults [ rule ] project

                    updatedProject : Project
                    updatedProject =
                        reviewResult.project
                            |> Project.addModule
                                { path = "B.elm"
                                , source = """
module B exposing (a)
import A exposing (..)
a = value ++ "ok"
"""
                                }
                            |> clearModuleDocsCache
                in
                Rule.reviewV3 Review.Options.defaults reviewResult.rules updatedProject
                    |> .errors
                    |> Expect.equal reviewResult.errors
        ]
