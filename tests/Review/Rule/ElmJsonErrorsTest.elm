module Review.Rule.ElmJsonErrorsTest exposing (all)

import Elm.Package
import Elm.Project
import Elm.Version
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


type alias Context =
    Maybe Rule.ElmJsonKey


rule : Rule
rule =
    Rule.newProjectRuleSchema "TestRule" initialProjectContext
        |> Rule.withElmJsonProjectVisitor (\elmJson _ -> ( [], elmJson |> Maybe.map .elmJsonKey ))
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.fromProjectRuleSchema


initialProjectContext : Context
initialProjectContext =
    Nothing


finalEvaluationForProject : Context -> List (Error { useErrorForModule : () })
finalEvaluationForProject maybeElmJsonKey =
    case maybeElmJsonKey of
        Just elmJsonKey ->
            [ Rule.errorForElmJson elmJsonKey
                (\_ ->
                    { message = "Error for elm.json"
                    , details = [ "This is an elm.json error" ]
                    , range = { start = { row = 2, column = 5 }, end = { row = 2, column = 27 } }
                    }
                )
            ]

        Nothing ->
            []


sourceCode : String
sourceCode =
    """module A exposing (..)
import B
a = 1
"""


project : Project
project =
    Project.new
        |> Project.addElmJson applicationElmJson


elmCore : () -> Elm.Package.Name
elmCore () =
    case Elm.Package.fromString "elm/core" of
        Just name ->
            name

        Nothing ->
            elmCore ()


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
            , depsDirect = [ ( elmCore (), Elm.Version.one ) ]
            , depsIndirect = []
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }


all : Test
all =
    Test.describe "Creating errors for `elm.json`"
        [ test "should not pass the elmJsonKey if the `elm.json` file does not exist" <|
            \() ->
                sourceCode
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should pass the elmJsonKey if the `elm.json` file exists" <|
            \() ->
                sourceCode
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "elm.json"
                          , [ Review.Test.error
                                { message = "Error for elm.json"
                                , details = [ "This is an elm.json error" ]
                                , under = "\"type\": \"application\","
                                }
                            ]
                          )
                        ]
        ]
