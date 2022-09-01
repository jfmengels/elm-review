module Review.Rule.DataExtractTest exposing (all)

import Elm.Package
import Elm.Project
import Elm.Version
import Json.Encode as Encode
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


type alias Context =
    ()


rule : Rule
rule =
    Rule.newProjectRuleSchema "TestRule" ()
        |> Rule.withElmJsonProjectVisitor (\_ context -> ( [], context ))
        |> Rule.withDataExtractor (dataExtractor >> Just)
        |> Rule.fromProjectRuleSchema


dataExtractor : Context -> Encode.Value
dataExtractor () =
    Encode.object
        [ ( "foo", Encode.string "bar" )
        , ( "other", Encode.list Encode.int [ 1, 2, 3 ] )
        , ( "null", Encode.null )
        ]


sourceCode : String
sourceCode =
    """module A exposing (..)
import B
a = 1
"""


all : Test
all =
    Test.describe "Extract data"
        [ test "should not pass the elmJsonKey if the `elm.json` file does not exist" <|
            \() ->
                sourceCode
                    |> Review.Test.run rule
                    -- Bad formatting is on purpose
                    |> Review.Test.expectDataExtract (Just """{
        "foo": "bar",
                    "other": [ 1, 2, 3 ],"null": null
}""")
        ]
