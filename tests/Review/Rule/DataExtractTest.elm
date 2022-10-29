module Review.Rule.DataExtractTest exposing (all)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Encode as Encode
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, test)


type alias ProjectContext =
    Dict String (List String)


type alias ModuleContext =
    { declarations : List String
    }


rule : Maybe (Rule.Error { useErrorForModule : () }) -> Rule
rule maybeError =
    Rule.newProjectRuleSchema "TestRule" Dict.empty
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation
            (\_ ->
                case maybeError of
                    Just error ->
                        [ error ]

                    Nothing ->
                        []
            )
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor (\node context -> ( [], declarationListVisitor node context ))


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\_ ->
            { declarations = []
            }
        )


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleContext ->
            Dict.singleton (String.join "." moduleName) moduleContext.declarations
        )
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts =
    Dict.union


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor nodes context =
    let
        declarations : List String
        declarations =
            List.filterMap
                (\node ->
                    case Node.value node of
                        Declaration.FunctionDeclaration { declaration } ->
                            declaration
                                |> Node.value
                                |> .name
                                |> Node.value
                                |> Just

                        _ ->
                            Nothing
                )
                nodes
    in
    { context | declarations = declarations }


dataExtractor : ProjectContext -> Encode.Value
dataExtractor declarations =
    Encode.object
        [ ( "foo", Encode.string "bar" )
        , ( "other", Encode.list Encode.int [ 1, 2, 3 ] )
        , ( "declarations", Encode.dict identity (Encode.list Encode.string) declarations )
        , ( "null", Encode.null )
        ]


all : Test
all =
    Test.describe "Extract data"
        [ test "should be able to extract data from the rule as JSON" <|
            \() ->
                [ """module A exposing (..)
import B
a = 1
b = 2
c = 3
"""
                , """module B exposing (..)
x = 1
y = 2
z = 3
"""
                ]
                    |> Review.Test.runOnModules (rule Nothing)
                    -- Bad formatting is on purpose
                    |> Review.Test.expectDataExtract """{
        "foo": "bar",
                    "other": [ 1, 2, 3 ],
  "declarations": {
    "A": [ "a", "b", "c" ],
    "B": [ "x", "y", "z" ]
  }
,"null": null}"""
        , test "should extract even if there are errors" <|
            \() ->
                [ """module A exposing (..)
import B
a = 1
b = 2
c = 3
"""
                , """module B exposing (..)
x = 1
y = 2
z = 3
"""
                ]
                    |> Review.Test.runOnModules (rule (Just (Rule.globalError { message = "message", details = [ "details" ] })))
                    |> Review.Test.expect
                        [ Review.Test.globalErrors [ { message = "message", details = [ "details" ] } ]
                        , Review.Test.dataExtract """{
        "foo": "bar",
                    "other": [ 1, 2, 3 ],
  "declarations": {
    "A": [ "a", "b", "c" ],
    "B": [ "x", "y", "z" ]
  }
,"null": null}"""
                        ]
        , test "should prevent extract when an error prevents it" <|
            \() ->
                [ """module A exposing (..)
import B
a = 1
b = 2
c = 3
"""
                , """module B exposing (..)
x = 1
y = 2
z = 3
"""
                ]
                    |> Review.Test.runOnModules
                        (rule
                            (Just
                                (Rule.globalError { message = "message", details = [ "details" ] }
                                    |> Rule.preventExtract
                                )
                            )
                        )
                    |> Review.Test.expectGlobalErrors
                        [ { message = "message", details = [ "details" ] } ]
        ]
