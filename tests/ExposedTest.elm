module ExposedTest exposing (all)

import Dict exposing (Dict)
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Rule.withExposed"
        [ test "when the module exposes everything" <|
            \() ->
                """module A exposing (..)
a = 1
b = 2
type alias TypeAlias = {}
type CustomType = CustomType Int
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """{ exposed = Dict.fromList [("CustomType",True),("TypeAlias",False),("a",False),("b",False)], exposesAll = True }"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        , test "when the module exposes elements explicitly" <|
            \() ->
                """module A exposing (Opaque, Shown(..), TypeAlias, a, b)
a = 1
b = 2
hidden = 3
type alias TypeAlias = {}
type alias HiddenTypeAlias = {}
type Opaque = Opaque Int
type Shown = Shown Int
type Hidden = Hidden Int
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """{ exposed = Dict.fromList [("Opaque",False),("Shown",True),("TypeAlias",False),("a",False),("b",False)], exposesAll = False }"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        ]


type alias ModuleContext =
    { exposesAll : Bool
    , exposed : Dict String Bool
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" contextCreator
        |> Rule.withSimpleModuleDefinitionVisitor (always [])
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


contextCreator : Rule.ContextCreator () ModuleContext
contextCreator =
    Rule.initContextCreator
        (\exposed () ->
            exposed
        )
        |> Rule.withExposed


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    [ Rule.error
        { message = Debug.toString context
        , details = [ "details" ]
        }
        { start = { row = 1, column = 1 }
        , end = { row = 1, column = 7 }
        }
    ]
