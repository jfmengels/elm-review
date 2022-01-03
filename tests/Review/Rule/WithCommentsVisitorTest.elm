module Review.Rule.WithCommentsVisitorTest exposing (all)

import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, test)


type alias Context =
    List String


all : Test
all =
    describe "Review.Rule.withCommentsVisitorVisitor"
        [ test "passes the list of comments in source order to the rule" <|
            \() ->
                Review.Test.run rule """port module ModuleName exposing (a)
{-| Module

@docs a
-}

-- Loose

{-| Type doc comment
-}
type A
    = Bar
    -- In type
    | Baz

{-| Type alias doc comment
-}
type alias Rec =
    { a : Int
    -- In type alias
    , b : String}

{-| Function doc comment
-}
a =
    -- In Function
    0

b = 2

{-| Port doc comment
-}
port output : Json.Encode.Value -> Cmd msg

-- Last comment
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "comments"
                            , details =
                                [ """{-| Module

@docs a
-}"""
                                , "-- Loose"
                                , "-- In type"
                                , "-- In type alias"
                                , "-- In Function"
                                , """{-| Port doc comment
-}"""
                                , "-- Last comment"
                                ]
                            , under = "port module"
                            }
                        ]
        ]


rule : Rule
rule =
    Rule.newModuleRuleSchema "WithCommentsVisitorTestRule" []
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    [ Rule.error { message = "comments", details = context }
        { start = { row = 1, column = 1 }
        , end = { row = 1, column = 12 }
        }
    ]


commentsVisitor : List (Node String) -> Context -> ( List (Error {}), Context )
commentsVisitor comments _ =
    ( [], List.map Node.value comments )
