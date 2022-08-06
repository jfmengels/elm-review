module Review.Rule.WithCommentsVisitorTest exposing (all)

import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Rule.withCommentsVisitor"
        [ test "passes the list of comments in source order to the rule" <|
            \() ->
                """port module ModuleName exposing (a)
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
                    |> Review.Test.run rule
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
    Rule.newModuleRuleSchema "WithCommentsVisitorTestRule" ()
        |> Rule.withSimpleCommentsVisitor commentsVisitor
        |> Rule.fromModuleRuleSchema


commentsVisitor : List (Node String) -> List (Error {})
commentsVisitor comments =
    [ Rule.error { message = "comments", details = List.map Node.value comments }
        { start = { row = 1, column = 1 }
        , end = { row = 1, column = 12 }
        }
    ]
