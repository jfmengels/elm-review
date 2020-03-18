module NoTodoCommentTest exposing (all)

import NoTodoComment exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "TODO needs to be handled"


details : List String
details =
    [ "At fruits.com, we prefer not to have lingering TODO comments. Either fix the TODO now or create an issue for it."
    ]


all : Test
all =
    describe "NoTodoComment"
        [ test "should not regular comments" <|
            \() ->
                """
module A exposing (..)
-- Some comment
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report TODO comment, and report the TODO until the end of the line" <|
            \() ->
                """
module A exposing (..)
-- TODO Do this
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "TODO Do this"
                            }
                        ]
        , test "should report TODO comment, and report the TODO until the end of the line (comment is indented)" <|
            \() ->
                """
module A exposing (..)
    -- TODO Do this
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "TODO Do this"
                            }
                        ]
        , test "should report TODO comment, and report the TODO until the end of the line, not the following lines" <|
            \() ->
                """
module A exposing (..)
-- TODO Do this
-- because there is a good reason
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "TODO Do this"
                            }
                        ]
        , test "should report TODO comment, and report the TODO until the end of the line, not the following lines (multiline comment)" <|
            \() ->
                """
module A exposing (..)
{- TODO Do this
because there is a good reason
-}
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "TODO Do this"
                            }
                        ]
        , test "should report TODO comment, and report the TODO until the end of the line, not the following lines (multiline comment, TODO doesn't start at the beginning)" <|
            \() ->
                """
module A exposing (..)
{- There is something
TODO Do this
because there is a good reason
-}
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "TODO Do this"
                            }
                        ]
        , test "should report TODO comment, and report the TODO until the end of the line, not the following lines (indented multiline comment)" <|
            \() ->
                """
module A exposing (..)
        {- There is something
        TODO Do this
        because there is a good reason
        -}
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "TODO Do this"
                            }
                        ]
        ]
