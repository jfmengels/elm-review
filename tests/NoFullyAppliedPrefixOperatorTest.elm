module NoFullyAppliedPrefixOperatorTest exposing (all)

import NoFullyAppliedPrefixOperator exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "Prefer using the infix form (`a + b`) over the prefix form (`(+) a b`) when possible"


details : List String
details =
    [ "The prefix form is generally harder to read over the infix form."
    ]


all : Test
all =
    describe "NoFullyAppliedPrefixOperator"
        [ test "should not report a lonely operator" <|
            \() ->
                """
module A exposing (..)
a = (++)
b = (::)
c = (//)
d = (+)
e = (/)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in infix position" <|
            \() ->
                """
module A exposing (..)
a = y ++ z
b = y :: z
c = y // z
d = y + z
e = y / z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in prefix position with one argument" <|
            \() ->
                """
module A exposing (..)
a = (++) z
b = (::) z
c = (//) z
d = (+) z
e = (/) z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an operator used in prefix position with both arguments" <|
            \() ->
                """
module A exposing (..)
a = (++) y z
b = (::) y z
c = (//) y z
d = (+) y z
e = (/) y z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "(++)"
                            }
                        , Review.Test.error
                            { message = message
                            , details = details
                            , under = "(::)"
                            }
                        , Review.Test.error
                            { message = message
                            , details = details
                            , under = "(//)"
                            }
                        , Review.Test.error
                            { message = message
                            , details = details
                            , under = "(+)"
                            }
                        , Review.Test.error
                            { message = message
                            , details = details
                            , under = "(/)"
                            }
                        ]
        ]
