module NoTestValuesInProductionCodeTest exposing (all)

import NoTestValuesInProductionCode exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoTestValuesInProductionCode"
        [ endsWithTest
        , startsWithTest
        ]


endsWithTest : Test
endsWithTest =
    describe "endsWith"
        [ test "should report an error when using a function or value that ends with the specified suffix" <|
            \() ->
                """module A exposing (..)
listTESTS_ONLY = []
value = List.map foo listTESTS_ONLY
"""
                    |> Review.Test.run (rule (NoTestValuesInProductionCode.endsWith "TESTS_ONLY"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Forbidden use of test-only value `listTESTS_ONLY` in production source code"
                            , details =
                                [ "This value was marked as being meant to only be used in test-related code, but I found it being used in code that will go to production."
                                , "You should either stop using it or rename it to not end with `TESTS_ONLY`."
                                ]
                            , under = "listTESTS_ONLY"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 22 }, end = { row = 3, column = 36 } }
                        ]
        , test "should not report an error when using a function or value that does not end with the specified suffix" <|
            \() ->
                """module A exposing (..)
list = []
value = List.map foo list
"""
                    |> Review.Test.run (rule (NoTestValuesInProductionCode.endsWith "TESTS_ONLY"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when using a test value inside another test value" <|
            \() ->
                """module A exposing (..)
listTESTS_ONLY = []
valueTESTS_ONLY = List.map foo listTESTS_ONLY
"""
                    |> Review.Test.run (rule (NoTestValuesInProductionCode.endsWith "TESTS_ONLY"))
                    |> Review.Test.expectNoErrors
        ]


startsWithTest : Test
startsWithTest =
    describe "startsWith"
        [ test "should report an error when using a function or value that starts with the specified suffix" <|
            \() ->
                """module A exposing (..)
tests__list = []
value = List.map foo tests__list
"""
                    |> Review.Test.run (rule (NoTestValuesInProductionCode.startsWith "tests__"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Forbidden use of test-only value `tests__list` in production source code"
                            , details =
                                [ "This value was marked as being meant to only be used in test-related code, but I found it being used in code that will go to production."
                                , "You should either stop using it or rename it to not start with `tests__`."
                                ]
                            , under = "tests__list"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 22 }, end = { row = 3, column = 33 } }
                        ]
        , test "should not report an error when using a function or value that does not start with the specified suffix" <|
            \() ->
                """module A exposing (..)
list = []
value = List.map foo list
"""
                    |> Review.Test.run (rule (NoTestValuesInProductionCode.startsWith "tests__"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when using a test value inside another test value" <|
            \() ->
                """module A exposing (..)
tests__list = []
tests__value = List.map foo tests__list
"""
                    |> Review.Test.run (rule (NoTestValuesInProductionCode.startsWith "TESTS_ONLY"))
                    |> Review.Test.expectNoErrors
        ]
