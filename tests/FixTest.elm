module FixTest exposing (all)

import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Lint.Fix as Fix
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Fix"
        [ mergeRangesTest
        ]


mergeRangesTest : Test
mergeRangesTest =
    describe "mergeRanges"
        [ test "should take the start of the one before and the end of the one after if ranges are distinct" <|
            \() ->
                let
                    a : Range
                    a =
                        { start = { row = 2, column = 10 }
                        , end = { row = 2, column = 15 }
                        }

                    b : Range
                    b =
                        { start = { row = 20, column = 1 }
                        , end = { row = 22, column = 5 }
                        }

                    expected : Range
                    expected =
                        { start = { row = 2, column = 10 }
                        , end = { row = 22, column = 5 }
                        }
                in
                Expect.all
                    [ \() ->
                        Fix.mergeRanges a b
                            |> Expect.equal expected
                    , \() ->
                        Fix.mergeRanges b a
                            |> Expect.equal expected
                    ]
                    ()
        , test "should take the start of the one that starts first and the end of the one ends last if ranges are shared" <|
            \() ->
                let
                    a : Range
                    a =
                        { start = { row = 2, column = 10 }
                        , end = { row = 10, column = 15 }
                        }

                    b : Range
                    b =
                        { start = { row = 5, column = 1 }
                        , end = { row = 22, column = 5 }
                        }

                    expected : Range
                    expected =
                        { start = { row = 2, column = 10 }
                        , end = { row = 22, column = 5 }
                        }
                in
                Expect.all
                    [ \() ->
                        Fix.mergeRanges a b
                            |> Expect.equal expected
                    , \() ->
                        Fix.mergeRanges b a
                            |> Expect.equal expected
                    ]
                    ()
        , test "should take the bigger one if one is included in the other" <|
            \() ->
                let
                    a : Range
                    a =
                        { start = { row = 2, column = 10 }
                        , end = { row = 10, column = 15 }
                        }

                    b : Range
                    b =
                        { start = { row = 3, column = 1 }
                        , end = { row = 4, column = 5 }
                        }
                in
                Expect.all
                    [ \() ->
                        Fix.mergeRanges a b
                            |> Expect.equal a
                    , \() ->
                        Fix.mergeRanges b a
                            |> Expect.equal a
                    ]
                    ()
        ]
