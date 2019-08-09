module FixTest exposing (all)

import Elm.Syntax.Range exposing (Range)
import Expect
import Lint.Fix as Fix
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Fix"
        [ mergeRangesTest
        , rangeUpUntilTest
        , fixTest
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


rangeUpUntilTest : Test
rangeUpUntilTest =
    describe "rangeUpUntil"
        [ test "should stop the range at the position if the position is in the range" <|
            \() ->
                let
                    range : Range
                    range =
                        { start = { row = 2, column = 10 }
                        , end = { row = 4, column = 10 }
                        }

                    position : { row : Int, column : Int }
                    position =
                        { row = 4, column = 3 }

                    expected : Range
                    expected =
                        { start = { row = 2, column = 10 }
                        , end = { row = 4, column = 3 }
                        }
                in
                Fix.rangeUpUntil range position
                    |> Expect.equal expected

        -- , test "should not change the range if the position is after the range" <|
        --     \() ->
        --         let
        --             range : Range
        --             range =
        --                 { start = { row = 2, column = 10 }
        --                 , end = { row = 4, column = 10 }
        --                 }
        --
        --             position : { row : Int, column : Int }
        --             position =
        --                 { row = 100, column = 100 }
        --         in
        --         Fix.rangeUpUntil range position
        --             |> Expect.equal range
        -- , test "should not change the range if the position is before the range" <|
        --     \() ->
        --         let
        --             range : Range
        --             range =
        --                 { start = { row = 2, column = 10 }
        --                 , end = { row = 4, column = 10 }
        --                 }
        --
        --             position : { row : Int, column : Int }
        --             position =
        --                 { row = 1, column = 1 }
        --         in
        --         Fix.rangeUpUntil range position
        --             |> Expect.equal range
        ]


fixTest : Test
fixTest =
    describe "fix"
        [ test "should apply a removal on a single line" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
a = Debug.log "foo" 1
"""

                    fixes =
                        [ Fix.removeRange
                            { start = { row = 2, column = 5 }
                            , end = { row = 2, column = 20 }
                            }
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (a)
a =  1
""")
        , test "should apply a replacement on a single line" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
some_var = 1
"""

                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 2, column = 9 }
                            }
                            "someVar"
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (a)
someVar = 1
""")
        , test "should insert something on a single line" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
a = 1
"""

                    fixes =
                        [ Fix.insertAt
                            { row = 2, column = 5 }
                            """Debug.log "foo" """
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (a)
a = Debug.log "foo" 1
""")
        , test "should apply multiple fixes regardless of the order" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
a = 1
"""

                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 2, column = 2 }
                            }
                            "someVar"
                        , Fix.insertAt
                            { row = 2, column = 5 }
                            """Debug.log "foo" """
                        ]
                in
                Expect.all
                    [ \() ->
                        Fix.fix fixes source
                            |> Expect.equal (Fix.Successful """module A exposing (a)
someVar = Debug.log "foo" 1
""")
                    , \() ->
                        Fix.fix (List.reverse fixes) source
                            |> Expect.equal (Fix.Successful """module A exposing (a)
someVar = Debug.log "foo" 1
""")
                    ]
                    ()
        , test "should apply a removal on multiple lines" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (someCode)
someCode = 2

a : Int
a = 1
"""

                    fixes =
                        [ Fix.removeRange
                            { start = { row = 4, column = 1 }
                            , end = { row = 5, column = 6 }
                            }
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (someCode)
someCode = 2


""")
        , test "should apply a replacement whose content is on multiple lines" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
some_var = 1
"""

                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 2, column = 13 }
                            }
                            "someVar =\n  1"
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (a)
someVar =
  1
""")
        , test "should apply a replacement on multiple lines" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
some_var =
  1
"""

                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 3, column = 4 }
                            }
                            "someVar = 1"
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (a)
someVar = 1
""")
        , test "should apply a replacement on multiple lines with something on multiple lines" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
some_var =
  1
"""

                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 3, column = 4 }
                            }
                            "foo =\n  2"
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (a)
foo =
  2
""")
        , test "should apply an insertion on multiple lines" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (someCode)
someCode = 2

a : Int
a = 1
"""

                    fixes =
                        [ Fix.insertAt
                            { row = 4, column = 1 }
                            "b =\n  2\n"
                        ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Successful """module A exposing (someCode)
someCode = 2

b =
  2
a : Int
a = 1
""")
        , test "should fail if the source code is the same after fixes" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (someCode)
someCode = 2

a : Int
a = 1
"""

                    fixes : List Fix.Fix
                    fixes =
                        [ Fix.insertAt { row = 4, column = 1 } "" ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Errored Fix.Unchanged)
        , test "should fail if the source code is unparsable after fixes" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (someCode)
someCode = 2
"""

                    fixes : List Fix.Fix
                    fixes =
                        [ Fix.removeRange { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ]
                in
                Fix.fix fixes source
                    |> Expect.equal (Fix.Errored <| Fix.SourceCodeIsNotValid """ule A exposing (someCode)
someCode = 2
""")
        ]
