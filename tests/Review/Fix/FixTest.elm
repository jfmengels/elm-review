module Review.Fix.FixTest exposing (all)

import Expect
import Review.Error as Error
import Review.Fix as Fix exposing (Fix)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "fix"
        [ test "should apply a removal on a single line" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (a)
a = Debug.log "foo" 1
"""

                    fixes : List Fix
                    fixes =
                        [ Fix.removeRange
                            { start = { row = 2, column = 5 }
                            , end = { row = 2, column = 20 }
                            }
                        ]
                in
                Fix.fix Error.Module fixes source
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

                    fixes : List Fix
                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 2, column = 9 }
                            }
                            "someVar"
                        ]
                in
                Fix.fix Error.Module fixes source
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

                    fixes : List Fix
                    fixes =
                        [ Fix.insertAt
                            { row = 2, column = 5 }
                            """Debug.log "foo" """
                        ]
                in
                Fix.fix Error.Module fixes source
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

                    fixes : List Fix
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
                        Fix.fix Error.Module fixes source
                            |> Expect.equal (Fix.Successful """module A exposing (a)
someVar = Debug.log "foo" 1
""")
                    , \() ->
                        Fix.fix Error.Module (List.reverse fixes) source
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

                    fixes : List Fix
                    fixes =
                        [ Fix.removeRange
                            { start = { row = 4, column = 1 }
                            , end = { row = 5, column = 6 }
                            }
                        ]
                in
                Fix.fix Error.Module fixes source
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

                    fixes : List Fix
                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 2, column = 13 }
                            }
                            "someVar =\n  1"
                        ]
                in
                Fix.fix Error.Module fixes source
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

                    fixes : List Fix
                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 3, column = 4 }
                            }
                            "someVar = 1"
                        ]
                in
                Fix.fix Error.Module fixes source
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

                    fixes : List Fix
                    fixes =
                        [ Fix.replaceRangeBy
                            { start = { row = 2, column = 1 }
                            , end = { row = 3, column = 4 }
                            }
                            "foo =\n  2"
                        ]
                in
                Fix.fix Error.Module fixes source
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

                    fixes : List Fix
                    fixes =
                        [ Fix.insertAt
                            { row = 4, column = 1 }
                            "b =\n  2\n"
                        ]
                in
                Fix.fix Error.Module fixes source
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
                Fix.fix Error.Module fixes source
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
                Fix.fix Error.Module fixes source
                    |> Expect.equal (Fix.Errored <| Fix.SourceCodeIsNotValid """ule A exposing (someCode)
someCode = 2
""")
        , test "should fail if the fixes' range overlap" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (someCode)
someCode = 2
"""

                    fixes : List Fix.Fix
                    fixes =
                        [ Fix.replaceRangeBy { start = { row = 10, column = 1 }, end = { row = 20, column = 1 } } ""
                        , Fix.replaceRangeBy { start = { row = 15, column = 1 }, end = { row = 20, column = 1 } } ""
                        ]
                in
                Expect.all
                    [ \() ->
                        Fix.fix Error.Module fixes source
                            |> Expect.equal (Fix.Errored Fix.HasCollisionsInFixRanges)
                    , \() ->
                        Fix.fix Error.Module (List.reverse fixes) source
                            |> Expect.equal (Fix.Errored Fix.HasCollisionsInFixRanges)
                    ]
                    ()
        , test "should fail if an insertion fix is contained inside another fix's range" <|
            \() ->
                let
                    source : String
                    source =
                        """module A exposing (someCode)
someCode = 2
                    """

                    fixes : List Fix.Fix
                    fixes =
                        [ Fix.replaceRangeBy { start = { row = 10, column = 1 }, end = { row = 20, column = 1 } } ""
                        , Fix.insertAt { row = 15, column = 1 } "foo"
                        ]
                in
                Expect.all
                    [ \() ->
                        Fix.fix Error.Module fixes source
                            |> Expect.equal (Fix.Errored Fix.HasCollisionsInFixRanges)
                    , \() ->
                        Fix.fix Error.Module (List.reverse fixes) source
                            |> Expect.equal (Fix.Errored Fix.HasCollisionsInFixRanges)
                    ]
                    ()
        , describe "README fixes"
            [ test "should apply fixes to README" <|
                \() ->
                    let
                        source : String
                        source =
                            """# My project
My description
"""

                        fixes : List Fix.Fix
                        fixes =
                            [ Fix.removeRange { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ]
                    in
                    Fix.fix Error.Module fixes source
                        |> Expect.equal (Fix.Errored <| Fix.SourceCodeIsNotValid """y project
My description
""")
            ]
        ]
