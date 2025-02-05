module Review.Fix.FixTest exposing (all)

import Expect
import Review.Fix as Fix exposing (Fix)
import Review.Fix.FixProblem exposing (FixProblem(..))
import Review.Fix.Internal as FixInternal
import Review.Rule exposing (Rule)
import Review.Test
import Review.Test.ArbitraryFixRule as ArbitraryFixRule
import Review.Test.FailureMessageHelper exposing (expectFailure)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (a)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (a)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (a)
a = Debug.log "foo" 1
""")
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (someCode)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (a)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (a)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (a)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Ok """module A exposing (someCode)
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
                FixInternal.applyEdits fixes source
                    |> Expect.equal (Err Unchanged)
        , test "should fail if the fixes' range overlap" <|
            \() ->
                let
                    testRule : Rule
                    testRule =
                        ArbitraryFixRule.rule
                            "src/A.elm"
                            [ Fix.replaceRangeBy { start = { row = 2, column = 12 }, end = { row = 20, column = 15 } } "321"
                            , Fix.replaceRangeBy { start = { row = 2, column = 13 }, end = { row = 20, column = 14 } } "432"
                            ]
                in
                """module A exposing (someCode)
someCode = 200
"""
                    |> Review.Test.run testRule
                    |> Review.Test.expectGlobalErrorsWithFixes
                        [ { message = ArbitraryFixRule.message
                          , details = ArbitraryFixRule.details
                          , fixes = [ ( "A", Review.Test.edited """module A exposing (..)
someCode = 432
""" ) ]
                          }
                        ]
                    |> expectFailure """FOUND COLLISIONS IN EDIT RANGES

I got something unexpected when applying the fixes provided by the error
with the following message:

  `Message`

I found that some edits were targeting (partially or completely) the same
section of code, among which the following two:

  1. Review.Fix.replaceRangeBy
         { start = { row = 2, column = 12 }, end = { row = 20, column = 15 } }
         "321"

  2. Review.Fix.replaceRangeBy
         { start = { row = 2, column = 13 }, end = { row = 20, column = 14 } }
         "432"

The problem with that is that I can't determine which fix
to apply first, and the result will be different and potentially invalid
based on the order in which I apply these fixes.

For this reason, I require that the ranges (for replacing and removing) and
the positions (for inserting) of every fix to be mutually exclusive.

Hint: Maybe you duplicated a fix, or you targeted the wrong node for one
of your fixes."""
        , test "should fail if an insertion fix is contained inside another fix's range" <|
            \() ->
                let
                    testRule : Rule
                    testRule =
                        ArbitraryFixRule.rule
                            "src/A.elm"
                            [ Fix.replaceRangeBy { start = { row = 10, column = 1 }, end = { row = 20, column = 1 } } ""
                            , Fix.insertAt { row = 15, column = 1 } "foo"
                            ]
                in
                """module A exposing (someCode)
someCode = 2
"""
                    |> Review.Test.run testRule
                    |> Review.Test.expectGlobalErrorsWithFixes
                        [ { message = ArbitraryFixRule.message
                          , details = ArbitraryFixRule.details
                          , fixes = [ ( "A", Review.Test.edited """ule A exposing (..)
someCode = 2
""" ) ]
                          }
                        ]
                    |> expectFailure """FOUND COLLISIONS IN EDIT RANGES

I got something unexpected when applying the fixes provided by the error
with the following message:

  `Message`

I found that some edits were targeting (partially or completely) the same
section of code, among which the following two:

  1. Review.Fix.removeRange
         { start = { row = 10, column = 1 }, end = { row = 20, column = 1 } }

  2. Review.Fix.insertAt
         { row = 15, column = 1 }
         "foo"

The problem with that is that I can't determine which fix
to apply first, and the result will be different and potentially invalid
based on the order in which I apply these fixes.

For this reason, I require that the ranges (for replacing and removing) and
the positions (for inserting) of every fix to be mutually exclusive.

Hint: Maybe you duplicated a fix, or you targeted the wrong node for one
of your fixes."""
        ]
