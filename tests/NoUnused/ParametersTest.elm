module NoUnused.ParametersTest exposing (all)

import NoUnused.Parameters exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]


all : Test
all =
    describe "NoUnused.Parameters"
        [ describe "in Function arguments" functionArgumentTests
        , describe "in Lambda arguments" lambdaArgumentTests
        , describe "in Let Functions" letFunctionTests

        --- in lambda
        , describe "with as pattern in lambdas" lambdaAsPatternTests
        , describe "with named pattern in lambdas" lambdaNamedPatternTests
        , describe "with record pattern in lambdas" lambdaRecordPatternTests
        , describe "with tuple pattern in lambdas" lambdaTuplePatternTests

        --- in function
        , describe "with as pattern in functions" functionAsPatternTests
        , describe "with named pattern in functions" functionNamedPatternTests
        , describe "with record pattern in functions" functionRecordPatternTests
        , describe "with tuple pattern in functions" functionTuplePatternTests

        -- Recursive parameters
        , describe "Recursive parameters" recursiveParameterTests
        ]


functionArgumentTests : List Test
functionArgumentTests =
    [ test "should report unused arguments" <|
        \() ->
            """module A exposing (..)
foo : Int -> String -> String -> String
foo one two three =
    three
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `one` is not used"
                        , details = details
                        , under = "one"
                        }
                    , Review.Test.error
                        { message = "Parameter `two` is not used"
                        , details = details
                        , under = "two"
                        }
                    ]
    , test "should not consider values from other modules" <|
        \() ->
            """module A exposing (..)
foo one =
    Bar.one
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `one` is not used"
                        , details = details
                        , under = "one"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                    ]
    , test "should not report used parameters (value reference)" <|
        \() ->
            """module A exposing (..)
foo one =
    one
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used parameters (record update reference)" <|
        \() ->
            """module A exposing (..)
foo one =
    { one | a = 1 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


lambdaArgumentTests : List Test
lambdaArgumentTests =
    [ test "should report unused arguments" <|
        \() ->
            """module A exposing (..)
foo =
    List.map (\\value -> Nothing) list
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `value` is not used"
                        , details = details
                        , under = "value"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    List.map (\\_ -> Nothing) list
"""
                    ]
    ]


letFunctionTests : List Test
letFunctionTests =
    [ test "should report unused arguments" <|
        \() ->
            """module A exposing (..)
foo =
    let
        one oneValue =
            1
        two twoValue =
            2
    in
    one two 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `oneValue` is not used"
                        , details = details
                        , under = "oneValue"
                        }
                    , Review.Test.error
                        { message = "Parameter `twoValue` is not used"
                        , details = details
                        , under = "twoValue"
                        }
                    ]
    , test "should report unused even if others with the same name are used in siblings" <|
        \() ->
            """module A exposing (..)
foo =
    let
        one oneValue =
            oneValue
        two oneValue =
            1
    in
    one two 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `oneValue` is not used"
                        , details = details
                        , under = "oneValue"
                        }
                        |> Review.Test.atExactly { start = { row = 6, column = 13 }, end = { row = 6, column = 21 } }
                    ]
    , test "should not report unused let functions" <|
        \() ->
            """module A exposing (..)
foo =
    let
        value =
            something 5
    in
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]



--- LAMBDA PATTERN TESTS ------------------------


lambdaAsPatternTests : List Test
lambdaAsPatternTests =
    [ test "should report unused pattern aliases" <|
        \() ->
            """module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        ( bish, bash )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\({ bish, bash }) ->
        ( bish, bash )
"""
                    ]
    , test "should report unused patterns in an as pattern" <|
        \() ->
            """module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        ( bish, bosh )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used"
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\({ bish } as bosh) ->
        ( bish, bosh )
"""
                    ]
    , test "should report unused patterns and unused aliases" <|
        \() ->
            """module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used"
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\({ bish } as bosh) ->
        bish
"""
                    , Review.Test.error
                        { message = "Pattern alias `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\({ bish, bash }) ->
        bish
"""
                    ]
    , test "should not report aliases to wildcards" <|
        -- Already handled by `NoUnused.Patterns`
        \() ->
            """module A exposing (..)
foo =
    \\(_ as bar) -> bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report nested unused pattern aliases" <|
        \() ->
            """module A exposing (..)
foo =
    \\(Named ( _, ( Named bash ) as bish )) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\(Named ( _, ( Named bash ) )) ->
        bash
"""
                    ]
    ]


lambdaNamedPatternTests : List Test
lambdaNamedPatternTests =
    [ test "should report unused variables extracted out of named patterns" <|
        \() ->
            """module A exposing (..)
foo =
    \\(Named bish) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\(Named _) ->
        bash
"""
                    ]
    , test "should report unused nested named patterns" <|
        \() ->
            """module A exposing (..)
foo =
    \\(Named (Bish bish)) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\(Named (Bish _)) ->
        bash
"""
                    ]
    , test "should not report named patterns" <|
        \() ->
            """module A exposing (..)
foo =
    \\(Pair _ _) -> bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused named patterns in tuples" <|
        \() ->
            """module A exposing (..)
foo =
    \\(Singular _, Pair _ _) -> bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


lambdaRecordPatternTests : List Test
lambdaRecordPatternTests =
    [ test "should report and remove unused record fields" <|
        \() ->
            """module A exposing (..)
foo =
    \\{ bish, bash, bosh } ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\{ bash, bosh } ->
        bar
"""
                    , Review.Test.error
                        { message = "Parameter `bash` is not used"
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\{ bish, bosh } ->
        bar
"""
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\{ bish, bash } ->
        bar
"""
                    ]
    , test "should report and replace unused record field by `_` when there is only a single field" <|
        \() ->
            """module A exposing (..)
foo =
    \\{ bish } ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    ]


lambdaTuplePatternTests : List Test
lambdaTuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """module A exposing (..)
foo =
    \\( bish, bash, bosh ) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\( _, bash, bosh ) ->
        bash
"""
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\( bish, bash, _ ) ->
        bash
"""
                    ]
    , test "should replace unused tuple with `_`" <|
        \() ->
            """module A exposing (..)
foo =
    \\( _, _ ) ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _ )"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    , test "should replace unused threeple with `_`" <|
        \() ->
            """module A exposing (..)
foo =
    \\( _, _, _ ) ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _, _ )"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    ]



--- FUNCTION PATTERN TESTS


functionAsPatternTests : List Test
functionAsPatternTests =
    [ test "should report unused pattern aliases" <|
        \() ->
            """module A exposing (..)
foo ({ bish, bash } as bosh) =
    ( bish, bash )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused patterns in an as pattern" <|
        \() ->
            """module A exposing (..)
foo ({ bish, bash } as bosh) =
    ( bish, bosh )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used"
                        , details = details
                        , under = "bash"
                        }
                    ]
    , test "should report unused patterns and unused aliases" <|
        \() ->
            """module A exposing (..)
foo ({ bish, bash } as bosh) =
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used"
                        , details = details
                        , under = "bash"
                        }
                    , Review.Test.error
                        { message = "Pattern alias `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should not report aliases to wildcards" <|
        -- Already handled by `NoUnused.Patterns`
        \() ->
            """module A exposing (..)
foo (_ as bar) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report nested unused pattern aliases" <|
        \() ->
            """module A exposing (..)
foo (Named ( _, ( Just bash ) as bish )) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                    ]
    , test "should not report aliased pattern if it contains a named pattern" <|
        \() ->
            """module A exposing (..)
foo (Named as bash) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


functionNamedPatternTests : List Test
functionNamedPatternTests =
    [ test "should report unused named patterns" <|
        \() ->
            """module A exposing (..)
foo (Named bish) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                    ]
    , test "should report unused nested named patterns" <|
        \() ->
            """module A exposing (..)
foo (Named (Bish bish)) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                    ]
    , test "should report unused named patterns with multiple segments" <|
        \() ->
            """module A exposing (..)
foo (Pair _ _) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused named patterns in tuples" <|
        \() ->
            """module A exposing (..)
foo (Singular _, Pair _ _) =
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused value-less named pattern" <|
        \() ->
            """module A exposing (..)
foo Thing =
    x
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


functionRecordPatternTests : List Test
functionRecordPatternTests =
    [ test "should report unused fields" <|
        \() ->
            """module A exposing (..)
foo { bish, bash, bosh } =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                    , Review.Test.error
                        { message = "Parameter `bash` is not used"
                        , details = details
                        , under = "bash"
                        }
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report only the unused record values" <|
        \() ->
            """module A exposing (..)
foo { bish, bash, bosh } =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                    ]
    ]


functionTuplePatternTests : List Test
functionTuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """module A exposing (..)
foo ( bish, bash, bosh ) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused tuple" <|
        \() ->
            """module A exposing (..)
foo ( _, _ ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _ )"
                        }
                    ]
    , test "should report ()" <|
        \() ->
            """module A exposing (..)
foo () =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused when it contains ()" <|
        \() ->
            """module A exposing (..)
foo ( _, () ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused when it contains empty tuples" <|
        \() ->
            """module A exposing (..)
foo ( _, ( _, _ ) ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _ )"
                        }
                    ]
    , test "should report unused threeple" <|
        \() ->
            """module A exposing (..)
foo ( _, _, _ ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _, _ )"
                        }
                    ]
    ]



-- RECURSIVE PARAMETERS


recursiveParameterTests : List Test
recursiveParameterTests =
    [ test "should report parameters that are only used to be passed to the function itself" <|
        \() ->
            """module A exposing (..)
foo x unused =
    if cond then
        x
    else
        foo (x - 1) unused
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is only used in recursion"
                        , details =
                            [ "This parameter is only used to be passed as an argument to 'foo', but its value is never read or used."
                            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                            ]
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 13 } }
                    ]
    , test "should report parameters that are only used to be passed to the function itself (record update)" <|
        \() ->
            """module A exposing (..)
bar x unused =
    if cond then
        x
    else
        bar (x - 1) { unused | x = 1 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is only used in recursion"
                        , details =
                            [ "This parameter is only used to be passed as an argument to 'bar', but its value is never read or used."
                            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                            ]
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 13 } }
                    ]
    , test "should report parameters that are only used to be passed to the function itself (complex expression in the same position)" <|
        \() ->
            """module A exposing (..)
bar x unused =
    if cond then
        x
    else
        bar (x - 1) (List.map fn unused |> List.head)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is only used in recursion"
                        , details =
                            [ "This parameter is only used to be passed as an argument to 'bar', but its value is never read or used."
                            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                            ]
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 13 } }
                    ]
    , test "should not report parameters that are also used elsewhere" <|
        \() ->
            """module A exposing (..)
foo x used =
    if cond then
        x
    else
        foo (x - 1) used + used
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report parameters where the position in call site don't match the one in arguments" <|
        \() ->
            """module A exposing (..)
foo x used =
    if cond then
        x
    else
        foo used x
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused recursive parameters if they were destructured and only one variable was found" <|
        \() ->
            """module A exposing (..)
bar x {unused} =
    if cond then
        x
    else
        bar (x - 1) (List.map fn unused |> List.head)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is only used in recursion"
                        , details =
                            [ "This parameter is only used to be passed as an argument to 'bar', but its value is never read or used."
                            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                            ]
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 8 }, end = { row = 2, column = 14 } }
                    ]
    , test "should report unused recursive parameters when function is called through |>" <|
        \() ->
            """module A exposing (..)
foo x unused =
    if cond then
        x
    else
        unused |> foo (x - 1)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is only used in recursion"
                        , details =
                            [ "This parameter is only used to be passed as an argument to 'foo', but its value is never read or used."
                            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                            ]
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 13 } }
                    ]
    , test "should report unused recursive parameters when function is called through <|" <|
        \() ->
            """module A exposing (..)
foo x unused =
    if cond then
        x
    else
        foo (x - 1) <| unused
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is only used in recursion"
                        , details =
                            [ "This parameter is only used to be passed as an argument to 'foo', but its value is never read or used."
                            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                            ]
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 13 } }
                    ]
    ]
