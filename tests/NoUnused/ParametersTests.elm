module NoUnused.ParametersTests exposing (all)

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

        --- un-tests
        , describe "in Case branches" caseTests
        , describe "in Let destructuring" letDestructuringTests

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
        ]


caseTests : List Test
caseTests =
    [ test "should not report unused values" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        bish ->
            Nothing
        bash ->
            Nothing
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report list of unused values" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        [one] -> 1
        [first, two] -> 2
        more -> 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report uncons of unused values" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        one :: [] -> 1
        first :: two :: [] -> 2
        _ :: _ :: more -> 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


functionArgumentTests : List Test
functionArgumentTests =
    [ test "should report unused arguments" <|
        \() ->
            """
module A exposing (..)
foo : Int -> String -> String -> String
foo one two three =
    three
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `one` is not used."
                        , details = details
                        , under = "one"
                        }
                    , Review.Test.error
                        { message = "Parameter `two` is not used."
                        , details = details
                        , under = "two"
                        }
                    ]
    , test "should not consider values from other modules" <|
        \() ->
            """
module A exposing (..)
foo one =
    Bar.one
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `one` is not used."
                        , details = details
                        , under = "one"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                    ]
    ]


lambdaArgumentTests : List Test
lambdaArgumentTests =
    [ test "should report unused arguments" <|
        \() ->
            """
module A exposing (..)
foo =
    List.map (\\value -> Nothing) list
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `value` is not used."
                        , details = details
                        , under = "value"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    List.map (\\_ -> Nothing) list
"""
                    ]
    ]


letDestructuringTests : List Test
letDestructuringTests =
    [ test "should not report unused values" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        ( left, right ) =
            tupleValue
    in
    left
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused patterns that are aliased" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        (_ as bar) = 1
    in
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


letFunctionTests : List Test
letFunctionTests =
    [ test "should report unused arguments" <|
        \() ->
            """
module A exposing (..)
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
                        { message = "Parameter `oneValue` is not used."
                        , details = details
                        , under = "oneValue"
                        }
                    , Review.Test.error
                        { message = "Parameter `twoValue` is not used."
                        , details = details
                        , under = "twoValue"
                        }
                    ]
    , test "should not report unused let functions" <|
        \() ->
            """
module A exposing (..)
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
            """
module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        ( bish, bash )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish, bash}) ->
        ( bish, bash )
"""
                    ]
    , test "should report unused patterns in an as pattern" <|
        \() ->
            """
module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        ( bish, bosh )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish} as bosh) ->
        ( bish, bosh )
"""
                    ]
    , test "should report unused patterns and unused aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    \\({ bish, bash } as bosh) ->
        bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish} as bosh) ->
        bish
"""
                    , Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\({bish, bash}) ->
        bish
"""
                    ]
    , test "should report unused patterns that are aliased" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(_ as bar) -> bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern `_` is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "_"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(bar) -> bar
"""
                    ]
    , test "should report nested unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Named ( _, ( Named bash ) as bish )) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(Named ( _, ( Named bash ) )) ->
        bash
"""
                    ]
    ]


lambdaNamedPatternTests : List Test
lambdaNamedPatternTests =
    [ test "should report unused named patterns" <|
        \() ->
            """
module A exposing (..)
foo =
    \\Named bish ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\Named _ ->
        bash
"""
                    ]
    , test "should report unused nested named patterns" <|
        \() ->
            """
module A exposing (..)
foo =
    \\Named (Bish bish) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\Named (Bish _) ->
        bash
"""
                    ]
    , test "should report unused named patterns with multiple segments" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Pair _ _) -> bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Named pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "Pair _ _"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(_) -> bash
"""
                    ]
    , test "should report unused named patterns in tuples" <|
        \() ->
            """
module A exposing (..)
foo =
    \\(Singular _, Pair _ _) -> bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Named pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "Singular _"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(_, Pair _ _) -> bish
"""
                    , Review.Test.error
                        { message = "Named pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "Pair _ _"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\(Singular _, _) -> bish
"""
                    ]
    ]


lambdaRecordPatternTests : List Test
lambdaRecordPatternTests =
    [ test "should replace unused record with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    \\{ bish, bash, bosh } ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bish`, `bash` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bish, bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    , test "should report unused record values" <|
        \() ->
            """
module A exposing (..)
foo =
    \\{ bish, bash, bosh } ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bish` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bish, bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\{bash} ->
        bash
"""
                    ]
    , test "should report highlight the least amount of values possible" <|
        \() ->
            """
module A exposing (..)
foo =
    \\{ bish, bash, bosh } ->
        bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bash` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\{bish} ->
        bish
"""
                    ]
    ]


lambdaTuplePatternTests : List Test
lambdaTuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """
module A exposing (..)
foo =
    \\( bish, bash, bosh ) ->
        bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\( _, bash, bosh ) ->
        bash
"""
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\( bish, bash, _ ) ->
        bash
"""
                    ]
    , test "should replace unused tuple with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    \\( _, _ ) ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "( _, _ )"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    \\_ ->
        bar
"""
                    ]
    , test "should replace unused threeple with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    \\( _, _, _ ) ->
        bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "( _, _, _ )"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
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
            """
module A exposing (..)
foo ({ bish, bash } as bosh) =
    ( bish, bash )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused patterns in an as pattern" <|
        \() ->
            """
module A exposing (..)
foo ({ bish, bash } as bosh) =
    ( bish, bosh )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                    ]
    , test "should report unused patterns and unused aliases" <|
        \() ->
            """
module A exposing (..)
foo ({ bish, bash } as bosh) =
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                    , Review.Test.error
                        { message = "Pattern alias `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused patterns that are aliased" <|
        \() ->
            """
module A exposing (..)
foo (_ as bar) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern `_` is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "_"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo (bar) =
    bar
"""
                    ]
    , test "should report nested unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo (Named ( _, ( Just bash ) as bish )) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern alias `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    ]
    ]


functionNamedPatternTests : List Test
functionNamedPatternTests =
    [ test "should report unused named patterns" <|
        \() ->
            """
module A exposing (..)
foo (Named bish) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    ]
    , test "should report unused nested named patterns" <|
        \() ->
            """
module A exposing (..)
foo (Named (Bish bish)) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    ]
    , test "should report unused named patterns with multiple segments" <|
        \() ->
            """
module A exposing (..)
foo (Pair _ _) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Named pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "Pair _ _"
                        }
                    ]
    , test "should report unused named patterns in tuples" <|
        \() ->
            """
module A exposing (..)
foo (Singular _, Pair _ _) =
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Named pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "Singular _"
                        }
                    , Review.Test.error
                        { message = "Named pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "Pair _ _"
                        }
                    ]
    ]


functionRecordPatternTests : List Test
functionRecordPatternTests =
    [ test "should replace unused record with `_`" <|
        \() ->
            """
module A exposing (..)
foo { bish, bash, bosh } =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bish`, `bash` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bish, bash, bosh"
                        }
                    ]
    , test "should report unused record values" <|
        \() ->
            """
module A exposing (..)
foo { bish, bash, bosh } =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bish` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bish, bash, bosh"
                        }
                    ]
    , test "should report highlight the least amount of values possible" <|
        \() ->
            """
module A exposing (..)
foo { bish, bash, bosh } =
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameters `bash` and `bosh` are not used."
                        , details = [ "You should either use these parameters somewhere, or remove them at the location I pointed at." ]
                        , under = "bash, bosh"
                        }
                    ]
    ]


functionTuplePatternTests : List Test
functionTuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """
module A exposing (..)
foo ( bish, bash, bosh ) =
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                    ]
    , test "should report unused tuple" <|
        \() ->
            """
module A exposing (..)
foo ( _, _ ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "( _, _ )"
                        }
                    ]
    , test "should report unused threeple" <|
        \() ->
            """
module A exposing (..)
foo ( _, _, _ ) =
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed."
                        , details = [ "You should remove it at the location I pointed at." ]
                        , under = "( _, _, _ )"
                        }
                    ]
    ]
