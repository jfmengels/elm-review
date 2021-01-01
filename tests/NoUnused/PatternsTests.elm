module NoUnused.PatternsTests exposing (all)

import NoUnused.Patterns exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "You should either use this value somewhere, or remove it at the location I pointed at."
    ]


all : Test
all =
    describe "NoUnused.Patterns"
        [ describe "in Case branches" caseTests
        , describe "in Let destructuring" letDestructuringTests

        --- un-tests
        , describe "in Function arguments" functionArgumentTests
        , describe "in Lambda arguments" lambdaArgumentTests
        , describe "in Let Functions" letFunctionTests

        --- patterns
        , describe "with as pattern" asPatternTests
        , describe "with list pattern" listPatternTests
        , describe "with named pattern" namedPatternTests
        , describe "with record pattern" recordPatternTests
        , describe "with tuple pattern" tuplePatternTests
        , describe "with uncons pattern" unconsPatternTests
        ]


caseTests : List Test
caseTests =
    [ test "reports unused values" <|
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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        _ ->
            Nothing
        bash ->
            Nothing
"""
                    , Review.Test.error
                        { message = "Value `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        bish ->
            Nothing
        _ ->
            Nothing
"""
                    ]
    , test "should not remove all list of unused values" <|
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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `one` is not used."
                        , details = details
                        , under = "one"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        [_] -> 1
        [first, two] -> 2
        more -> 3
"""
                    , Review.Test.error
                        { message = "Value `first` is not used."
                        , details = details
                        , under = "first"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        [one] -> 1
        [_, two] -> 2
        more -> 3
"""
                    , Review.Test.error
                        { message = "Value `two` is not used."
                        , details = details
                        , under = "two"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        [one] -> 1
        [first, _] -> 2
        more -> 3
"""
                    , Review.Test.error
                        { message = "Value `more` is not used."
                        , details = details
                        , under = "more"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        [one] -> 1
        [first, two] -> 2
        _ -> 3
"""
                    ]
    , test "should not remove all uncons of unused values" <|
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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `one` is not used."
                        , details = details
                        , under = "one"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        _ :: [] -> 1
        first :: two :: [] -> 2
        _ :: _ :: more -> 3
"""
                    , Review.Test.error
                        { message = "Value `first` is not used."
                        , details = details
                        , under = "first"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        one :: [] -> 1
        _ :: two :: [] -> 2
        _ :: _ :: more -> 3
"""
                    , Review.Test.error
                        { message = "Value `two` is not used."
                        , details = details
                        , under = "two"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        one :: [] -> 1
        first :: _ :: [] -> 2
        _ :: _ :: more -> 3
"""
                    , Review.Test.error
                        { message = "Value `more` is not used."
                        , details = details
                        , under = "more"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [] -> 0
        one :: [] -> 1
        first :: two :: [] -> 2
        _ :: _ :: _ -> 3
"""
                    ]
    , test "report unused values with the same name as found in other branches" <|
        \() ->
            """
module A exposing (..)
foo =
    case value of
        Thing foo ->
            Just foo
        OtherThing foo ->
            Nothing
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `foo` is not used."
                        , details = details
                        , under = "foo"
                        }
                        |> Review.Test.atExactly { start = { row = 7, column = 20 }, end = { row = 7, column = 23 } }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case value of
        Thing foo ->
            Just foo
        OtherThing _ ->
            Nothing
"""
                    ]
    , test "report unused values with the same name as found in different functions" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        bish ->
            bish
bar =
    case bar of
        bish ->
            Nothing
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 9, column = 9 }, end = { row = 9, column = 13 } }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        bish ->
            bish
bar =
    case bar of
        _ ->
            Nothing
"""
                    ]
    ]


functionArgumentTests : List Test
functionArgumentTests =
    [ test "should not report unused arguments" <|
        \() ->
            """
module A exposing (..)
foo : Int -> String -> String -> String
foo one two three =
    three
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


lambdaArgumentTests : List Test
lambdaArgumentTests =
    [ test "should not report unused arguments" <|
        \() ->
            """
module A exposing (..)
foo =
    List.map (\\value -> Nothing) list
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


letDestructuringTests : List Test
letDestructuringTests =
    [ test "should report unused values" <|
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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `right` is not used."
                        , details = details
                        , under = "right"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    let
        ( left, _ ) =
            tupleValue
    in
    left
"""
                    ]
    , test "should report unused patterns that are aliased" <|
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
    let
        (bar) = 1
    in
    bar
"""
                    ]
    , test "should report unused values even if the same name is used in different scopes" <|
        \() ->
            """
module A exposing (..)
foo =
    case a of
      A ->
        let
            ( left, right ) =
                tupleValue
        in
        left + right
      B ->
        let
            ( left, right ) =
                tupleValue
        in
        left
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `right` is not used."
                        , details = details
                        , under = "right"
                        }
                        |> Review.Test.atExactly { start = { row = 13, column = 21 }, end = { row = 13, column = 26 } }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case a of
      A ->
        let
            ( left, right ) =
                tupleValue
        in
        left + right
      B ->
        let
            ( left, _ ) =
                tupleValue
        in
        left
"""
                    ]
    ]


letFunctionTests : List Test
letFunctionTests =
    [ test "should not report unused arguments" <|
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
                |> Review.Test.expectNoErrors
    , test "should not report unused let functions" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        value foo =
            something foo
    in
    bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]



--- PATTERN TESTS ------------------------


asPatternTests : List Test
asPatternTests =
    [ test "should report unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        ({ bish, bash } as bosh) ->
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
    case bar of
        ({bish, bash}) ->
            ( bish, bash )
"""
                    ]
    , test "should report unused patterns in an as pattern" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        ({ bish, bash } as bosh) ->
            ( bish, bosh )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        ({bish} as bosh) ->
            ( bish, bosh )
"""
                    ]
    , test "should report unused patterns and unused aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        ({ bish, bash } as bosh) ->
            bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `bash` is not used."
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        ({bish} as bosh) ->
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
    case bar of
        ({bish, bash}) ->
            bish
"""
                    ]
    , test "should report unused patterns that are aliased" <|
        \() ->
            """
module A exposing (..)
foo =
    case 1 of
        (_ as bar) -> bar
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
    case 1 of
        (bar) -> bar
"""
                    ]
    , test "should report nested unused pattern aliases" <|
        \() ->
            """
module A exposing (..)
foo =
    case maybeTupleMaybe of
        Just ( _, ( Just _ ) as bish ) ->
            bash
        _ ->
            bosh
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
    case maybeTupleMaybe of
        Just ( _, ( Just _ ) ) ->
            bash
        _ ->
            bosh
"""
                    ]
    ]


listPatternTests : List Test
listPatternTests =
    [ test "should report unused list values" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        [ first, second ] ->
            another
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `first` is not used."
                        , details = details
                        , under = "first"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [ _, second ] ->
            another
"""
                    , Review.Test.error
                        { message = "Value `second` is not used."
                        , details = details
                        , under = "second"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        [ first, _ ] ->
            another
"""
                    ]
    ]


namedPatternTests : List Test
namedPatternTests =
    [ test "should report unused named patterns" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        Just bish ->
            bash
        Nothing ->
            bosh
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        Just _ ->
            bash
        Nothing ->
            bosh
"""
                    ]
    , test "should report unused parenthesized patterns" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        Just (Bish bish) ->
            bash
        Nothing ->
            bosh
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case bar of
        Just (Bish _) ->
            bash
        Nothing ->
            bosh
"""
                    ]
    , test "should not report unused named patterns in case" <|
        \() ->
            """
module A exposing (..)
foo =
    case maybeTupleMaybe of
        Just ( Just _, (Just _) as bish ) ->
            bish
        Just _ ->
            bash
        _ ->
            bosh
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused named patterns in destructuring" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        (Singular _) = bish
        (Pair _ _) = bash
    in
    bosh
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
    let
        (_) = bish
        (Pair _ _) = bash
    in
    bosh
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
    let
        (Singular _) = bish
        (_) = bash
    in
    bosh
"""
                    ]
    , test "should report unused named patterns in destructuring tuples" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        (Singular _, Pair _ _) = bish
    in
    bosh
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
    let
        (_, Pair _ _) = bish
    in
    bosh
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
    let
        (Singular _, _) = bish
    in
    bosh
"""
                    ]
    ]


recordPatternTests : List Test
recordPatternTests =
    [ test "should replace unused record with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        { bish, bash } =
            bar
    in
    bosh
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Values `bish` and `bash` are not used."
                        , details = [ "You should either use these values somewhere, or remove them at the location I pointed at." ]
                        , under = "{ bish, bash }"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    let
        _ =
            bar
    in
    bosh
"""
                    ]
    , test "should report unused record values" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        { bish, bash, bosh } =
            bar
    in
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Values `bish` and `bosh` are not used."
                        , details = [ "You should either use these values somewhere, or remove them at the location I pointed at." ]
                        , under = "bish, bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    let
        {bash} =
            bar
    in
    bash
"""
                    ]
    , test "should report highlight the least amount of values possible" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        { bish, bash, bosh } =
            bar
    in
    bish
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Values `bash` and `bosh` are not used."
                        , details = [ "You should either use these values somewhere, or remove them at the location I pointed at." ]
                        , under = "bash, bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    let
        {bish} =
            bar
    in
    bish
"""
                    ]
    ]


tuplePatternTests : List Test
tuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        ( bish, bash, bosh ) =
            bar
    in
    bash
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `bish` is not used."
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    let
        ( _, bash, bosh ) =
            bar
    in
    bash
"""
                    , Review.Test.error
                        { message = "Value `bosh` is not used."
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    let
        ( bish, bash, _ ) =
            bar
    in
    bash
"""
                    ]
    , test "should replace unused tuple with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        ( _, _ ) =
            bar
    in
    1
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
    let
        _ =
            bar
    in
    1
"""
                    ]
    , test "should replace unused threeple with `_`" <|
        \() ->
            """
module A exposing (..)
foo =
    let
        ( _, _, _ ) =
            bar
    in
    1
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
    let
        _ =
            bar
    in
    1
"""
                    ]
    , test "should not report a tuple containing an empty list" <|
        \() ->
            """
module A exposing (..)
foo =
    case bar of
        ( [], _ ) ->
            1
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


unconsPatternTests : List Test
unconsPatternTests =
    [ test "should report unused uncons values" <|
        \() ->
            """
module A exposing (..)
foo =
    case list of
        first :: rest ->
            list
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value `first` is not used."
                        , details = details
                        , under = "first"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case list of
        _ :: rest ->
            list
"""
                    , Review.Test.error
                        { message = "Value `rest` is not used."
                        , details = details
                        , under = "rest"
                        }
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
foo =
    case list of
        first :: _ ->
            list
"""
                    ]
    ]
