module NoUnused.ParametersTest exposing (all)

import Elm.Project
import Json.Decode
import NoUnused.Parameters exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule
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
            """module A exposing (a, b)
a = foo 1 2 3
foo : Int -> String -> String -> String
foo one two three =
    three
b = foo 1 2 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `one` is not used"
                        , details = details
                        , under = "one"
                        }
                        |> Review.Test.whenFixed """module A exposing (a, b)
a = foo 2 3
foo : String -> String -> String
foo two three =
    three
b = foo 2 3
"""
                    , Review.Test.error
                        { message = "Parameter `two` is not used"
                        , details = details
                        , under = "two"
                        }
                        |> Review.Test.whenFixed """module A exposing (a, b)
a = foo 1 3
foo : Int -> String -> String
foo one three =
    three
b = foo 1 3
"""
                    ]
    , test "should report unused arguments and fix in call sites in other modules" <|
        \() ->
            [ """module A exposing (foo, a)
a = foo 1 2
foo : Int -> String -> String
foo one two =
    two
"""
            , """module B exposing (a)
import A
b = A.foo 1 2
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.ignoredFilesImpactResults
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Parameter `one` is not used"
                            , details = details
                            , under = "one"
                            }
                            |> Review.Test.shouldFixFiles
                                [ ( "A", """module A exposing (foo, a)
a = foo 2
foo : String -> String
foo two =
    two
""" )
                                , ( "B", """module B exposing (a)
import A
b = A.foo 2
""" )
                                ]
                        ]
                    ]
    , test "should report unused arguments but not fix if files that should be touched are ignored" <|
        -- TODO Make similar test when file ignores fixes
        \() ->
            [ """module A exposing (foo, a)
a = foo 1 2
foo : Int -> String -> String
foo one two =
    two
"""
            , """module B exposing (a)
import A
b = A.foo 1 2
"""
            ]
                |> Review.Test.runOnModules (Rule.ignoreErrorsForFiles [ "src/B.elm" ] rule)
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Parameter `one` is not used"
                            , details = details
                            , under = "one"
                            }
                            |> Review.Test.whenFixed """module A exposing (foo, a)
a = foo 1 2
foo : Int -> String -> String
foo _ two =
    two
"""
                        ]
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    Bar.one
"""
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
    , test "should not report used parameters (used in let declaration)" <|
        \() ->
            """module A exposing (..)
fn a b =
    let x = a b
    in x
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should remove nested function calls" <|
        \() ->
            """module A exposing (a)
fn unused b =
    b
a = fn (fn 1 2) 3
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
fn b =
    b
a = fn 3
"""
                    ]
    , test "should report but not remove unused arguments when they are referenced without arguments (inside a single module)" <|
        \() ->
            """module A exposing (a)
fn unused b =
    b
a = fn
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
fn _ b =
    b
a = fn
"""
                    ]
    , test "should report but not remove unused arguments when they are referenced without arguments (across modules)" <|
        \() ->
            [ """module A exposing (fn)
fn unused b =
    b
""", """module B exposing (a)
import A
a = A.fn
""" ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Parameter `unused` is not used"
                            , details = details
                            , under = "unused"
                            }
                            |> Review.Test.whenFixed """module A exposing (fn)
fn _ b =
    b
"""
                        ]
                      )
                    ]
    , test "should not report _ argument when it can't be fixed" <|
        \() ->
            """module A exposing (a)
fn _ = 1
a = List.map fn [1, 2, 3]
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report and remove _ argument when it can be fixed" <|
        \() ->
            """module A exposing (a)
fn _ = 1
a = fn 2
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `_` is not used"
                        , details = details
                        , under = "_"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
fn = 1
a = fn
"""
                    ]
    , test "should report even if there are no function calls" <|
        \() ->
            """module A exposing (a)
fn _ = 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `_` is not used"
                        , details = details
                        , under = "_"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
fn = 1
"""
                    ]
    , test "should report unused arguments even for packages" <|
        \() ->
            """
module Exposed exposing (value)
notExposed unused = 1
value = notExposed 2
"""
                |> Review.Test.runWithProjectData package rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.whenFixed """
module Exposed exposing (value)
notExposed = 1
value = notExposed
"""
                    ]
    , test "should autofix unused arguments of publicly-exposed functions of a package to _" <|
        \() ->
            """
module Exposed exposing (exposed)
exposed unused = 1
"""
                |> Review.Test.runWithProjectData package rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.whenFixed """
module Exposed exposing (exposed)
exposed _ = 1
"""
                    ]
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
    , test "should not report used arguments in lambda" <|
        \() ->
            """module A exposing (a)
a = List.any (\\e -> e.details) errors
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report _ argument in lambda" <|
        \() ->
            """module A exposing (a)
a = List.map (\\_ -> Nothing) list
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


letFunctionTests : List Test
letFunctionTests =
    [ test "should report unused arguments" <|
        \() ->
            """module A exposing (..)
foo =
    let
        one oneValue a =
            a
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    let
        one a =
            a
        two twoValue =
            2
    in
    one 3
"""
                    , Review.Test.error
                        { message = "Parameter `twoValue` is not used"
                        , details = details
                        , under = "twoValue"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    let
        one oneValue a =
            a
        two _ =
            2
    in
    one two 3
"""
                    ]
    , test "should report unused arguments (with a type annotation)" <|
        \() ->
            """module A exposing (..)
foo =
    let
        one : a -> Int
        one unused =
            1
    in
    one two
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    let
        one : Int
        one =
            1
    in
    one
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    let
        one oneValue =
            oneValue
        two _ =
            1
    in
    one two 3
"""
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
    , test "should fix only calls related to the function, and not those for a similarly named function in another scope" <|
        \() ->
            """module A exposing (..)
foo =
    let fn x = 1
    in fn x

bar =
    let fn x = 1
    in fn x
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `x` is not used"
                        , details = details
                        , under = "x"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    let fn = 1
    in fn

bar =
    let fn x = 1
    in fn x
"""
                    , Review.Test.error
                        { message = "Parameter `x` is not used"
                        , details = details
                        , under = "x"
                        }
                        |> Review.Test.atExactly { start = { row = 7, column = 12 }, end = { row = 7, column = 13 } }
                        |> Review.Test.whenFixed """module A exposing (..)
foo =
    let fn x = 1
    in fn x

bar =
    let fn = 1
    in fn
"""
                    ]
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo ({ bish, bash }) =
    ( bish, bash )
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo ({ bish } as bosh) =
    ( bish, bosh )
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo ({ bish } as bosh) =
    bish
"""
                    , Review.Test.error
                        { message = "Pattern alias `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo ({ bish, bash }) =
    bish
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo (Named ( _, ( Just bash ) )) =
    bash
"""
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
type Named = Named Int

foo : Named -> Int
foo (Named bish) =
    1

a = foo (Named 2)
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
type Named = Named Int

foo : Named -> Int
foo (Named _) =
    1

a = foo (Named 2)
"""
                    ]
    , test "should report unused nested named patterns" <|
        \() ->
            """module A exposing (..)
type Named = Named Bish
type Bish = Bish Int

foo : Named -> Int
foo (Named (Bish bish)) =
    1

a = foo (Named (Bish 2))
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
type Named = Named Bish
type Bish = Bish Int

foo : Named -> Int
foo (Named (Bish _)) =
    1

a = foo (Named (Bish 2))
"""
                    ]
    , test "should not report unused named patterns with multiple segments" <|
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
            """module A exposing (a)
a = foo
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
                        |> Review.Test.whenFixed """module A exposing (a)
a = foo
foo { bash, bosh } =
    bar
"""
                    , Review.Test.error
                        { message = "Parameter `bash` is not used"
                        , details = details
                        , under = "bash"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
a = foo
foo { bish, bosh } =
    bar
"""
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
a = foo
foo { bish, bash } =
    bar
"""
                    ]
    , test "should report only the unused record values" <|
        \() ->
            """module A exposing (a)
a = foo
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
                        |> Review.Test.whenFixed """module A exposing (a)
a = foo
foo { bash, bosh } =
    bash
"""
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
a = foo
foo { bish, bash } =
    bash
"""
                    ]
    , test "should report and autofix only when under an alias" <|
        \() ->
            """module A exposing (a)
a = foo
foo ({ unused, used } as alias_)=
    fn used alias_
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.whenFixed """module A exposing (a)
a = foo
foo ({ used } as alias_)=
    fn used alias_
"""
                    ]
    , test "should only remove an unused record value from the implementation when the field can't be found in the signature" <|
        \() ->
            """module A exposing (..)
type alias A = { unused : Int, used : Int }
foo : A -> Int
foo { unused, used } =
    used
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 4, column = 7 }, end = { row = 4, column = 13 } }
                        |> Review.Test.whenFixed """module A exposing (..)
type alias A = { unused : Int, used : Int }
foo : A -> Int
foo { used } =
    used
"""
                    ]
    , test "should remove entire argument if only one field is extracted and it's unused" <|
        \() ->
            """module A exposing (a)
a = foo { bish = 1, bash = 2 }
foo : { bish : a, bash : b } -> Int
foo { bish } =
    1
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 4, column = 7 }, end = { row = 4, column = 11 } }
                        |> Review.Test.whenFixed """module A exposing (a)
a = foo
foo : Int
foo =
    1
"""
                    ]
    , test "should autofix in type signature and calls and report only the unused record values" <|
        \() ->
            """module A exposing (a)
foo : { bish : a, bash : b } -> b
foo { bish, bash } =
    bash

a = foo { bish = 1, bash = 2 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : { bash : b } -> b
foo { bash } =
    bash

a = foo { bash = 2 }
"""
                    ]
    , test "should report and autofix unused field (last field)" <|
        \() ->
            """module A exposing (a)
foo : { bash : b, bish : a } -> b
foo { bash, bish } =
    bash

a = foo { bash = 2, bish = 1 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 17 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : { bash : b} -> b
foo { bash } =
    bash

a = foo { bash = 2}
"""
                    ]
    , test "should report and autofix unused parameter (field but only field)" <|
        \() ->
            """module A exposing (a)
foo : { bish : a } -> b
foo { bish } =
    1

a = foo { bish = 1 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : b
foo =
    1

a = foo
"""
                    ]
    , test "should autofix extensible record in type signature (first)" <|
        \() ->
            """module A exposing (a)
foo : { z | bish : a, bash : b } -> b
foo { bish, bash } =
    bash

a = foo { bish = 1, bash = 2 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : { z | bash : b } -> b
foo { bash } =
    bash

a = foo { bash = 2 }
"""
                    ]
    , test "should autofix extensible record in type signature (last)" <|
        \() ->
            """module A exposing (a)
foo : { z | bash : b, bish : a } -> b
foo { bish, bash } =
    bash

a = foo { bish = 1, bash = 2 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : { z | bash : b} -> b
foo { bash } =
    bash

a = foo { bash = 2 }
"""
                    ]
    , test "should autofix extensible record in type signature (only, remove entire parameter)" <|
        \() ->
            """module A exposing (a)
foo : { z | bish : a } -> b
foo { bish } =
    1

a = foo { bish = 1, bash = 2 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : b
foo =
    1

a = foo
"""
                    ]
    , test "should only autofix the declaration if argument is a record update, not the type annotation" <|
        \() ->
            -- This would be possible, but we'd have to also fix the reference in the record being updated
            -- (In this example, that would be `b`)
            """module A exposing (a)
foo : { bish : a, bash : b } -> b
foo { bish, bash } =
    bash

b = { bish = 1, bash = 2 }
a = foo { b | bish = 3, bash = 4 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : { bish : a, bash : b } -> b
foo { bash } =
    bash

b = { bish = 1, bash = 2 }
a = foo { b | bish = 3, bash = 4 }
"""
                    ]
    , test "should remove unused record fields even if under parentheses" <|
        \() ->
            """module A exposing (a)
foo : { unused : a, used : b } -> b
foo ({ unused, used }) =
    fn used

fn x = x
a = foo { unused = 1, used = 2 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : { used : b } -> b
foo ({ used }) =
    fn used

fn x = x
a = foo { used = 2 }
"""
                    ]
    , test "should remove unused record fields even if under tuple" <|
        \() ->
            """module A exposing (a)
foo : ( { unused : a, used : b }, Int ) -> b
foo ( { unused, used }, n ) =
    fn used n

fn x = x
a = foo ( { unused = 1, used = 2 }, 3 )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 9 }, end = { row = 3, column = 15 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : ( { used : b }, Int ) -> b
foo ( { used }, n ) =
    fn used n

fn x = x
a = foo ( { used = 2 }, 3 )
"""
                    ]
    , test "should only remove unused record fields from declaration if under an alias" <|
        \() ->
            """module A exposing (a)
foo : { unused : a, used : b } -> b
foo ({ unused, used } as alias_) =
    fn used alias_

fn x = x
a = foo { unused = 1, used = 2 }
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } }
                        |> Review.Test.whenFixed """module A exposing (a)
foo : { unused : a, used : b } -> b
foo ({ used } as alias_) =
    fn used alias_

fn x = x
a = foo { unused = 1, used = 2 }
"""
                    ]
    ]


functionTuplePatternTests : List Test
functionTuplePatternTests =
    [ test "should report unused tuple values" <|
        \() ->
            """module A exposing (..)
foo : ( a, b, c ) -> b
foo ( bish, bash, bosh ) =
    bash

a = foo ( 1, 2, 3 )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Parameter `bish` is not used"
                        , details = details
                        , under = "bish"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo : ( b, c ) -> b
foo ( bash, bosh ) =
    bash

a = foo ( 2, 3 )
"""
                    , Review.Test.error
                        { message = "Parameter `bosh` is not used"
                        , details = details
                        , under = "bosh"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo : ( a, b ) -> b
foo ( bish, bash ) =
    bash

a = foo ( 1, 2 )
"""
                    ]
    , test "should report unused tuple" <|
        \() ->
            """module A exposing (..)
foo ( _, _ ) =
    bar

a = foo ( 1, 2 )
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
    bar

a = foo
"""
                    ]
    , test "should report but not fix unused nested tuple" <|
        \() ->
            """module A exposing (..)
foo ( bar, ( _, _ ) ) =
    bar

a = foo ( 1, ( 2, 3 ) )
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Tuple pattern is not needed"
                        , details = [ "You should remove this pattern." ]
                        , under = "( _, _ )"
                        }
                        |> Review.Test.whenFixed """module A exposing (..)
foo ( bar ) =
    bar

a = foo ( 1 ) 
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo ( _ ) =
    bar
"""
                    ]
    , test "should report unused threeple" <|
        \() ->
            """module A exposing (..)
foo ( _, _, _ ) =
    bar

a = foo ( 1, 2, 3 )
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
    bar

a = foo
"""
                    ]
    ]



-- RECURSIVE PARAMETERS


recursiveParameterTests : List Test
recursiveParameterTests =
    [ test "should report parameters that are only used to be passed to the function itself" <|
        \() ->
            """module A exposing (a)
foo x unused =
    if cond then
        x
    else
        foo (x - 1) unused
a = foo 1 2
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
                        |> Review.Test.whenFixed """module A exposing (a)
foo x =
    if cond then
        x
    else
        foo (x - 1)
a = foo 1
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
bar x =
    if cond then
        x
    else
        bar (x - 1)
"""
                    ]
    , test "should report parameters that are only used to be passed to the function itself (complex expression in the same position)" <|
        \() ->
            """module A exposing (..)
bar x unused =
    if cond then
        x
    else
        bar (x - 1) { unused = unused }
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
                        |> Review.Test.whenFixed """module A exposing (..)
bar x =
    if cond then
        x
    else
        bar (x - 1)
"""
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
        bar (x - 1) { unused = unused }
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
                        |> Review.Test.whenFixed """module A exposing (..)
bar x =
    if cond then
        x
    else
        bar (x - 1)
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo x =
    if cond then
        x
    else
        foo (x - 1)
"""
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
                        |> Review.Test.whenFixed """module A exposing (..)
foo x =
    if cond then
        x
    else
        foo (x - 1)
"""
                    ]
    , test "should not report recursive parameter when it's the only argument, as it leads to a compiler error" <|
        \() ->
            """module A exposing (crash)
crash arg =
    crash arg
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


package : Project
package =
    Project.new
        |> Project.addElmJson (createPackageElmJson ())


createPackageElmJson : () -> { path : String, raw : String, project : Elm.Project.Project }
createPackageElmJson () =
    case Json.Decode.decodeString Elm.Project.decoder rawPackageElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawPackageElmJson
            , project = elmJson
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


rawPackageElmJson : String
rawPackageElmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}"""
