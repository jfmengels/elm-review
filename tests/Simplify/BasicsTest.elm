module Simplify.BasicsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


all : Test
all =
    describe "Basics"
        [ identityTests
        , alwaysTests
        , toFloatTests
        , roundTests
        , ceilingTests
        , floorTests
        , truncateTests
        , absTests
        , minTests
        , maxTests
        , compareTests
        ]



-- BASICS


identityTests : Test
identityTests =
    describe "Basics.identity"
        [ test "should not report identity function on its own" <|
            \() ->
                """module A exposing (..)
a = identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace identity x by x" <|
            \() ->
                """module A exposing (..)
a = identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace f >> identity by f" <|
            \() ->
                """module A exposing (..)
a = f >> identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace identity >> f by f" <|
            \() ->
                """module A exposing (..)
a = identity >> f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace f << identity by f" <|
            \() ->
                """module A exposing (..)
a = f << identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace identity << f by f" <|
            \() ->
                """module A exposing (..)
a = identity << f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        ]


alwaysTests : Test
alwaysTests =
    describe "Basics.always"
        [ test "should not report always function on its own" <|
            \() ->
                """module A exposing (..)
a = always
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report always with 1 argument" <|
            \() ->
                """module A exposing (..)
a = always x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace always x y by x" <|
            \() ->
                """module A exposing (..)
a = always x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace always x <| y by x" <|
            \() ->
                """module A exposing (..)
a = always x <| y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace y |> always x by x" <|
            \() ->
                """module A exposing (..)
a = y |> always x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace f >> always g by always g" <|
            \() ->
                """module A exposing (..)
a = f >> always g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
"""
                        ]
        , test "should replace always g << f by always g" <|
            \() ->
                """module A exposing (..)
a = always g << f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
"""
                        ]
        ]


toFloatTests : Test
toFloatTests =
    describe "Basics.toFloat"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = toFloat
b = toFloat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify toFloat 1 to 1" <|
            \() ->
                """module A exposing (..)
a = toFloat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify toFloat -1 to -1" <|
            \() ->
                """module A exposing (..)
a = toFloat -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify toFloat 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = toFloat 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should not report toFloat 1.2" <|
            \() ->
                """module A exposing (..)
a = toFloat 1.2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


roundTests : Test
roundTests =
    describe "Basics.round"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = round
b = round n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify round 1 to 1" <|
            \() ->
                """module A exposing (..)
a = round 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify round -1 to -1" <|
            \() ->
                """module A exposing (..)
a = round -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify round 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = round 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify round 1.1 to 1" <|
            \() ->
                """module A exposing (..)
a = round 1.1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.round on a number literal can be evaluated"
                            , details = [ "You can replace this call by the resulting Int value." ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify round <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = round <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.round cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify round << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = round << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.round cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


ceilingTests : Test
ceilingTests =
    describe "Basics.ceiling"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = ceiling
b = ceiling n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify ceiling 1 to 1" <|
            \() ->
                """module A exposing (..)
a = ceiling 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify ceiling -1 to -1" <|
            \() ->
                """module A exposing (..)
a = ceiling -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify ceiling 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = ceiling 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify ceiling 0.9 to 1" <|
            \() ->
                """module A exposing (..)
a = ceiling 0.9
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.ceiling on a number literal can be evaluated"
                            , details = [ "You can replace this call by the resulting Int value." ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify ceiling <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = ceiling <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.ceiling cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify ceiling << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = ceiling << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.ceiling cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


floorTests : Test
floorTests =
    describe "Basics.floor"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = floor
b = floor n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify floor 1 to 1" <|
            \() ->
                """module A exposing (..)
a = floor 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify floor -1 to -1" <|
            \() ->
                """module A exposing (..)
a = floor -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify floor 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = floor 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify floor 1.1 to 1" <|
            \() ->
                """module A exposing (..)
a = floor 1.1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.floor on a number literal can be evaluated"
                            , details = [ "You can replace this call by the resulting Int value." ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify floor <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = floor <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.floor cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify floor << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = floor << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.floor cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


truncateTests : Test
truncateTests =
    describe "Basics.truncate"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = truncate
b = truncate n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify truncate 1 to 1" <|
            \() ->
                """module A exposing (..)
a = truncate 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify truncate -1 to -1" <|
            \() ->
                """module A exposing (..)
a = truncate -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify truncate 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = truncate 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify truncate 1.1 to 1" <|
            \() ->
                """module A exposing (..)
a = truncate 1.1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.truncate on a number literal can be evaluated"
                            , details = [ "You can replace this call by the resulting Int value." ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify truncate <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = truncate <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.truncate cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify truncate << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = truncate << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.truncate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


absTests : Test
absTests =
    describe "Basics.abs"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a0 = abs
a1 = abs n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace abs 3 by 3" <|
            \() ->
                """module A exposing (..)
a = abs 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.abs on a number literal can be evaluated"
                            , details = [ "You can replace this call by the resulting absolute value." ]
                            , under = "abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace abs 3e6 by 3e6" <|
            \() ->
                """module A exposing (..)
a = abs 3e6
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.abs on a number literal can be evaluated"
                            , details = [ "You can replace this call by the resulting absolute value." ]
                            , under = "abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3e6
"""
                        ]
        , test "should replace abs 0x1 by 0x1" <|
            \() ->
                """module A exposing (..)
a = abs 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.abs on a number literal can be evaluated"
                            , details = [ "You can replace this call by the resulting absolute value." ]
                            , under = "abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should replace abs (abs n) by abs n" <|
            \() ->
                """module A exposing (..)
a = Basics.abs (abs n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.abs after Basics.abs"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Basics.abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (abs n)
"""
                        ]
        , test "should replace Basics.abs <| abs <| f <| n by abs <| f <| n" <|
            \() ->
                """module A exposing (..)
a = Basics.abs <| abs <| f <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.abs after Basics.abs"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Basics.abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = abs <| f <| n
"""
                        ]
        , test "should replace n |> f |> abs |> Basics.abs by n |> f |> abs" <|
            \() ->
                """module A exposing (..)
a = n |> f |> abs |> Basics.abs
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.abs after Basics.abs"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Basics.abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n |> f |> abs
"""
                        ]
        , test "should replace abs << abs by abs" <|
            \() ->
                """module A exposing (..)
a = Basics.abs << abs
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.abs after Basics.abs"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Basics.abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = abs
"""
                        ]
        , test "should replace abs -n by abs n" <|
            \() ->
                """module A exposing (..)
a = abs -n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.abs on a negated value makes the negation unnecessary"
                            , details = [ "You can remove the negation of the value given to the abs call." ]
                            , under = "abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = abs n
"""
                        ]
        , test "should replace abs (negate n) by abs n" <|
            \() ->
                """module A exposing (..)
a = abs (negate n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.abs on a negated value makes the negation unnecessary"
                            , details = [ "You can remove the negation of the value given to the abs call." ]
                            , under = "abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = abs n
"""
                        ]
        , test "should replace abs (if c then x |> f |> negate else -(y |> f)) by abs (if c then x |> f else y |> f)" <|
            \() ->
                """module A exposing (..)
a = abs (if c then x |> f |> negate else -(y |> f))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.abs on a negated value makes the negation unnecessary"
                            , details = [ "You can remove the negation of the value given to the abs call." ]
                            , under = "abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = abs (if c then (x |> f) else (y |> f))
"""
                        ]
        , test "should replace abs << negate by abs" <|
            \() ->
                """module A exposing (..)
a = abs << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.negate before Basics.abs"
                            , details = [ "You can remove the composition with negate." ]
                            , under = "abs"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = abs
"""
                        ]
        ]


minTests : Test
minTests =
    describe "Basics.min"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a0 = min
a1 = min n
a2 = min n m
a3 = min 3 m
a4 = min n 4
a5 = min (min n0 n1) (min n2 n3)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace min n n by n" <|
            \() ->
                """module A exposing (..)
a = min n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.min with two equal values can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should replace min n -n by -(abs n)" <|
            \() ->
                """module A exposing (..)
a = min n -n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.min with a first value that is equal to negative the second value results in the its negative absolute value"
                            , details = [ "You can replace this call by the negated Basics.abs on either its first or second argument." ]
                            , under = "min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -(abs n)
"""
                        ]
        , test "should replace min (min n m) n by n" <|
            \() ->
                """module A exposing (..)
a = Basics.min (min n m) n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Basics.min m n
"""
                        ]
        , test "should replace min (min m n) n by n" <|
            \() ->
                """module A exposing (..)
a = Basics.min (min m n) n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Basics.min m n
"""
                        ]
        , test "should replace min n (min n m) by n" <|
            \() ->
                """module A exposing (..)
a = Basics.min n (min n m)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (min n m)
"""
                        ]
        , test "should replace min n (min m n) by n" <|
            \() ->
                """module A exposing (..)
a = Basics.min n (min m n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (min m n)
"""
                        ]
        , test "should replace min (min (min o p) n) (min m n) by min (min o p) (min m n)" <|
            \() ->
                """module A exposing (..)
a = Basics.min (min (min o p) n) (min m n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Basics.min (min o p) (min m n)
"""
                        ]
        , test "should replace min n << min n by min n" <|
            \() ->
                """module A exposing (..)
a = Basics.min n << min n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.min on Basics.min with an equal value"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = min n
"""
                        ]
        , test "should replace min n >> min n by min n" <|
            \() ->
                """module A exposing (..)
a = min n >> Basics.min n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.min on Basics.min with an equal value"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = min n
"""
                        ]
        , test "should replace min (min n m) << min n by min (min n m)" <|
            \() ->
                """module A exposing (..)
a = Basics.min (min n m) << min n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across the arguments of the composed functions"
                            , details = [ "You can remove the operation that has an equal argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Basics.min (min n m)
"""
                        ]
        , test "should replace min n << min (min n m) by min (min n m)" <|
            \() ->
                """module A exposing (..)
a = Basics.min n << min (min n m)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across the arguments of the composed functions"
                            , details = [ "You can remove the operation that has an equal argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = min (min n m)
"""
                        ]
        , test "should replace min (min n0 n1) << min (min n2 (min n3 n1)) by min n0 << min (min n2 (min n3 n1))" <|
            \() ->
                """module A exposing (..)
a = Basics.min (min n0 n1) << min (min n2 (min n3 n1))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.min contains unnecessary equal values across the arguments of the composed functions"
                            , details = [ "You can replace the inner call that has an equal argument by its other argument." ]
                            , under = "Basics.min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Basics.min n0 << min (min n2 (min n3 n1))
"""
                        ]
        , test "should replace min 3 4 by 3" <|
            \() ->
                """module A exposing (..)
a = min 3 4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.min with a first value that is less than the second value results in the first value"
                            , details = [ "You can replace this call by the its first argument." ]
                            , under = "min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace min 4 3 by 3" <|
            \() ->
                """module A exposing (..)
a = min 4 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.min with a first value that is greater than the second value results in the second value"
                            , details = [ "You can replace this call by the its second argument." ]
                            , under = "min"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        ]


maxTests : Test
maxTests =
    describe "Basics.max"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a0 = max
a1 = max n
a2 = max n m
a3 = max 3 m
a4 = max n 4
a5 = max (max n0 n1) (max n2 n3)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace max n n by n" <|
            \() ->
                """module A exposing (..)
a = max n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.max with two equal values can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "max"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should replace max n -n by abs n" <|
            \() ->
                """module A exposing (..)
a = max n -n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.max with a first value that is equal to negative the second value results in the its absolute value"
                            , details = [ "You can replace this call by Basics.abs on either its first or second argument." ]
                            , under = "max"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (abs n)
"""
                        ]
        , test "should replace max (max (max o p) n) (max m n) by max (max o p) (max m n)" <|
            \() ->
                """module A exposing (..)
a = Basics.max (max (max o p) n) (max m n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Basics.max contains unnecessary equal values across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Basics.max"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Basics.max (max o p) (max m n)
"""
                        ]
        , test "should replace max n << max n by max n" <|
            \() ->
                """module A exposing (..)
a = Basics.max n << max n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.max on Basics.max with an equal value"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Basics.max"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = max n
"""
                        ]
        , test "should replace max n >> max n by max n" <|
            \() ->
                """module A exposing (..)
a = max n >> Basics.max n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Basics.max on Basics.max with an equal value"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Basics.max"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = max n
"""
                        ]
        , test "should replace max 3 4 by 4" <|
            \() ->
                """module A exposing (..)
a = max 3 4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.max with a first value that is less than the second value results in the second value"
                            , details = [ "You can replace this call by the its second argument." ]
                            , under = "max"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 4
"""
                        ]
        , test "should replace max 4 3 by 4" <|
            \() ->
                """module A exposing (..)
a = max 4 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.max with a first value that is greater than the second value results in the first value"
                            , details = [ "You can replace this call by the its first argument." ]
                            , under = "max"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 4
"""
                        ]
        ]


compareTests : Test
compareTests =
    describe "Basics.compare"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a0 = compare
a1 = compare n
a2 = compare n m
a3 = compare 3 m
a4 = compare n 4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report compare n n when expect NaN is enabled" <|
            \() ->
                """module A exposing (..)
a = compare n n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace compare n n by EQ when expect NaN not enabled" <|
            \() ->
                """module A exposing (..)
a = compare n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.compare with two equal arguments results in EQ"
                            , details = [ "You can replace this call by EQ." ]
                            , under = "compare"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = EQ
"""
                        ]
        , test "should replace compare 3 4 by LT" <|
            \() ->
                """module A exposing (..)
a = compare 3 4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.compare with a left value less than the right results in LT"
                            , details = [ "You can replace this call by LT." ]
                            , under = "compare"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = LT
"""
                        ]
        , test "should replace compare 4 3 by GT" <|
            \() ->
                """module A exposing (..)
a = compare 4 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.compare with a left value greater than the right results in GT"
                            , details = [ "You can replace this call by GT." ]
                            , under = "compare"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = GT
"""
                        ]
        ]
