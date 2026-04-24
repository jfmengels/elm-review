module NoUnsortedLetDeclarationsTest exposing (all)

import NoUnsortedLetDeclarations
    exposing
        ( alphabetically
        , glueDependenciesAfterFirstDependent
        , glueDependenciesAfterLastDependent
        , glueDependenciesBeforeFirstDependent
        , glueDependenciesBeforeLastDependent
        , glueHelpersAfter
        , glueHelpersBefore
        , rule
        , sortLetDeclarations
        , usedInExpressionFirst
        , usedInExpressionLast
        , usedInOtherDeclarationsFirst
        , usedInOtherDeclarationsLast
        , valuesAfterFunctions
        , valuesBeforeFunctions
        )
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnsortedLetDeclarations"
        [ passes
        , orderings
        , glues
        ]


passes : Test
passes =
    describe "passes when"
        [ test "single declaration" <|
            \() ->
                """module A exposing (..)
f =
    let
        foo =
            bar
    in
    foo
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "no orderings" <|
            \() ->
                """module A exposing (..)
f =
    let
        foo =
            bar
        bar =
            baz
    in
    foo
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


orderings : Test
orderings =
    describe "orderings"
        [ alphabeticallyTests
        , usedInExpressionFirstTests
        , usedInExpressionLastTests
        , usedInOtherDeclarationsLastTests
        , usedInOtherDeclarationsFirstTests
        , valuesBeforeFunctionsTests
        , valuesAfterFunctionsTests
        ]


alphabeticallyTests : Test
alphabeticallyTests =
    describe "alphabetically"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        bar =
            x
        baz =
            y
        foo =
            z
    in
    foo |> bar |> baz
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        foo =
            z
        bar =
            x
        baz =
            y
    in
    foo |> bar |> baz
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
f =
    let
        bar =
            x
        baz =
            y
        foo =
            z
    in
    foo |> bar |> baz
"""
                        ]
        , test "passes sorted destructuring" <|
            \() ->
                """module A exposing (..)
f =
    let
        (Opaque a) =
            i

        ( z, b ) =
            j

        { y, c } =
            k

        d =
            l
    in
    x
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails unsorted destructuring" <|
            \() ->
                """module A exposing (..)
f =
    let
        ( z, b ) =
            j

        (Opaque a) =
            i

        d =
            l

        { y, c } =
            k
    in
    x
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        (Opaque a) =
            i

        ( z, b ) =
            j

        { y, c } =
            k

        d =
            l
    in
    x
"""
                        ]
        ]


usedInExpressionFirstTests : Test
usedInExpressionFirstTests =
    describe "usedInExpressionFirst"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        -- These are used in the expression
        y =
            b

        x =
            a

        -- These are not
        a =
            i

        b =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a =
            i

        b =
            j

        x =
            a

        y =
            b
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a =
            i

        b =
            j
    in
    x + y
"""
                        ]
        , test "falls back to other" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a =
            i

        b =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "falls back to other failing" <|
            \() ->
                """module A exposing (..)
f =
    let
        y =
            b

        x =
            a

        b =
            j

        a =
            i
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a =
            i

        b =
            j
    in
    x + y
"""
                        ]
        ]


usedInExpressionLastTests : Test
usedInExpressionLastTests =
    describe "usedInExpressionLast"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        -- These are not used in the expression
        x =
            i

        y =
            j

        -- These are used in the expression
        b =
            y

        a =
            x
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        b =
            y

        a =
            x

        x =
            i

        y =
            j
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            i

        y =
            j

        b =
            y

        a =
            x
    in
    a + b
"""
                        ]
        , test "falls back to other" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            i

        y =
            j

        a =
            x

        b =
            y
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "falls back to other failing" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            i

        b =
            y

        y =
            j

        a =
            x
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            i

        y =
            j

        a =
            x

        b =
            y
    in
    a + b
"""
                        ]
        ]


usedInOtherDeclarationsLastTests : Test
usedInOtherDeclarationsLastTests =
    describe "usedInOtherDeclarationsLast"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a =
            x

        b =
            y

        x =
            i

        y =
            j
    in
    0
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a =
            x

        x =
            i

        b =
            y

        y =
            j
    in
    0
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        a =
            x

        b =
            y

        x =
            i

        y =
            j
    in
    0
"""
                        ]
        ]


usedInOtherDeclarationsFirstTests : Test
usedInOtherDeclarationsFirstTests =
    describe "usedInOtherDeclarationsFirst"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            i

        y =
            j

        a =
            x

        b =
            y
    in
    0
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a =
            x

        x =
            i

        b =
            y

        y =
            j
    in
    0
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInOtherDeclarationsFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)
f =
    let
        x =
            i

        y =
            j

        a =
            x

        b =
            y
    in
    0
"""
                        ]
        ]


valuesBeforeFunctionsTests : Test
valuesBeforeFunctionsTests =
    describe "valuesBeforeFunctions"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a i =
            i

        b j =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesBeforeFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        a i =
            i

        y =
            b

        b j =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesBeforeFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
f =
    let
        x =
            a

        y =
            b

        a i =
            i

        b j =
            j
    in
    x + y
"""
                        ]
        ]


valuesAfterFunctionsTests : Test
valuesAfterFunctionsTests =
    describe "valuesAfterFunctions"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        a i =
            i

        b j =
            j

        x =
            a

        y =
            b
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesAfterFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)
f =
    let
        x =
            a

        a i =
            i

        y =
            b

        b j =
            j
    in
    x + y
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> valuesAfterFunctions
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
f =
    let
        a i =
            i

        b j =
            j

        x =
            a

        y =
            b
    in
    x + y
"""
                        ]
        ]


glues : Test
glues =
    describe "glues"
        [ glueHelpersBeforeTests
        , glueHelpersAfterTests
        , glueDependenciesBeforeFirstDependentTests
        , glueDependenciesAfterFirstDependentTests
        , glueDependenciesBeforeLastDependentTests
        , glueDependenciesAfterLastDependentTests
        ]


glueHelpersBeforeTests : Test
glueHelpersBeforeTests =
    describe "glueHelpersBefore"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        z =
            zed

        a =
            foo

        calledInB =
            foo

        b =
            bar calledInB
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is not helper if used in multiple funcs" <|
            \() ->
                """module A exposing (..)

func =
    let
        calledInB =
            foo

        z =
            zed

        a =
            foo calledInB

        b =
            bar calledInB
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is a helper if multiple bindings used in single binding" <|
            \() ->
                """module A exposing (..)

func =
    let
        (help1, help2) =
            foo

        z =
            zed

        a =
            foo

        (y, b) =
            bar help1 help2
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

func =
    let
        z =
            zed

        a =
            foo

        (help1, help2) =
            foo

        (y, b) =
            bar help1 help2
    in
    a + b
"""
                        ]
        , test "does not glue to self" <|
            \() ->
                """module A exposing (..)

func =
    let
        z =
            zed

        a =
            foo

        calledInB =
            calledInB foo

        b =
            bar calledInB
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        z =
            zed

        dalledInB =
            foo

        a =
            foo

        b =
            bar calledInB dalledInB

        calledInB =
            calledInB foo
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

func =
    let
        z =
            zed

        a =
            foo

        calledInB =
            calledInB foo

        dalledInB =
            foo

        b =
            bar calledInB dalledInB
    in
    a + b
"""
                        ]
        , test "chains properly and ignores mutual dependencies" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            foo

        mutualDep3 =
            mutualDep1

        b =
            bar calledInB

        mutualDep2 =
            bar mutualDep3

        calledInBHelp =
            bar

        mutualDep1 =
            mutualDep2

        z =
            zed

        calledInB =
            calledInBHelp foo
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

func =
    let
        mutualDep1 =
            mutualDep2

        mutualDep2 =
            bar mutualDep3

        mutualDep3 =
            mutualDep1

        z =
            zed

        a =
            foo

        calledInBHelp =
            bar

        calledInB =
            calledInBHelp foo

        b =
            bar calledInB
    in
    a + b
"""
                        ]
        , test "handles mutual recursion when one is not viable for gluing" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            aHelp

        aHelp =
            a

        b =
            bar

        z =
            zed
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

func =
    let
        z =
            zed

        aHelp =
            a

        a =
            aHelp

        b =
            bar
    in
    a + b
"""
                        ]
        ]


glueHelpersAfterTests : Test
glueHelpersAfterTests =
    describe "glueHelpersAfter"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        z =
            zed

        a =
            foo

        b =
            bar calledInB

        calledInB =
            foo
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is not helper if used in multiple funcs" <|
            \() ->
                """module A exposing (..)

func =
    let
        calledInB =
            foo

        z =
            zed

        a =
            foo calledInB

        b =
            bar calledInB
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "does not glue to self" <|
            \() ->
                """module A exposing (..)

func =
    let
        z =
            zed

        a =
            foo

        b =
            bar calledInB

        calledInB =
            calledInB foo
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        z =
            zed

        dalledInB =
            foo

        a =
            foo

        b =
            bar calledInB dalledInB

        calledInB =
            calledInB foo
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

func =
    let
        z =
            zed

        a =
            foo

        b =
            bar calledInB dalledInB

        calledInB =
            calledInB foo

        dalledInB =
            foo
    in
    a + b
"""
                        ]
        , test "chains properly and ignores mutual dependencies" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            foo

        mutualDep3 =
            mutualDep1

        b =
            bar calledInB

        mutualDep2 =
            bar mutualDep3

        calledInBHelp =
            bar

        mutualDep1 =
            mutualDep2

        z =
            zed

        calledInB =
            calledInBHelp foo
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

func =
    let
        mutualDep1 =
            mutualDep2

        mutualDep2 =
            bar mutualDep3

        mutualDep3 =
            mutualDep1

        z =
            zed

        a =
            foo

        b =
            bar calledInB

        calledInB =
            calledInBHelp foo

        calledInBHelp =
            bar
    in
    a + b
"""
                        ]
        , test "handles mutual recursion when one is not viable for gluing" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            aHelp

        aHelp =
            a

        b =
            bar

        z =
            zed
    in
    a + b
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

func =
    let
        z =
            zed

        a =
            aHelp

        aHelp =
            a

        b =
            bar
    in
    a + b
"""
                        ]
        ]


glueDependenciesBeforeFirstDependentTests : Test
glueDependenciesBeforeFirstDependentTests =
    describe "glueDependenciesBeforeFirstDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        help =
            foo

        a =
            foo help

        b =
            bar help

        z =
            zed help
    in
    a + b + z
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is not a dependency if in exactly one func" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            foo help

        b =
            bar

        help =
            foo

        z =
            zed
    in
    bar
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is a dependency if multiple bindings used in exactly one func each (not the same)" <|
            \() ->
                """module A exposing (..)

func =
    let
        (helpA, helpB) =
            foo

        a =
            foo helpA

        b =
            bar helpB

        z =
            zed
    in
    bar
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing (..)
func =
    let
        help =
            foo x

        a =
            foo help

        b =
            bar help x

        x =
            y

        y =
            help

        z =
            zed help y
    in
    a
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
func =
    let
        help =
            foo x

        a =
            foo help

        x =
            y

        b =
            bar help x

        y =
            help

        z =
            zed help y
    in
    a
"""
                        ]
        ]


glueDependenciesAfterFirstDependentTests : Test
glueDependenciesAfterFirstDependentTests =
    describe "glueDependenciesAfterFirstDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            foo help

        help =
            foo

        b =
            bar help

        z =
            zed help
    in
    a + b + z
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueDependenciesAfterFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing (..)
func =
    let
        help =
            foo x

        a =
            foo help

        b =
            bar help x

        x =
            y

        y =
            help

        z =
            zed help y
    in
    a
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> glueDependenciesAfterFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
func =
    let
        a =
            foo help

        help =
            foo x

        b =
            bar help x

        x =
            y

        z =
            zed help y

        y =
            help
    in
    a
"""
                        ]
        ]


glueDependenciesBeforeLastDependentTests : Test
glueDependenciesBeforeLastDependentTests =
    describe "glueDependenciesBeforeLastDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            foo help

        b =
            bar help

        help =
            foo

        z =
            zed help
    in
    a + b + z
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueDependenciesBeforeLastDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing (..)
func =
    let
        help =
            foo x

        a =
            foo help

        b =
            bar help x

        x =
            y

        y =
            help

        z =
            zed help y
    in
    a
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> glueDependenciesBeforeLastDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
func =
    let
        a =
            foo help

        x =
            y

        b =
            bar help x

        help =
            foo x

        y =
            help

        z =
            zed help y
    in
    a
"""
                        ]
        ]


glueDependenciesAfterLastDependentTests : Test
glueDependenciesAfterLastDependentTests =
    describe "glueDependenciesAfterLastDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)

func =
    let
        a =
            foo help

        b =
            bar help

        z =
            zed help

        help =
            foo
    in
    a + b + z
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionLast
                            |> alphabetically
                            |> glueDependenciesAfterLastDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing (..)
func =
    let
        help =
            foo x

        a =
            foo help

        b =
            bar help x

        x =
            y

        y =
            help

        z =
            zed help y
    in
    a
"""
                    |> Review.Test.run
                        (sortLetDeclarations
                            |> usedInExpressionFirst
                            |> alphabetically
                            |> glueDependenciesAfterLastDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)
func =
    let
        a =
            foo help

        b =
            bar help x

        x =
            y

        z =
            zed help y

        help =
            foo x

        y =
            help
    in
    a
"""
                        ]
        ]


unsortedError : Review.Test.ExpectedError
unsortedError =
    Review.Test.error
        { message = "Let declarations are not sorted."
        , details =
            [ "Let declarations were found out of order.  They should be sorted as specified in the rule configuration." ]
        , under = "let"
        }
