module NoUnsortedTopLevelDeclarationsTest exposing (all)

import NoUnsortedTopLevelDeclarations
    exposing
        ( alphabetically
        , exposedOrderWithPrivateFirst
        , exposedOrderWithPrivateLast
        , glueDependenciesAfterFirstDependent
        , glueDependenciesAfterLastDependent
        , glueDependenciesBeforeFirstDependent
        , glueDependenciesBeforeLastDependent
        , glueHelpersAfter
        , glueHelpersBefore
        , portsFirst
        , portsLast
        , rule
        , sortTopLevelDeclarations
        , typesFirst
        , typesLast
        )
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnsortedTopLevelDeclarations"
        [ passes
        , orderings
        , glues
        , docCommentDetectionTests
        ]


passes : Test
passes =
    describe "passes when"
        [ test "single declaration" <|
            \() ->
                """module A exposing (..)
f =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "no orderings" <|
            \() ->
                """module A exposing (..)
f =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


orderings : Test
orderings =
    describe "orderings"
        [ alphabeticallyTests
        , exposedOrderWithPrivateLastTests
        , exposedOrderWithPrivateFirstTests
        , typesFirstTests
        , typesLastTests
        , portsFirstTests
        , portsLastTests
        ]


alphabeticallyTests : Test
alphabeticallyTests =
    describe "alphabetically"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing (..)
bar =
    x
baz =
    y
foo =
    z
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unordered" <|
            \() ->
                """module A exposing (..)
{-| A
-}

{-| foo
-}
foo =
    z

{-| bar
-}
bar =
    x

{-| baz
-}
baz =
    y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing (..)
{-| A
-}

{-| bar
-}
bar =
    x

{-| baz
-}
baz =
    y

{-| foo
-}
foo =
    z
"""
                        ]
        , test "passes when ordered with types and aliases" <|
            \() ->
                """module A exposing (..)
type A
    = A

a =
    foo

b =
    bar

z =
    zed

type alias Z =
    A
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unordered with types and aliases" <|
            \() ->
                """module A exposing (..)

{-| This is fine in this order too.
-}
type Bar = Bar

bar =
    x

{-| This isn't!
-}
type alias Zed = {}
{-| foo
-}
foo =
    z
baz =
    y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing (..)

{-| This is fine in this order too.
-}
type Bar = Bar

bar =
    x

baz =
    y
{-| foo
-}
foo =
    z
{-| This isn't!
-}
type alias Zed = {}
"""
                        ]
        ]


exposedOrderWithPrivateLastTests : Test
exposedOrderWithPrivateLastTests =
    describe "exposedOrderWithPrivateLast"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "falls back to second sorting" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

z =
    zed

b =
    bar
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar

z =
    zed
"""
                        ]
        , test "fails when unsorted" <|
            \() ->
                """module A exposing
    ( a, A
    , Z
    )

{-|

@docs a, A
@docs Z

-}

b =
    bar

z =
    zed
{-| A
-}
type A
    = A

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( a, A
    , Z
    )

{-|

@docs a, A
@docs Z

-}

a =
    foo

{-| A
-}
type A
    = A
type alias Z =
    A

b =
    bar

z =
    zed
"""
                        ]
        ]


exposedOrderWithPrivateFirstTests : Test
exposedOrderWithPrivateFirstTests =
    describe "exposedOrderWithPrivateFirst"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

b =
    bar

z =
    zed

type A
    = A

a =
    foo

type alias Z =
    A
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

b =
    bar

type A
    = A

z =
    zed

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateFirst
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

b =
    bar

z =
    zed

type A
    = A

a =
    foo

type alias Z =
    A
"""
                        ]
        ]


typesFirstTests : Test
typesFirstTests =
    describe "typesFirst"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A
{-| Z
-}
type alias Z =
    A

z =
    zed

a =
    foo
{-| Port doc comment
-}
port b: String -> Cmd msg

c =
    bar
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar
{-| Port doc comment
-}
port b: String -> Cmd msg

type A
    = A

z =
    zed
{-| Z
-}
type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A
{-| Z
-}
type alias Z =
    A

z =
    zed

a =
    foo
{-| Port doc comment
-}
port b: String -> Cmd msg

c =
    bar
"""
                        ]
        ]


typesLastTests : Test
typesLastTests =
    describe "typesLast"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

z =
    zed

a =
    foo

port b: String -> Cmd msg

c =
    bar

type A
    = A

type alias Z =
    A
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesLast
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar

type A
    = A

z =
    zed

port b: String -> Cmd msg

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> typesLast
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

z =
    zed

a =
    foo

port b: String -> Cmd msg

c =
    bar

type A
    = A

type alias Z =
    A
"""
                        ]
        ]


portsFirstTests : Test
portsFirstTests =
    describe "portsFirst"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

port b: String -> Cmd msg

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar

port b: String -> Cmd msg

type A
    = A

z =
    zed

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

port b: String -> Cmd msg

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar
"""
                        ]
        ]


portsLastTests : Test
portsLastTests =
    describe "portsLast"
        [ test "passes when ordered" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar

port b: String -> Cmd msg
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsLast
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when unsorted" <|
            \() ->
                """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

c =
    bar

port b: String -> Cmd msg

type A
    = A

z =
    zed

type alias Z =
    A

a =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsLast
                            |> typesFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed """port module A exposing
    ( A, z
    , Z
    )

{-|

@docs A, z
@docs Z

-}

type A
    = A

type alias Z =
    A

z =
    zed

a =
    foo

c =
    bar

port b: String -> Cmd msg
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
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

calledInB =
    foo

b =
    bar calledInB

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "ports are not helpers" <|
            \() ->
                """port module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

port calledInB : String -> Cmd msg

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar calledInB

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is not helper if used in multiple funcs" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo calledInB

type alias Z =
    A

b =
    bar calledInB

calledInB =
    foo

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "does not glue to self" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

calledInB =
    calledInB

b =
    bar calledInB

z =
    z
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar calledInB dalledInB

z =
    zed

dalledInB =
    foo

calledInB =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

calledInB =
    foo

dalledInB =
    foo

b =
    bar calledInB dalledInB

z =
    zed
"""
                        ]
        , test "chains properly and ignores mutual dependencies" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

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
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

calledInBHelp =
    bar

calledInB =
    calledInBHelp foo

b =
    bar calledInB

mutualDep1 =
    mutualDep2

mutualDep2 =
    bar mutualDep3

mutualDep3 =
    mutualDep1

z =
    zed
"""
                        ]
        , test "handles mutual recursion when one is not viable for gluing" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    aHelp

aHelp =
    a

type alias Z =
    A

b =
    bar

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersBefore
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

aHelp =
    a

a =
    aHelp

type alias Z =
    A

b =
    bar

z =
    zed
"""
                        ]
        ]


glueHelpersAfterTests : Test
glueHelpersAfterTests =
    describe "glueHelpersAfter"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar calledInB

calledInB =
    foo

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "ports are not helpers" <|
            \() ->
                """port module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

port calledInB : String -> Cmd msg

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar calledInB

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar calledInB dalledInB

z =
    zed

dalledInB =
    foo

calledInB =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar calledInB dalledInB

calledInB =
    foo

dalledInB =
    foo

z =
    zed
"""
                        ]
        , test "chains properly and ignores mutual dependencies" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

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
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo

type alias Z =
    A

b =
    bar calledInB

calledInB =
    calledInBHelp foo

calledInBHelp =
    bar

mutualDep1 =
    mutualDep2

mutualDep2 =
    bar mutualDep3

mutualDep3 =
    mutualDep1

z =
    zed
"""
                        ]
        , test "handles mutual recursion when one is not viable for gluing" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    aHelp

type alias Z =
    A

aHelp =
    a

b =
    bar

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueHelpersAfter
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    aHelp

aHelp =
    a

type alias Z =
    A

b =
    bar

z =
    zed
"""
                        ]
        ]


glueDependenciesBeforeFirstDependentTests : Test
glueDependenciesBeforeFirstDependentTests =
    describe "glueDependenciesBeforeFirstDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

help =
    foo

a =
    foo help

type alias Z =
    A

b =
    bar help

z =
    zed help
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "ports are not dependencies" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

port help : String -> Cmd msg

type A
    = A

a =
    foo help

type alias Z =
    A

b =
    bar help

z =
    zed help
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is not a dependency if in exactly one func" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo help

type alias Z =
    A

b =
    bar

help =
    foo

z =
    zed
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

help =
    foo x

a =
    foo help

type alias Z =
    A

b =
    bar help x

x =
    y

y =
    help

z =
    zed help y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesBeforeFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed
                                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

help =
    foo x

a =
    foo help

type alias Z =
    A

x =
    y

b =
    bar help x

y =
    help

z =
    zed help y
"""
                        ]
        ]


glueDependenciesAfterFirstDependentTests : Test
glueDependenciesAfterFirstDependentTests =
    describe "glueDependenciesAfterFirstDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo help

help =
    foo

type alias Z =
    A

b =
    bar help

z =
    zed help
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesAfterFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "ports are not dependencies" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

port help : String -> Cmd msg

type A
    = A

a =
    foo help

type alias Z =
    A

b =
    bar help

z =
    zed help
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesAfterFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

help =
    foo x

a =
    foo help

type alias Z =
    A

b =
    bar help x

x =
    y

y =
    help

z =
    zed help y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesAfterFirstDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed
                                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo help

help =
    foo x

type alias Z =
    A

b =
    bar help x

x =
    y

z =
    zed help y

y =
    help
"""
                        ]
        ]


glueDependenciesBeforeLastDependentTests : Test
glueDependenciesBeforeLastDependentTests =
    describe "glueDependenciesBeforeLastDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo help

type alias Z =
    A

b =
    bar help

help =
    foo

z =
    zed help
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesBeforeLastDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "ports are not dependencies" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

port help : String -> Cmd msg

type A
    = A

a =
    foo help

type alias Z =
    A

b =
    bar help

z =
    zed help
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesBeforeLastDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

help =
    foo x

a =
    foo help

type alias Z =
    A

x =
    y

b =
    bar help x

y =
    help

z =
    zed help y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesBeforeLastDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed
                                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo help

type alias Z =
    A

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
"""
                        ]
        ]


glueDependenciesAfterLastDependentTests : Test
glueDependenciesAfterLastDependentTests =
    describe "glueDependenciesAfterLastDependent"
        [ test "passes when ordered" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo help

type alias Z =
    A

b =
    bar help

z =
    zed help

help =
    foo
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesAfterLastDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "ports are not dependencies" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

port help : String -> Cmd msg

type A
    = A

a =
    foo help

type alias Z =
    A

b =
    bar help

z =
    zed help
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> portsFirst
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesAfterLastDependent
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "fails when not ordered and removes cycles" <|
            \() ->
                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

help =
    foo x

a =
    foo help

type alias Z =
    A

x =
    y

b =
    bar help x

y =
    help

z =
    zed help y
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> exposedOrderWithPrivateLast
                            |> alphabetically
                            |> glueDependenciesAfterLastDependent
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError False
                            |> Review.Test.whenFixed
                                """module A exposing
    ( A, a
    , Z
    )

{-|

@docs A, a
@docs Z

-}

type A
    = A

a =
    foo help

type alias Z =
    A

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
"""
                        ]
        ]


docCommentDetectionTests : Test
docCommentDetectionTests =
    describe "correctly attaches doc comments"
        [ test "when before imports" <|
            \() ->
                """port module A exposing (..)

{-| before import
-}

import Dict

port z : () -> Int

a = 1
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed
                                """port module A exposing (..)

{-| before import
-}

import Dict

a = 1

port z : () -> Int
"""
                        ]
        , test "when after imports" <|
            \() ->
                """port module A exposing (..)

import Dict

{-| after import
-}
port z : () -> Int

a = 1
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed
                                """port module A exposing (..)

import Dict

a = 1

{-| after import
-}
port z : () -> Int
"""
                        ]
        , test "no import, but two before" <|
            \() ->
                """port module A exposing (..)

{-| doc1
-}

{-| doc2
-}
port z : () -> Int

a = 1
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed
                                """port module A exposing (..)

{-| doc1
-}

a = 1

{-| doc2
-}
port z : () -> Int
"""
                        ]
        , test "no import, but parsed doc commment" <|
            \() ->
                """port module A exposing (..)

{-| doc1
-}

{-| doc2
-}
a = 1

port z : () -> Int

b = 1
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed
                                """port module A exposing (..)

{-| doc1
-}

{-| doc2
-}
a = 1

b = 1

port z : () -> Int
"""
                        ]
        , test "no import, but @docs in comment" <|
            \() ->
                """port module A exposing (..)

{-|

## Expose

@docs a, b

-}

port z : () -> Int

a = 1
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed
                                """port module A exposing (..)

{-|

## Expose

@docs a, b

-}

a = 1

port z : () -> Int
"""
                        ]
        , test "no import, no @docs in comment, so assume it's for port" <|
            \() ->
                """port module A exposing (..)

{-| nothing
-}
port z : () -> Int

a = 1
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed
                                """port module A exposing (..)

a = 1

{-| nothing
-}
port z : () -> Int
"""
                        ]
        , test "multiple ports etc" <|
            \() ->
                """port module A exposing (..)

{-| module
-}

{-| z
-}
port z : () -> Int

{-| G
-}
type G = G

{-| b
-}
b = 1

port s : () -> Int

{-| f
-}
port f : () -> Int

{-| a
-}
a = 1
"""
                    |> Review.Test.run
                        (sortTopLevelDeclarations
                            |> alphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError True
                            |> Review.Test.whenFixed
                                """port module A exposing (..)

{-| module
-}

{-| a
-}
a = 1

{-| b
-}
b = 1

{-| f
-}
port f : () -> Int

{-| G
-}
type G = G

port s : () -> Int

{-| z
-}
port z : () -> Int
"""
                        ]
        ]


unsortedError : Bool -> Review.Test.ExpectedError
unsortedError portModule =
    Review.Test.error
        { message = "Top-level declarations are not sorted."
        , details =
            [ "Top-level declarations were found out of order.  They should be sorted as specified in the rule configuration." ]
        , under =
            if portModule then
                "port module"

            else
                "module"
        }
