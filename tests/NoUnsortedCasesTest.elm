module NoUnsortedCasesTest exposing (all)

import NoUnsortedCases
    exposing
        ( defaults
        , doNotLookPastUnsortable
        , doNotSortLiterals
        , doNotSortTypesFromDependencies
        , rule
        , sortListPatternsByLength
        , sortOnlyMatchingTypes
        , sortTypesFromDependenciesAlphabetically
        )
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnsortedCases"
        [ passes, fails, bugs ]


bugs : Test
bugs =
    describe "correctly handles"
        [ test "does not unstably sort patterns (1)" <|
            \() ->
                """module A exposing (..)


type Foo
    = A
    | B


a =
    case ( x, y, z ) of
        ( _, False, Just B ) ->
            0

        ( _, False, Nothing ) ->
            1

        ( True, True, Just A ) ->
            2

        ( _, True, _ ) ->
            3

        _ ->
            4
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "does not unstably sort patterns (2)" <|
            \() ->
                """module A exposing (..)


type Foo
    = A
    | B


a =
    case ( x, y, z ) of
        ( _, False, Nothing ) ->
            1

        ( _, False, Just B ) ->
            0

        ( True, True, Just A ) ->
            2

        ( _, True, _ ) ->
            3

        _ ->
            4
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)


type Foo
    = A
    | B


a =
    case ( x, y, z ) of
        ( _, False, Just B ) ->
            0

        ( _, False, Nothing ) ->
            1

        ( True, True, Just A ) ->
            2

        ( _, True, _ ) ->
            3

        _ ->
            4
"""
                        ]
        ]


passes : Test
passes =
    describe "does not report an error when"
        [ test "case is sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , passesCrossModule
        , passesWildcards
        , passesTransparentPatterns
        , passesTuples
        , passesLists
        , passesUncons
        , passesLiterals
        , passesTypesFromDependencies
        , passesSubpatterns
        , passesNotOnWhitelist
        ]


passesCrossModule : Test
passesCrossModule =
    describe "case is defined in another module"
        [ test "and is sorted" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "but type is not exported (this is a compile error, just for checking context management)" <|
            \() ->
                [ """module A exposing (Custom)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Bar -> "Bar"
        Foo -> "Foo"
        Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "with qualified names" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A

toString : Custom -> String
toString custom =
    case custom of
        A.Foo -> "Foo"
        A.Bar -> "Bar"
        A.Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "with qualified names disambiguating" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B

toString : Custom -> String
toString custom =
    case custom of
        A.Foo -> "Foo"
        A.Bar -> "Bar"
        A.Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "with disambiguation by import" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Bar -> "Bar"
        Foo -> "Foo"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "with local name" <|
            \() ->
                [ """module A exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module B exposing (..)

import A

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


passesWildcards : Test
passesWildcards =
    describe "with wildcards"
        [ test "and is sorted order with all pattern at end" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Baz -> "Baz"
        _ -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "and is sorted with var pattern at end" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Baz -> "Baz"
        bar -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "preserves control flow when sorting would destroy it with tuples" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (_, A) -> "A"
        (Foo, _) -> "FooNotA"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "control flow is not mangled by transitive assumption" <|
            \() ->
                """module A exposing (..)

type T
    = A
    | B
    | C String
    | D Int

foo t1 t2=
    case ( t1, t2 ) of
        ( A, _ ) ->
            Just A

        ( _, A ) ->
            Just A

        ( _, C s ) ->
            Just (C s)

        ( C s, _ ) ->
            Just (C s)

        ( B, _ ) ->
            Just B

        ( _, B ) ->
            Just B

        ( D _, D i ) ->
            Just (D i)
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type T
    = A
    | B
    | C String
    | D Int

foo t1 t2=
    case ( t1, t2 ) of
        ( A, _ ) ->
            Just A

        ( _, A ) ->
            Just A

        ( _, C s ) ->
            Just (C s)

        ( B, _ ) ->
            Just B

        ( C s, _ ) ->
            Just (C s)

        ( _, B ) ->
            Just B

        ( D _, D i ) ->
            Just (D i)
"""
                        ]
        , test "preserves control flow when sorting would destroy it with lists" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString cs =
    case cs of
        [_, Foo] -> "_Foo"
        [Foo, _] -> "Foo_"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "preserves control flow when sorting would destroy it with uncons" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString cs =
    case cs of
        _ :: Foo :: _ -> "_Foo_"
        Foo :: _ -> "Foo_"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


passesTransparentPatterns : Test
passesTransparentPatterns =
    describe "with transparent patterns"
        [ test "and is sorted with parenthesized patterns" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        (Baz) -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "and is sorted with as pattern" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        (Bar as b) -> "Bar"
        Baz -> "Baz"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


passesTuples : Test
passesTuples =
    describe "with tuples"
        [ test "and is sorted" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (Foo, A) -> "FooA"
        (Foo, B) -> "FooB"
        (Foo, C) -> "FooC"
        (Bar, A) -> "BarA"
        (Bar, B) -> "BarB"
        (Bar, C) -> "BarC"
        (Baz, A) -> "BazA"
        (Baz, B) -> "BazB"
        (Baz, C) -> "BazC"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "and is sorted with threeples" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case (custom1, custom2, custom3) of
        (Foo, A, Foo) -> "FooAFoo"
        (Foo, A, Bar) -> "FooABar"
        (Foo, A, Baz) -> "FooABaz"
        (Foo, B, Foo) -> "FooBFoo"
        (Foo, B, Bar) -> "FooBBar"
        (Foo, B, Baz) -> "FooBBaz"
        (Foo, C, Foo) -> "FooCFoo"
        (Foo, C, Bar) -> "FooCBar"
        (Foo, C, Baz) -> "FooCBaz"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "and is sorted with nested tuples" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case ((custom1, custom2), custom3) of
        ((Foo, A), Foo) -> "FooAFoo"
        ((Foo, A), Bar) -> "FooABar"
        ((Foo, A), Baz) -> "FooABaz"
        ((Foo, B), Foo) -> "FooBFoo"
        ((Foo, B), Bar) -> "FooBBar"
        ((Foo, B), Baz) -> "FooBBaz"
        ((Foo, C), Foo) -> "FooCFoo"
        ((Foo, C), Bar) -> "FooCBar"
        ((Foo, C), Baz) -> "FooCBaz"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "and is sorted with deeply nested tuples" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case ((custom1, (custom2, custom2)), custom3, (custom2, custom2)) of
        ((Foo, (A, A)), Foo, (A, A)) -> "1"
        ((Foo, (A, B)), Foo, (A, A)) -> "2"
        ((Foo, (A, C)), Foo, (A, A)) -> "3"
        ((Foo, (B, A)), Foo, (A, A)) -> "4"
        ((Foo, (B, A)), Foo, (A, C)) -> "5"
        ((Foo, (B, A)), Foo, (B, A)) -> "6"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "preserves order when moving wildcards would create compile errors" <|
            \() ->
                """module A exposing (..)

type Thing
    = Thing Int Int
    | OtherThing

toInt : Thing -> Result a -> Int
toInt foo bar =
    case ( foo, bar) of
        ( Thing _ _, _ ) ->
            1
        ( _, Ok _ ) ->
            2
        ( _, Err _ ) ->
           3
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


passesLists : Test
passesLists =
    describe "with lists"
        [ passesListsLengthFirst
        , passesListsElementwise
        , passesUncons
        ]


passesListsLengthFirst : Test
passesListsLengthFirst =
    describe "sorted length-first"
        [ test "and is sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        [Foo, Foo] -> "FooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Foo, Foo, Foo] -> "FooFooFoo"
        _ -> "Too many..."
"""
                    |> Review.Test.run
                        (defaults
                            |> sortListPatternsByLength
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "is sorted with mixed list/uncons" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        Foo :: [] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        [Bar, Foo] -> "BarFoo"
        Bar :: Bar :: [] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        Bar :: Bar :: _ -> "BarBar+"
        Bar :: _ :: _ -> "Bar++"
        Foo :: _ -> "Foo+"
        Bar :: _ -> "Bar+"
        _ -> "Too many..."
"""
                    |> Review.Test.run
                        (defaults
                            |> sortListPatternsByLength
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


passesListsElementwise : Test
passesListsElementwise =
    describe "sorted elementwise"
        [ test "is sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Foo, Foo] -> "FooFoo"
        [Foo, Foo, Foo] -> "FooFooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Baz] -> "Baz"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "is sorted with mixed list/uncons" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        Foo :: Foo :: [] -> "FooFoo"
        [Foo, Foo, Foo] -> "FooFooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        Foo :: _ -> "Foo+"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        Bar :: Bar :: _ -> "BarBar+"
        [Bar, Baz] -> "BarBaz"
        Bar :: _ :: _ -> "Bar++"
        Bar :: _ -> "Bar+"
        [Baz] -> "Baz"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


passesUncons : Test
passesUncons =
    describe "with uncons"
        [ test "is sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        Foo :: _ -> "Foo"
        Bar :: _ -> "Bar"
        Baz :: _ -> "Baz"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "is sorted including tuples" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List (Custom, Int) -> String
toString xs =
    case xs of
        (Foo, 1) :: (Foo, 2) :: _ -> "Foo"
        (Bar, 1) :: (Bar, 2) :: _ -> "Bar"
        (Baz, 1) :: (Baz, 2) :: _ -> "Baz"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "preserves order when mixed with lists when matches could be equally short in simple case" <|
            \() ->
                """module A exposing (..)

foo : List String -> String
foo list =
    case list of
        [] ->
            ""
        [ last ] ->
            last
        second :: rest ->
            second ++ rest
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "preserves order when mixed with lists when matches could be equally short in complex case" <|
            \() ->
                """module A exposing (..)

type Nonempty a
    = Nonempty a (List a)

foo : String -> Nonempty String -> String
foo fn list =
    case list of
        Nonempty first [] ->
            first
        Nonempty first [ last ] ->
            first ++ " " ++ fn ++ " " ++ last
        Nonempty first (second :: rest) ->
            first ++ ", " ++ foo fn (Nonempty second rest)
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


passesLiterals : Test
passesLiterals =
    describe "with literals"
        [ test "in literal order" <|
            \() ->
                """module A exposing (..)

toString : Int -> String
toString i =
    case i of
        0 -> "0"
        2 -> "2"
        4 -> "4"
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "in literal order in combination with other types" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> Int -> String
toString c i =
    case (c, i) of
        (Foo, 0) -> "0"
        (Bar, 0) -> "0"
        (Bar, 2) -> "2"
        (Baz, 2) -> "2"
        (_, 4) -> "4"
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


passesTypesFromDependencies : Test
passesTypesFromDependencies =
    describe "with types from dependencies"
        [ test "is sorted in declaration order" <|
            \() ->
                """module A exposing (..)

toString : Bool -> String
toString b =
    case b of
        True -> "True"
        False -> "False"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "is sorted in alphabetical order" <|
            \() ->
                """module A exposing (..)

toString : Bool -> String
toString b =
    case b of
        False -> "False"
        True -> "True"
"""
                    |> Review.Test.run
                        (defaults
                            |> sortTypesFromDependenciesAlphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "in any order when not sorting" <|
            \() ->
                """module A exposing (..)

toString : Bool -> String
toString b =
    case b of
        False -> "False"
        True -> "True"

toString2 : Bool -> String
toString2 b =
    case b of
        True -> "True"
        False -> "False"
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortTypesFromDependencies
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


passesSubpatterns : Test
passesSubpatterns =
    describe "with subpatterns"
        [ test "is sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Maybe Custom -> String
toString custom =
    case custom of
        Just Foo -> "Foo"
        Just Bar -> "Bar"
        Just Baz -> "Baz"
        Nothing -> "Nothing"
"""
                    |> Review.Test.run
                        (defaults
                            |> sortTypesFromDependenciesAlphabetically
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "with non-sortable patterns" <|
            \() ->
                """module A exposing (..)

type Container = Container Custom Int Int

type Custom = Foo | Bar | Baz

toString : Container -> String
toString c =
    case c of
        Container Foo 1 2 -> "Foo"
        Container Bar 2 1 -> "Bar"
        Container Baz 2 2 -> "Baz"
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortLiterals
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "does not sort past non-sortable patterns when specified" <|
            \() ->
                """module A exposing (..)

type Container = Container Int Custom Int

type Custom = Foo | Bar | Baz

toString : Container -> String
toString c =
    case c of
        Container 1 Baz 1 -> "Baz"
        Container 1 Foo 1 -> "Foo"
        Container 1 Bar 1 -> "Bar"
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortLiterals
                            |> doNotLookPastUnsortable
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


passesNotOnWhitelist : Test
passesNotOnWhitelist =
    describe "not on whitelist"
        [ test "not sorted" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Foo -> "Foo"
        Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "B", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "with qualified names" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A

toString : Custom -> String
toString custom =
    case custom of
        A.Bar -> "Bar"
        A.Foo -> "Foo"
        A.Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "B", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "with qualified names disambiguating" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B

toString : Custom -> String
toString custom =
    case custom of
        A.Bar -> "Bar"
        A.Foo -> "Foo"
        A.Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "B", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "with disambiguation by import" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Bar -> "Bar"
        Foo -> "Foo"
        Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "A", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "with local name" <|
            \() ->
                [ """module A exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module B exposing (..)

import A

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Bar -> "Bar"
        Foo -> "Foo"
        Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "A", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "with import name" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Bool -> Custom -> String
toString b custom =
    case (b, custom) of
        (False, Bar) -> "Bar"
        (False, Foo) -> "Foo"
        (True, Foo) -> "Foo"
        (False, Baz) -> "Baz"
        _ -> "Rest"
"""
                    |> Review.Test.run
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "A", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


fails : Test
fails =
    describe "reports an error when"
        [ test "case is not sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Bar -> "Bar"
        Baz -> "Baz"
        Foo -> "Foo"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                        ]
        , test "type is not exposed" <|
            \() ->
                """module A exposing (toString)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Bar -> "Bar"
        Baz -> "Baz"
        Foo -> "Foo"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (toString)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                        ]
        , fixesProperly
        , failsCrossModule
        , failsWildcards
        , failsTransparentPatterns
        , failsTuples
        , failsLists
        , failsUncons
        , failsLiterals
        , failsTypesFromDependencies
        , failsSubpatterns
        , failsOnWhitelist
        ]


fixesProperly : Test
fixesProperly =
    describe "automatic fixes"
        [ test "preserve comments" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    -- A block of patterns
    case custom of
        Bar ->
            -- Bar
            "Bar"

        Baz ->
            -- Baz
            "Baz"

        Foo ->
            -- Foo
            "Foo"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    -- A block of patterns
    case custom of
        Foo ->
            -- Foo
            "Foo"

        Bar ->
            -- Bar
            "Bar"

        Baz ->
            -- Baz
            "Baz"
"""
                        ]
        , test "work with multiline expression/patterns" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String -> String
toString custom string =
    -- A multiline expression
    case
        ( custom
        , string
        )
    of
        ( Bar, "A pattern" ) ->
            -- Bar
            "Bar"

        ( Baz, foo ) ->
            -- Baz
            "Baz"

        ( Foo, _ ) ->
            -- Foo
            toString
                |> toPipeline
                |> andSuch

        _ ->
            \"\"\"Multiline
        string
        expression?\"\"\"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String -> String
toString custom string =
    -- A multiline expression
    case
        ( custom
        , string
        )
    of
        ( Foo, _ ) ->
            -- Foo
            toString
                |> toPipeline
                |> andSuch

        ( Bar, "A pattern" ) ->
            -- Bar
            "Bar"

        ( Baz, foo ) ->
            -- Baz
            "Baz"

        _ ->
            \"\"\"Multiline
        string
        expression?\"\"\"
"""
                        ]
        , test "do not mangle syntax in complex cases" <|
            \() ->
                """module A exposing (..)
type A = B | C | D
foo bar =
    let
        thing =
            case bar of
                B ->
                    \\a b ->
                        case ( Nothing, Nothing ) of
                            ( Just a_, Just b_ ) ->
                                GT
                            ( Just _, Nothing ) ->
                                LT
                            ( Nothing, Just _ ) ->
                                GT
                            ( Nothing, Nothing ) ->
                                LT
                D ->
                    \\a b -> GT
                C ->
                    \\a b -> GT
    in
    thing
        """
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 13 }, end = { row = 6, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
type A = B | C | D
foo bar =
    let
        thing =
            case bar of
                B ->
                    \\a b ->
                        case ( Nothing, Nothing ) of
                            ( Just a_, Just b_ ) ->
                                GT
                            ( Just _, Nothing ) ->
                                LT
                            ( Nothing, Just _ ) ->
                                GT
                            ( Nothing, Nothing ) ->
                                LT
                C ->
                    \\a b -> GT
                D ->
                    \\a b -> GT
    in
    thing
        """
                        ]
        ]


failsCrossModule : Test
failsCrossModule =
    describe "case is defined in another module"
        [ test "is not sorted" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Baz -> "Baz"
        Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ unsortedError
                                |> Review.Test.whenFixed """module B exposing (..)

import A exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                            ]
                          )
                        ]
        , test "with qualified names" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A

toString : Custom -> String
toString custom =
    case custom of
        A.Bar -> "Bar"
        A.Foo -> "Foo"
        A.Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ unsortedError
                                |> Review.Test.whenFixed
                                    """module B exposing (..)

import A

toString : Custom -> String
toString custom =
    case custom of
        A.Foo -> "Foo"
        A.Bar -> "Bar"
        A.Baz -> "Baz"
"""
                            ]
                          )
                        ]
        , test "with qualified names disambiguating" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B

toString : Custom -> String
toString custom =
    case custom of
        A.Baz -> "Baz"
        A.Bar -> "Bar"
        A.Foo -> "Foo"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "C"
                          , [ unsortedError
                                |> Review.Test.whenFixed
                                    """module C exposing (..)

import A
import B

toString : Custom -> String
toString custom =
    case custom of
        A.Foo -> "Foo"
        A.Bar -> "Bar"
        A.Baz -> "Baz"
"""
                            ]
                          )
                        ]
        , test "with disambiguation by import" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "C"
                          , [ unsortedError
                                |> Review.Test.whenFixed
                                    """module C exposing (..)

import A
import B exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Bar -> "Bar"
        Foo -> "Foo"
"""
                            ]
                          )
                        ]
        , test "with local name" <|
            \() ->
                [ """module A exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module B exposing (..)

import A

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Bar -> "Bar"
        Foo -> "Foo"
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ unsortedError
                                |> Review.Test.whenFixed
                                    """module B exposing (..)

import A

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                            ]
                          )
                        ]
        ]


failsWildcards : Test
failsWildcards =
    describe "with wildcards"
        [ test "is not sorted with all pattern at end" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Foo -> "Foo"
        _ -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Baz -> "Baz"
        _ -> "Bar"
"""
                        ]
        , test "is not sorted with var pattern at end" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Foo -> "Foo"
        bar -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Baz -> "Baz"
        bar -> "Bar"
"""
                        ]
        , test "sorts past wildcards where possible with tuples" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (_, B) -> "B"
        (_, A) -> "A"
        (Bar, _) -> "BarNotBOrA"
        (Foo, _) -> "FooNotBOrA"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (_, A) -> "A"
        (_, B) -> "B"
        (Foo, _) -> "FooNotBOrA"
        (Bar, _) -> "BarNotBOrA"
        _ -> "Too many..."
"""
                        ]
        , test "sorts past wildcards where possible with lists" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString cs =
    case cs of
        [_, Bar] -> "_Bar"
        [_, Foo] -> "_Foo"
        [Bar, _] -> "Bar_"
        [Foo, _] -> "Foo_"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString cs =
    case cs of
        [_, Foo] -> "_Foo"
        [_, Bar] -> "_Bar"
        [Foo, _] -> "Foo_"
        [Bar, _] -> "Bar_"
        _ -> "Too many..."
"""
                        ]
        , test "sorts past wildcards where possible with uncons" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString cs =
    case cs of
        _ :: Bar :: _ -> "_Bar_"
        _ :: Foo :: _ -> "_Foo_"
        Bar :: _ -> "Bar_"
        Foo :: _ -> "Foo_"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString cs =
    case cs of
        _ :: Foo :: _ -> "_Foo_"
        _ :: Bar :: _ -> "_Bar_"
        Foo :: _ -> "Foo_"
        Bar :: _ -> "Bar_"
        _ -> "Too many..."
"""
                        ]
        ]


failsTransparentPatterns : Test
failsTransparentPatterns =
    describe "with transparent patterns"
        [ test "is not sorted with parenthesized patterns" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        (Baz) -> "Bar"
        Foo -> "Foo"
        Bar -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        (Baz) -> "Bar"
"""
                        ]
        , test "is not sorted with as pattern" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Foo -> "Foo"
        (Bar as b) -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        (Bar as b) -> "Bar"
        Baz -> "Baz"
"""
                        ]
        ]


failsTuples : Test
failsTuples =
    describe "with tuples"
        [ test "is not sorted at first level" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (Bar, A) -> "BarA"
        (Bar, B) -> "BarB"
        (Bar, C) -> "BarC"
        (Foo, A) -> "FooA"
        (Foo, B) -> "FooB"
        (Foo, C) -> "FooC"
        (Baz, A) -> "BazA"
        (Baz, B) -> "BazB"
        (Baz, C) -> "BazC"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (Foo, A) -> "FooA"
        (Foo, B) -> "FooB"
        (Foo, C) -> "FooC"
        (Bar, A) -> "BarA"
        (Bar, B) -> "BarB"
        (Bar, C) -> "BarC"
        (Baz, A) -> "BazA"
        (Baz, B) -> "BazB"
        (Baz, C) -> "BazC"
"""
                        ]
        , test "is not sorted at second level" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (Foo, A) -> "FooA"
        (Foo, C) -> "FooC"
        (Foo, B) -> "FooB"
        (Bar, A) -> "BarA"
        (Bar, B) -> "BarB"
        (Bar, C) -> "BarC"
        (Baz, A) -> "BazA"
        (Baz, B) -> "BazB"
        (Baz, C) -> "BazC"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> String
toString custom1 custom2 =
    case (custom1, custom2) of
        (Foo, A) -> "FooA"
        (Foo, B) -> "FooB"
        (Foo, C) -> "FooC"
        (Bar, A) -> "BarA"
        (Bar, B) -> "BarB"
        (Bar, C) -> "BarC"
        (Baz, A) -> "BazA"
        (Baz, B) -> "BazB"
        (Baz, C) -> "BazC"
"""
                        ]
        , test "is not sorted with threeples at third level" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case (custom1, custom2, custom3) of
        (Foo, A, Foo) -> "FooAFoo"
        (Foo, A, Bar) -> "FooABar"
        (Foo, A, Baz) -> "FooABaz"
        (Foo, B, Foo) -> "FooBFoo"
        (Foo, B, Baz) -> "FooBBaz"
        (Foo, B, Bar) -> "FooBBar"
        (Foo, C, Foo) -> "FooCFoo"
        (Foo, C, Bar) -> "FooCBar"
        (Foo, C, Baz) -> "FooCBaz"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case (custom1, custom2, custom3) of
        (Foo, A, Foo) -> "FooAFoo"
        (Foo, A, Bar) -> "FooABar"
        (Foo, A, Baz) -> "FooABaz"
        (Foo, B, Foo) -> "FooBFoo"
        (Foo, B, Bar) -> "FooBBar"
        (Foo, B, Baz) -> "FooBBaz"
        (Foo, C, Foo) -> "FooCFoo"
        (Foo, C, Bar) -> "FooCBar"
        (Foo, C, Baz) -> "FooCBaz"
        _ -> "Too many..."
"""
                        ]
        , test "is not sorted with nested tuples" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case ((custom1, custom2), custom3) of
        ((Foo, A), Foo) -> "FooAFoo"
        ((Foo, A), Bar) -> "FooABar"
        ((Foo, A), Baz) -> "FooABaz"
        ((Foo, B), Foo) -> "FooBFoo"
        ((Foo, C), Foo) -> "FooCFoo"
        ((Foo, B), Bar) -> "FooBBar"
        ((Foo, B), Baz) -> "FooBBaz"
        ((Foo, C), Bar) -> "FooCBar"
        ((Foo, C), Baz) -> "FooCBaz"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case ((custom1, custom2), custom3) of
        ((Foo, A), Foo) -> "FooAFoo"
        ((Foo, A), Bar) -> "FooABar"
        ((Foo, A), Baz) -> "FooABaz"
        ((Foo, B), Foo) -> "FooBFoo"
        ((Foo, B), Bar) -> "FooBBar"
        ((Foo, B), Baz) -> "FooBBaz"
        ((Foo, C), Foo) -> "FooCFoo"
        ((Foo, C), Bar) -> "FooCBar"
        ((Foo, C), Baz) -> "FooCBaz"
        _ -> "Too many..."
"""
                        ]
        , test "is not sorted with deeply nested tuples" <|
            \() ->
                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case ((custom1, (custom2, custom2)), custom3, (custom2, custom2)) of
        ((Foo, (A, A)), Foo, (A, A)) -> "1"
        ((Foo, (A, B)), Foo, (A, A)) -> "2"
        ((Foo, (B, A)), Foo, (A, A)) -> "4"
        ((Foo, (A, C)), Foo, (A, A)) -> "3"
        ((Foo, (B, A)), Foo, (A, C)) -> "5"
        ((Foo, (B, A)), Foo, (B, A)) -> "6"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom1 = Foo | Bar | Baz

type Custom2 = A | B | C

toString : Custom1 -> Custom2 -> Custom1 -> String
toString custom1 custom2 custom3 =
    case ((custom1, (custom2, custom2)), custom3, (custom2, custom2)) of
        ((Foo, (A, A)), Foo, (A, A)) -> "1"
        ((Foo, (A, B)), Foo, (A, A)) -> "2"
        ((Foo, (A, C)), Foo, (A, A)) -> "3"
        ((Foo, (B, A)), Foo, (A, A)) -> "4"
        ((Foo, (B, A)), Foo, (A, C)) -> "5"
        ((Foo, (B, A)), Foo, (B, A)) -> "6"
        _ -> "Too many..."
"""
                        ]
        ]


failsLists : Test
failsLists =
    describe "with lists"
        [ failsListsLengthFirst
        , failsListsElementwise
        ]


failsListsLengthFirst : Test
failsListsLengthFirst =
    describe "sorted length-first"
        [ test "is not sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        [Foo, Foo] -> "FooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar, Foo] -> "BarFoo"
        [Bar, Baz] -> "BarBaz"
        [Bar, Bar] -> "BarBar"
        [Foo, Foo, Foo] -> "FooFooFoo"
        _ -> "Too many..."
"""
                    |> Review.Test.run
                        (defaults
                            |> sortListPatternsByLength
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        [Foo, Foo] -> "FooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Foo, Foo, Foo] -> "FooFooFoo"
        _ -> "Too many..."
"""
                        ]
        , test "in elementwise order" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Foo, Foo] -> "FooFoo"
        [Foo, Foo, Foo] -> "FooFooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Baz] -> "Baz"
        _ -> "Too many..."
"""
                    |> Review.Test.run
                        (defaults
                            |> sortListPatternsByLength
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        [Foo, Foo] -> "FooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Foo, Foo, Foo] -> "FooFooFoo"
        _ -> "Too many..."
"""
                        ]
        , test "with mixed list/uncons in elementwise order" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        Foo :: Foo :: [] -> "FooFoo"
        [Foo, Foo, Foo] -> "FooFooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        Foo :: _ -> "Foo+"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        Bar :: Bar :: _ -> "BarBar+"
        [Bar, Baz] -> "BarBaz"
        Bar :: _ :: _ -> "Bar++"
        Bar :: _ -> "Bar+"
        [Baz] -> "Baz"
        _ -> "Too many..."
"""
                    |> Review.Test.run
                        (defaults
                            |> sortListPatternsByLength
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        Foo :: Foo :: [] -> "FooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Foo, Foo, Foo] -> "FooFooFoo"
        Bar :: Bar :: _ -> "BarBar+"
        Bar :: _ :: _ -> "Bar++"
        Foo :: _ -> "Foo+"
        Bar :: _ -> "Bar+"
        _ -> "Too many..."
"""
                        ]
        ]


failsListsElementwise : Test
failsListsElementwise =
    describe "sorted elementwise"
        [ test "is not sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Foo, Foo] -> "FooFoo"
        [Foo, Foo, Foo] -> "FooFooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Baz] -> "Baz"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Foo, Foo] -> "FooFoo"
        [Foo, Foo, Foo] -> "FooFooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Baz] -> "Baz"
        _ -> "Too many..."
"""
                        ]
        , test "in length-first order" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        [Foo, Foo] -> "FooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Foo, Foo, Foo] -> "FooFooFoo"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        [Foo] -> "Foo"
        [Foo, Foo] -> "FooFoo"
        [Foo, Foo, Foo] -> "FooFooFoo"
        [Foo, Bar] -> "FooBar"
        [Foo, Baz] -> "FooBaz"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        [Bar, Bar] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        [Baz] -> "Baz"
        _ -> "Too many..."
"""
                        ]
        , test "with mixed list/uncons in length-first order" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        Foo :: [] -> "Foo"
        [Bar] -> "Bar"
        [Baz] -> "Baz"
        [Bar, Foo] -> "BarFoo"
        Bar :: Bar :: [] -> "BarBar"
        [Bar, Baz] -> "BarBaz"
        Bar :: Bar :: _ -> "BarBar+"
        Bar :: _ :: _ -> "Bar++"
        Foo :: _ -> "Foo+"
        Bar :: _ -> "Bar+"
        _ -> "Too many..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        [] -> ""
        Foo :: [] -> "Foo"
        Foo :: _ -> "Foo+"
        [Bar] -> "Bar"
        [Bar, Foo] -> "BarFoo"
        Bar :: Bar :: [] -> "BarBar"
        Bar :: Bar :: _ -> "BarBar+"
        [Bar, Baz] -> "BarBaz"
        Bar :: _ :: _ -> "Bar++"
        Bar :: _ -> "Bar+"
        [Baz] -> "Baz"
        _ -> "Too many..."
"""
                        ]
        ]


failsUncons : Test
failsUncons =
    describe "with uncons"
        [ test "is not sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        Foo :: _ -> "Foo"
        Baz :: _ -> "Baz"
        Bar :: _ -> "Bar"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List Custom -> String
toString xs =
    case xs of
        Foo :: _ -> "Foo"
        Bar :: _ -> "Bar"
        Baz :: _ -> "Baz"
"""
                        ]
        , test "is not sorted including tuples" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List (Custom, Int) -> String
toString xs =
    case xs of
        (Bar, 1) :: (Bar, 2) :: _ -> "Bar"
        (Foo, 1) :: (Foo, 2) :: _ -> "Foo"
        (Baz, 1) :: (Baz, 2) :: _ -> "Baz"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : List (Custom, Int) -> String
toString xs =
    case xs of
        (Foo, 1) :: (Foo, 2) :: _ -> "Foo"
        (Bar, 1) :: (Bar, 2) :: _ -> "Bar"
        (Baz, 1) :: (Baz, 2) :: _ -> "Baz"
"""
                        ]
        ]


failsLiterals : Test
failsLiterals =
    describe "with literals"
        [ test "not in literal order with ints" <|
            \() ->
                """module A exposing (..)

toString : Int -> String
toString i =
    case i of
        2 -> "2"
        0 -> "0"
        4 -> "4"
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

toString : Int -> String
toString i =
    case i of
        0 -> "0"
        2 -> "2"
        4 -> "4"
        _ -> "Something else..."
"""
                        ]
        , test "not in literal order with hex ints" <|
            \() ->
                """module A exposing (..)

toString : Int -> String
toString i =
    case i of
        0x2 -> "2"
        0x0 -> "0"
        0xF -> "F"
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

toString : Int -> String
toString i =
    case i of
        0x0 -> "0"
        0x2 -> "2"
        0xF -> "F"
        _ -> "Something else..."
"""
                        ]
        , test "not in literal order with Strings" <|
            \() ->
                """module A exposing (..)

toString : String -> Char
toString s =
    case s of
        "A" -> 'A'
        "C" -> 'C'
        "B" -> 'B'
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

toString : String -> Char
toString s =
    case s of
        "A" -> 'A'
        "B" -> 'B'
        "C" -> 'C'
        _ -> "Something else..."
"""
                        ]
        , test "not in literal order with Chars" <|
            \() ->
                """module A exposing (..)

toString : Char -> String
toString c =
    case c of
        'A' -> "A"
        'C' -> "C"
        'B' -> "B"
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

toString : Char -> String
toString c =
    case c of
        'A' -> "A"
        'B' -> "B"
        'C' -> "C"
        _ -> "Something else..."
"""
                        ]
        , test "not in literal order in combination with other types" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> Int -> String
toString c i =
    case (c, i) of
        (Foo, 2) -> "0"
        (Foo, 0) -> "0"
        (Bar, 2) -> "2"
        (Baz, 2) -> "2"
        (_, 4) -> "4"
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> Int -> String
toString c i =
    case (c, i) of
        (Foo, 0) -> "0"
        (Foo, 2) -> "0"
        (Bar, 2) -> "2"
        (Baz, 2) -> "2"
        (_, 4) -> "4"
        _ -> "Something else..."
"""
                        ]
        , test "in literal order in combination with other types and not sorted" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> Int -> String
toString c i =
    case (c, i) of
        (Foo, 0) -> "0"
        (Baz, 2) -> "2"
        (Bar, 2) -> "2"
        (_, 4) -> "4"
        _ -> "Something else..."
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Custom -> Int -> String
toString c i =
    case (c, i) of
        (Foo, 0) -> "0"
        (Bar, 2) -> "2"
        (Baz, 2) -> "2"
        (_, 4) -> "4"
        _ -> "Something else..."
"""
                        ]
        ]


failsTypesFromDependencies : Test
failsTypesFromDependencies =
    describe "with types from dependencies"
        [ test "in alphabetical order with declaration sorting" <|
            \() ->
                """module A exposing (..)

toString : Bool -> String
toString b =
    case b of
        False -> "False"
        True -> "True"
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

toString : Bool -> String
toString b =
    case b of
        True -> "True"
        False -> "False"
"""
                        ]
        , test "in declaration order with alphabetical sorting" <|
            \() ->
                """module A exposing (..)

toString : Bool -> String
toString b =
    case b of
        True -> "True"
        False -> "False"
"""
                    |> Review.Test.run
                        (defaults
                            |> sortTypesFromDependenciesAlphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

toString : Bool -> String
toString b =
    case b of
        False -> "False"
        True -> "True"
"""
                        ]
        ]


failsSubpatterns : Test
failsSubpatterns =
    describe "with subpatterns"
        [ test "not sorted at first level" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Maybe Custom -> String
toString custom =
    case custom of
        Nothing -> "Nothing"
        Just Foo -> "Foo"
        Just Bar -> "Bar"
        Just Baz -> "Baz"
"""
                    |> Review.Test.run
                        (defaults
                            |> sortTypesFromDependenciesAlphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Maybe Custom -> String
toString custom =
    case custom of
        Just Foo -> "Foo"
        Just Bar -> "Bar"
        Just Baz -> "Baz"
        Nothing -> "Nothing"
"""
                        ]
        , test "not sorted at lower level" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Maybe Custom -> String
toString custom =
    case custom of
        Just Bar -> "Bar"
        Just Foo -> "Foo"
        Just Baz -> "Baz"
        Nothing -> "Nothing"
"""
                    |> Review.Test.run
                        (defaults
                            |> sortTypesFromDependenciesAlphabetically
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Maybe Custom -> String
toString custom =
    case custom of
        Just Foo -> "Foo"
        Just Bar -> "Bar"
        Just Baz -> "Baz"
        Nothing -> "Nothing"
"""
                        ]
        , test "with non-sortable patterns" <|
            \() ->
                """module A exposing (..)

type Container = Container Custom Int Int

type Custom = Foo | Bar | Baz

toString : Container -> String
toString c =
    case c of
        Container Bar 2 1 -> "Bar"
        Container Foo 1 2 -> "Foo"
        Container Baz 2 2 -> "Baz"
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortLiterals
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Container = Container Custom Int Int

type Custom = Foo | Bar | Baz

toString : Container -> String
toString c =
    case c of
        Container Foo 1 2 -> "Foo"
        Container Bar 2 1 -> "Bar"
        Container Baz 2 2 -> "Baz"
"""
                        ]
        , test "sorts past non-sortable patterns" <|
            \() ->
                """module A exposing (..)

type Container = Container Int {field : Bool} Custom Int

type Custom = Foo | Bar | Baz

toString : Container -> String
toString c =
    case c of
        Container 1 {field} Baz 1 -> "Baz"
        Container 1 {field} Foo 1 -> "Foo"
        Container 1 {field} Bar 1 -> "Bar"
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortLiterals
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Container = Container Int {field : Bool} Custom Int

type Custom = Foo | Bar | Baz

toString : Container -> String
toString c =
    case c of
        Container 1 {field} Foo 1 -> "Foo"
        Container 1 {field} Bar 1 -> "Bar"
        Container 1 {field} Baz 1 -> "Baz"
"""
                        ]
        , test "does not sort past non-sortable patterns when specified" <|
            \() ->
                """module A exposing (..)

type X
    = A
    | B () Int

f x =
    case x of
        B () 2 ->
            1

        B () 1 ->
            1

        A ->
            1
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotLookPastUnsortable
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type X
    = A
    | B () Int

f x =
    case x of
        A ->
            1

        B () 2 ->
            1

        B () 1 ->
            1
"""
                        ]
        ]


failsOnWhitelist : Test
failsOnWhitelist =
    describe "on whitelist"
        [ test "not sorted" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Foo -> "Foo"
        Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "A", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ unsortedError
                                |> Review.Test.whenFixed """module B exposing (..)

import A exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                            ]
                          )
                        ]
        , test "with qualified names" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

import A

toString : Custom -> String
toString custom =
    case custom of
        A.Baz -> "Baz"
        A.Foo -> "Foo"
        A.Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "A", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ unsortedError
                                |> Review.Test.whenFixed """module B exposing (..)

import A

toString : Custom -> String
toString custom =
    case custom of
        A.Foo -> "Foo"
        A.Bar -> "Bar"
        A.Baz -> "Baz"
"""
                            ]
                          )
                        ]
        , test "with qualified names disambiguating" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B

toString : Custom -> String
toString custom =
    case custom of
        A.Baz -> "Baz"
        A.Foo -> "Foo"
        A.Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "A", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "C"
                          , [ unsortedError
                                |> Review.Test.whenFixed """module C exposing (..)

import A
import B

toString : Custom -> String
toString custom =
    case custom of
        A.Foo -> "Foo"
        A.Bar -> "Bar"
        A.Baz -> "Baz"
"""
                            ]
                          )
                        ]
        , test "with disambiguation by import" <|
            \() ->
                [ """module A exposing (..)

type Custom = Foo | Bar | Baz
"""
                , """module B exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module C exposing (..)

import A
import B exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Foo -> "Foo"
        Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "B", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "C"
                          , [ unsortedError
                                |> Review.Test.whenFixed """module C exposing (..)

import A
import B exposing (Custom(..))

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Bar -> "Bar"
        Foo -> "Foo"
"""
                            ]
                          )
                        ]
        , test "with local name" <|
            \() ->
                [ """module A exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module B exposing (..)

import A

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Baz -> "Baz"
        Foo -> "Foo"
        Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "B", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ unsortedError
                                |> Review.Test.whenFixed """module B exposing (..)

import A

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        Foo -> "Foo"
        Bar -> "Bar"
        Baz -> "Baz"
"""
                            ]
                          )
                        ]
        , test "with sub module" <|
            \() ->
                [ """module A.C.Internal exposing (..)

type Custom = Baz | Bar | Foo
"""
                , """module B exposing (..)

import A.C.Internal as X

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        X.Baz -> "Baz"
        X.Foo -> "Foo"
        X.Bar -> "Bar"
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> sortOnlyMatchingTypes (matchesName ( "A.C.Internal", "Custom" ))
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ unsortedError
                                |> Review.Test.whenFixed """module B exposing (..)

import A.C.Internal as X

type Custom = Foo | Bar | Baz

toString : Custom -> String
toString custom =
    case custom of
        X.Baz -> "Baz"
        X.Bar -> "Bar"
        X.Foo -> "Foo"
"""
                            ]
                          )
                        ]
        , test "with import name" <|
            \() ->
                """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Bool -> Custom -> String
toString b custom =
    case (b, custom) of
        (False, Bar) -> "Bar"
        (False, Foo) -> "Foo"
        (True, Foo) -> "Foo"
        (False, Baz) -> "Baz"
        _ -> "Rest"
"""
                    |> Review.Test.run
                        (defaults
                            |> sortOnlyMatchingTypes
                                (\m t ->
                                    case ( m, t ) of
                                        ( "A", "Custom" ) ->
                                            True

                                        ( "Basics", "Bool" ) ->
                                            True

                                        _ ->
                                            False
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom = Foo | Bar | Baz

toString : Bool -> Custom -> String
toString b custom =
    case (b, custom) of
        (True, Foo) -> "Foo"
        (False, Foo) -> "Foo"
        (False, Bar) -> "Bar"
        (False, Baz) -> "Baz"
        _ -> "Rest"
"""
                        ]
        ]


matchesName : ( String, String ) -> String -> String -> Bool
matchesName ( expM, expT ) m t =
    m == expM && t == expT


unsortedError : Review.Test.ExpectedError
unsortedError =
    Review.Test.error
        { message = "Case patterns are not sorted."
        , details =
            [ "Case patterns were found out of order.  They should be sorted as specified in the rule configuration."
            ]
        , under = "case"
        }
