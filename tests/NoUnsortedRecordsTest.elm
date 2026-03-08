module NoUnsortedRecordsTest exposing (all)

import NoUnsortedRecords
    exposing
        ( defaults
        , doNotSortAmbiguousRecords
        , doNotSortUnknownRecords
        , reportAmbiguousRecordsWithoutFix
        , reportUnknownRecordsWithoutFix
        , rule
        , sortGenericFieldsLast
        , treatAllSubrecordsAsCanonical
        , treatCustomTypeRecordsAsCanonical
        , treatSubrecordsAsUnknown
        , typecheckAllRecords
        )
import Review.Project exposing (addDependency)
import Review.Test
import Review.Test.Dependencies exposing (elmParser, projectWithElmCore)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnsortedRecords"
        [ unknownRecords
        , ambiguousRecords
        , withAlias
        , inTypeAnnotations
        , inExpressions
        , inPatterns
        , avoidBadFixes
        , disambiguatesByHasAllFields
        , disambiguatesByTypeAnnotation
        , customTypeArgs
        , disambiguatesByKnownFunctionArgTypes
        , typeVarSupport
        , recordConstructorSupport
        , usesRecordFieldTypes
        , operatorSupport
        , dependencySupport
        , genericRecordSupport
        , localBindingSupport
        , simpleTypeInferenceSupport
        , subrecords
        , typecheckUnambiguous
        ]


unknownRecords : Test
unknownRecords =
    describe "unknown records"
        [ test "passes unknown record that is alphabetical" <|
            \() ->
                """module A exposing (..)

a = { a = 1, b = 2, c = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "fails unknown record that is not alphabetical" <|
            \() ->
                """module A exposing (..)

a = { c = 3, b = 2, a = 1 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

a = { a = 1 , b = 2, c = 3}
"""
                        ]
        , test "passes unknown record that is not alphabetical with option" <|
            \() ->
                """module A exposing (..)

a = { c = 3, b = 2, a = 1 }
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortUnknownRecords
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "reports unknown record that is not alphabetical with option" <|
            \() ->
                """module A exposing (..)

a = { c = 3, b = 2, a = 1 }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportUnknownRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unknownRecordError "{ c = 3, b = 2, a = 1 }" ]
        , test "does not report unknown record with single field even with option" <|
            \() ->
                """module A exposing (..)

a = { c = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportUnknownRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "does not report unit record even with option" <|
            \() ->
                """module A exposing (..)

a = {}
"""
                    |> Review.Test.run
                        (defaults
                            |> reportUnknownRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


ambiguousRecords : Test
ambiguousRecords =
    describe "ambiguous records"
        [ test "passes ambiguous record that is alphabetical" <|
            \() ->
                """module A exposing (..)

type alias A = { b : Int, c : Int, a : Int }
type alias B = { c : Int, a : Int, b : Int }

a = { a = 1, b = 2, c = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "fails ambiguous record that is not alphabetical" <|
            \() ->
                """module A exposing (..)

type alias A = { b : Int, c : Int, a : Int }
type alias B = { c : Int, a : Int, b : Int }

a = { b = 2, a = 1, c = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { b : Int, c : Int, a : Int }
type alias B = { c : Int, a : Int, b : Int }

a = { a = 1, b = 2, c = 3 }
"""
                        ]
        , test "passes ambiguous record that is not alphabetical with option" <|
            \() ->
                """module A exposing (..)

type alias A = { b : Int, c : Int, a : Int }
type alias B = { c : Int, a : Int, b : Int }

a = { b = 2, a = 1, c = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortAmbiguousRecords
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "passes unambiguous record (because of identical field orders)" <|
            \() ->
                """module A exposing (..)

type alias A = { b : Int, c : Int, a : Int }
type alias B = { b : Int, c : Int, a : Int }

a = { b = 2, c = 3, a = 1 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "passes records with same sorting but different indices (due to missing fields)" <|
            \() ->
                """module A exposing (..)

type alias A =
    { a : Int
    , b : String
    , c : Bool
    }
type alias B =
    { b : String, c : Bool }

a r info = { r | b = info.b, c = info.c }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "reports ambiguous record that is not alphabetical with option" <|
            \() ->
                """module A exposing (..)

type alias A = { b : Int, c : Int, a : Int }
type alias B = { c : Int, a : Int, b : Int }

a = { b = 2, a = 1, c = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors [ ambiguousRecordError [ "A.A", "A.B" ] "{ b = 2, a = 1, c = 3 }" ]
        , test "does not report ambiguous records with a single field even with option" <|
            \() ->
                """module A exposing (..)

type alias A = { b : Int, c : Int, a : Int }
type alias B = { c : Int, a : Int, b : Int }

a r =
    case r of
        { b } ->
            True
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


withAlias : Test
withAlias =
    describe "record corresponds to a known alias"
        [ test "fields are in sorted order with type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

a : A
a = { foo = 1, bar = 2, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "fields are in sorted order without type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

a = { foo = 1, bar = 2, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "fields are in order to multiple matching aliases" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { foo : Int, bar : Int, baz : Int }

a = { foo = 1, bar = 2, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "fields are not in sorted order with type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

a : A
a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

a : A
a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "fields are not in sorted order without type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "fields are not in order to multiple matching aliases with degenerate orders" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { foo : Int, bar : Int, baz : Int }

a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { foo : Int, bar : Int, baz : Int }

a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "does not keep unexposed aliases" <|
            \() ->
                [ """module B exposing (foo)

type alias A = { foo : Int, bar : Int, baz : Int }

foo : A -> Bool
foo a = True
"""
                , """module A exposing (..)

import B

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = { bar = 2, foo = 1, baz = 3 }
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


inExpressions : Test
inExpressions =
    describe "expressions are sorted"
        [ test "sorts expressions" <|
            \() ->
                """module A exposing (..)

a = { b = 1, c = 2, a = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

a = { a = 3 , b = 1, c = 2}
"""
                        ]
        , test "sorts multiline expressions" <|
            \() ->
                """module A exposing (..)

a =
    { b = 1
    , c = 2
    , a = 3
    }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

a =
    { a = 3
    
    , b = 1, c = 2
    }
"""
                        ]
        ]


inTypeAnnotations : Test
inTypeAnnotations =
    describe "type annotations are sorted (when not custom types or type aliases)"
        [ test "sorts type annotations" <|
            \() ->
                """module A exposing (..)

a : { b : Int, c : Int, a : Int } -> Bool
a _ = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

a : { a : Int , b : Int, c : Int} -> Bool
a _ = True
"""
                        ]
        , test "sorts multiline type annotations" <|
            \() ->
                """module A exposing (..)

a :
    { b : Int
    , c : Int
    , a : Int
    }
    -> Bool
a _ =
    True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

a :
    { a : Int
    
    , b : Int, c : Int
    }
    -> Bool
a _ =
    True
"""
                        ]
        ]


inPatterns : Test
inPatterns =
    describe "patterns are sorted"
        [ test "sorts patterns" <|
            \() ->
                """module A exposing (..)

a { foo, baz, bar } = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed """module A exposing (..)

a { bar, baz, foo } = True
"""
                        ]
        ]


avoidBadFixes : Test
avoidBadFixes =
    describe "avoids bad fixes"
        [ test "do not splice onto the ends of comments" <|
            \() ->
                """module A exposing (..)

type alias Rec =
    { foo : Int
    , bar : Int
    , baz : Int
    }

test : Rec -> Rec
test r =
    { r
        | bar = 1
        -- comment
        , foo = 2
        , baz = 3
    }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 11, column = 5 }, end = { row = 11, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias Rec =
    { foo : Int
    , bar : Int
    , baz : Int
    }

test : Rec -> Rec
test r =
    { r
        | foo = 2
        , bar = 1
        -- comment
        , baz = 3
    }
"""
                        ]
        ]


disambiguatesByHasAllFields : Test
disambiguatesByHasAllFields =
    describe "disambiguates by whether all fields must be present"
        [ test "passes" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int, extra : Int }

a = { foo = 1, bar = 2, baz = 3}
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "fails" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int, extra : Int }

a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int, extra : Int }

a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "does not match any when all fields must be present" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a = { baz = 3, bar = 2 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a = { bar = 2 , baz = 3}
"""
                        ]
        , test "is ambiguous with record updates" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a r = { r | baz = 3, bar = 2, foo = 1 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 7 }, end = { row = 7, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a r = { r | bar = 2, baz = 3, foo = 1 }
"""
                        ]
        , test "is ambiguous with record updates not sorting" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a r = { r | baz = 3, bar = 2, foo = 1 }
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortAmbiguousRecords
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "patterns do not match any when all fields must be present" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a : { baz : Int, bar : Int } -> Bool
a _ = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a : { bar : Int , baz : Int} -> Bool
a _ = True
"""
                        ]
        , test "patterns match when all fields must be present" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a : { bar : Int, foo : Int, baz : Int } -> Bool
a _ = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a : { foo : Int, baz : Int , bar : Int} -> Bool
a _ = True
"""
                        ]
        , test "is ambiguous with generic records" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a : { r | baz : Int, bar : Int, foo : Int } -> Bool
a _ = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a : { r | bar : Int, baz : Int, foo : Int } -> Bool
a _ = True
"""
                        ]
        , test "is ambiguous with generic records not sorting" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, baz : Int, bar : Int }

type alias B = { baz : Int, bar : Int, foo : Int, extra : Int }

a : { r | baz : Int, bar : Int, foo : Int } -> Bool
a _ = True
"""
                    |> Review.Test.run
                        (defaults
                            |> doNotSortAmbiguousRecords
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


disambiguatesByTypeAnnotation : Test
disambiguatesByTypeAnnotation =
    describe "disambiguates using type annotation"
        [ test "disambiguation is possible because of type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : A
a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : A
a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "disambiguation is possible because of type annotation with type vars" <|
            \() ->
                """module A exposing (..)

type alias A a = { foo : a, bar : Int, baz : Int }

type alias B a = { bar : Int, foo : a, baz : Int }

a : A Int
a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A a = { foo : a, bar : Int, baz : Int }

type alias B a = { bar : Int, foo : a, baz : Int }

a : A Int
a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "disambiguation is possible because of complex type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : Int -> String -> ( Int, String, List A )
a i s = ( i, s, [ { bar = 2, foo = 1, baz = 3 } ] )
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 19 }, end = { row = 8, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : Int -> String -> ( Int, String, List A )
a i s = ( i, s, [ { foo = 1, bar = 2, baz = 3 } ] )
"""
                        ]
        , test "disambiguation of pattern is possible because of type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : Int -> A -> Int -> Bool
a i1 { bar, foo, baz } i2 = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 6 }, end = { row = 8, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : Int -> A -> Int -> Bool
a i1 { foo, bar, baz } i2 = True
"""
                        ]
        , test "disambiguation of a pattern that matches nothing is possible because of type annotation" <|
            \() ->
                """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar : { bar : Int, foo : Int } -> Int
fooBar { foo, bar } = foo + bar
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar : { bar : Int, foo : Int } -> Int
fooBar { bar, foo } = foo + bar
"""
                        ]
        , test "disambiguation of a pattern that matches nothing is possible because of type annotation even when not all fields are destructured" <|
            \() ->
                """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar : { bar : Int, extra : Int, foo : Int } -> Int
fooBar { foo, bar } = foo + bar
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar : { bar : Int, extra : Int, foo : Int } -> Int
fooBar { bar, foo } = foo + bar
"""
                        ]
        , test "disambiguation of a pattern that matches nothing is possible because of type annotation even when not all fields are destructured when generic has field not in other matches" <|
            \() ->
                """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar : { r | bar : Int, extra : Int, foo : Int } -> Int
fooBar { foo, bar } = foo + bar
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar : { r | bar : Int, extra : Int, foo : Int } -> Int
fooBar { bar, foo } = foo + bar
"""
                        ]
        , test "disambiguation of a pattern that matches nothing is not possible because of generic type annotation without extra fields" <|
            \() ->
                """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar : { r | bar : Int, foo : Int } -> Int
fooBar { foo, bar } = foo + bar
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ ambiguousRecordError [ "A.BBF", "A.FBB" ] "{ foo, bar }"
                        , ambiguousRecordError [ "A.BBF", "A.FBB" ] "{ r | bar : Int, foo : Int }"
                        ]
        , test "disambiguation of a pattern that matches multiple without is not possible" <|
            \() ->
                """module A exposing (..)

type alias FBB = { foo : Int, bar : Int, baz : Int }

type alias BBF = { bar : Int, baz : Int, foo : Int }

fooBar { foo, bar } = foo + bar
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ ambiguousRecordError
                            [ "A.BBF", "A.FBB" ]
                            "{ foo, bar }"
                        ]
        ]


customTypeArgs : Test
customTypeArgs =
    describe "record is an argument of a type"
        [ test "fields are in sorted order" <|
            \() ->
                """module A exposing (..)

type A = A { foo : Int, bar : Int, baz : Int }

a = A { foo = 1, bar = 2, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "fields are not in sorted order" <|
            \() ->
                """module A exposing (..)

type A = A { foo : Int, bar : Int, baz : Int }

a = A { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type A = A { foo : Int, bar : Int, baz : Int }

a = A { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "disambiguates by constructor for expressions" <|
            \() ->
                """module A exposing (..)

type Custom
    = A { foo : Int, bar : Int, baz : Int }
    | B { bar : Int, foo : Int, baz : Int }

a = A { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 7 }, end = { row = 7, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom
    = A { foo : Int, bar : Int, baz : Int }
    | B { bar : Int, foo : Int, baz : Int }

a = A { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "disambiguates by constructor arg index for expressions" <|
            \() ->
                """module A exposing (..)

type Custom
    = A Int { foo : Int, bar : Int, baz : Int } String { bar : Int, foo : Int, baz : Int }

a = A 3 { bar = 1, foo = 2, baz = 3 } "hello" { foo = 1, bar = 2, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 9 }, end = { row = 6, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom
    = A Int { foo : Int, bar : Int, baz : Int } String { bar : Int, foo : Int, baz : Int }

a = A 3 { foo = 2, bar = 1, baz = 3 } "hello" { foo = 1, bar = 2, baz = 3 }
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 47 }, end = { row = 6, column = 48 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom
    = A Int { foo : Int, bar : Int, baz : Int } String { bar : Int, foo : Int, baz : Int }

a = A 3 { bar = 1, foo = 2, baz = 3 } "hello" { bar = 2, foo = 1, baz = 3 }
"""
                        ]
        , test "disambiguates by constructor arg index for patterns" <|
            \() ->
                """module A exposing (..)

type Custom
    = A Int { foo : Int, bar : Int, baz : Int } String { bar : Int, foo : Int, baz : Int }


a custom =
    case custom of
        A _ { bar, foo } _ {foo, bar} -> False
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 9, column = 13 }, end = { row = 9, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom
    = A Int { foo : Int, bar : Int, baz : Int } String { bar : Int, foo : Int, baz : Int }


a custom =
    case custom of
        A _ { foo, bar } _ {foo, bar} -> False
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 9, column = 28 }, end = { row = 9, column = 29 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type Custom
    = A Int { foo : Int, bar : Int, baz : Int } String { bar : Int, foo : Int, baz : Int }


a custom =
    case custom of
        A _ { bar, foo } _ {bar, foo} -> False
"""
                        ]
        ]


disambiguatesByKnownFunctionArgTypes : Test
disambiguatesByKnownFunctionArgTypes =
    describe "disambiguates by known function arg types"
        [ test "possible because of type annotation in same module" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = foo { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 10, column = 12 }, end = { row = 10, column = 13 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = foo { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "possible because of type annotation in other module" <|
            \() ->
                [ """module B exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

foo : A -> Bool
foo a = True
"""
                , """module A exposing (..)

import B exposing (foo)

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = foo { bar = 2, foo = 1, baz = 3 }
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ unsortedError
                                |> Review.Test.atExactly { start = { row = 8, column = 12 }, end = { row = 8, column = 13 } }
                                |> Review.Test.whenFixed """module A exposing (..)

import B exposing (foo)

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = foo { foo = 1, bar = 2, baz = 3 }
"""
                            ]
                          )
                        ]
        , test "possible because of type annotation in indirect import" <|
            \() ->
                [ """module C exposing (..)

type alias C = { foo : Int, bar : Int, baz : Int }
"""
                , """module B exposing (..)

import C exposing (C)

type B = B C
"""
                , """module A exposing (..)

import B exposing (B(..))

b : B
b = B { bar = 2, foo = 1, baz = 3 }
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ unsortedError
                                |> Review.Test.atExactly { start = { row = 6, column = 7 }, end = { row = 6, column = 8 } }
                                |> Review.Test.whenFixed """module A exposing (..)

import B exposing (B(..))

b : B
b = B { foo = 1, bar = 2, baz = 3 }
"""
                            ]
                          )
                        ]
        , test "does not keep unexposed functions" <|
            \() ->
                [ """module B exposing (A)

type alias A = { foo : Int, bar : Int, baz : Int }

foo : A -> Bool
foo a = True
"""
                , """module A exposing (..)

import B

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = B.foo { bar = 2, foo = 1, baz = 3 }
"""
                ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> doNotSortAmbiguousRecords
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "does understand unexposed alias from an exposed function" <|
            \() ->
                [ """module B exposing (foo)

type alias A = { foo : Int, bar : Int, baz : Int }

foo : A -> Bool
foo a = True
"""
                , """module A exposing (..)

import B exposing (foo)

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = foo { foo = 1, bar = 2, baz = 3 }
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "possible because of type annotation in custom type pattern" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

type Custom = Custom A

func : Custom -> Bool
func c =
    case c of
        Custom { bar, foo, baz } -> True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 11, column = 16 }, end = { row = 11, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

type Custom = Custom A

func : Custom -> Bool
func c =
    case c of
        Custom { foo, bar, baz } -> True
"""
                        ]
        , test "does understand unexposed alias from an exposed constructor" <|
            \() ->
                [ """module B exposing (Custom(..))

type alias A = { foo : Int, bar : Int, baz : Int }

type Custom = Custom A
"""
                , """module A exposing (..)

import B exposing (Custom(..))

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = Custom { foo = 1, bar = 2, baz = 3 }
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "does not understand unexposed constructor" <|
            \() ->
                [ """module B exposing (A)

type alias A = { foo : Int, bar : Int, baz : Int }

type Custom = Custom A
"""
                , """module A exposing (..)

import B

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = B.Custom { bar = 2, baz = 3, foo = 1}
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "does understand imported record constructor" <|
            \() ->
                [ """module B exposing (A)

type alias A = { a : { foo : Int, bar : Int, baz : Int } }

type Custom = Custom A
"""
                , """module A exposing (..)

import B exposing (A)

type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func = A { foo = 1, bar = 2, baz = 3}
"""
                ]
                    |> Review.Test.runOnModules (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


typeVarSupport : Test
typeVarSupport =
    describe "handles type variables"
        [ test "in custom types" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : Maybe A
a = Just { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 10 }, end = { row = 8, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

a : Maybe A
a = Just { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "in non-dependency custom types" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

type Custom a = Custom a

a : Custom A
a = Custom { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 10, column = 12 }, end = { row = 10, column = 13 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

type Custom a = Custom a

a : Custom A
a = Custom { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "in aliases" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

type alias Mebbe a = a

a : Mebbe A
a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 10, column = 5 }, end = { row = 10, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

type alias B = { bar : Int, foo : Int, baz : Int }

type alias Mebbe a = a

a : Mebbe A
a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "can match with type vars" <|
            \() ->
                """module A exposing (..)

type alias A a = { foo : a, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : { foo : String, bar : Int, baz : Int }
func =
    { bar = 1, foo = "foo", baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A a = { foo : a, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : { foo : String, bar : Int, baz : Int }
func =
    { foo = "foo", bar = 1, baz = 3 }
"""
                        ]
        , test "can assign type vars" <|
            \() ->
                """module A exposing (..)

type alias A a b = { foo : a, bar : b, baz : Maybe Int }
type alias B a = { bar : a, foo : a, baz : Maybe Int }

func : { foo : String, bar : Int, baz : Maybe a }
func =
    { bar = 1, foo = "foo", baz = Nothing }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A a b = { foo : a, bar : b, baz : Maybe Int }
type alias B a = { bar : a, foo : a, baz : Maybe Int }

func : { foo : String, bar : Int, baz : Maybe a }
func =
    { foo = "foo", bar = 1, baz = Nothing }
"""
                        ]
        , test "does not recurse infinitely by a typevar being assigned to itself" <|
            \() ->
                """module A exposing (..)

type alias C z = { y : z }

c : C a -> a
c = .y

foo : { a | field : Int } -> C x -> Int
foo _ _ = 0

record =
    { f = foo (c 1) (c 2) }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


recordConstructorSupport : Test
recordConstructorSupport =
    describe "using record constructors"
        [ test "possible because of type annotation of field for record constructor" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

type alias C = { a : A, b : B }

func =
    C { bar = 1, foo = 2, baz = 3 } { foo = 2, bar = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 9, column = 7 }, end = { row = 9, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

type alias C = { a : A, b : B }

func =
    C { foo = 2, bar = 1, baz = 3 } { foo = 2, bar = 1, baz = 3 }
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 9, column = 37 }, end = { row = 9, column = 38 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

type alias C = { a : A, b : B }

func =
    C { bar = 1, foo = 2, baz = 3 } { bar = 1, foo = 2, baz = 3 }
"""
                        ]
        ]


usesRecordFieldTypes : Test
usesRecordFieldTypes =
    describe "uses record field types"
        [ test "can disambiguate by field type" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : {a : A, b : B}
func =
    { a = { bar = 1, foo = 2, baz = 3 }, b = { foo = 1, bar = 2, baz = 3 } }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 11 }, end = { row = 8, column = 12 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : {a : A, b : B}
func =
    { a = { foo = 2, bar = 1, baz = 3 }, b = { foo = 1, bar = 2, baz = 3 } }
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 46 }, end = { row = 8, column = 47 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : {a : A, b : B}
func =
    { a = { bar = 1, foo = 2, baz = 3 }, b = { bar = 2, foo = 1, baz = 3 } }
"""
                        ]
        , test "can disambiguate by field type without alias" <|
            \() ->
                """module A exposing (..)

type alias A = { a : { foo : Int, bar : Int, baz : Int }, b : { bar : Int, foo : Int, baz : Int } }

func : A
func =
    { a = { bar = 1, foo = 2, baz = 3 }, b = { foo = 1, bar = 2, baz = 3 } }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 11 }, end = { row = 7, column = 12 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { a : { foo : Int, bar : Int, baz : Int }, b : { bar : Int, foo : Int, baz : Int } }

func : A
func =
    { a = { foo = 2, bar = 1, baz = 3 }, b = { foo = 1, bar = 2, baz = 3 } }
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 46 }, end = { row = 7, column = 47 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { a : { foo : Int, bar : Int, baz : Int }, b : { bar : Int, foo : Int, baz : Int } }

func : A
func =
    { a = { bar = 1, foo = 2, baz = 3 }, b = { bar = 2, foo = 1, baz = 3 } }
"""
                        ]
        , test "can disambiguate by type" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : String, foo : Int, baz : Int }

func : { bar : Int, foo : Int, baz : Int } -> Bool
func { bar, foo, baz } = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : String, foo : Int, baz : Int }

func : { foo : Int, bar : Int, baz : Int } -> Bool
func { bar, foo, baz } = True
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : String, foo : Int, baz : Int }

func : { bar : Int, foo : Int, baz : Int } -> Bool
func { foo, bar, baz } = True
"""
                        ]
        , test "can disambiguate by type via record access" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : String, foo : Int, baz : Int }

func : Int
func = { bar = 1, foo = 2, baz = 3 }.bar
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 8 }, end = { row = 7, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : String, foo : Int, baz : Int }

func : Int
func = { foo = 2, bar = 1, baz = 3 }.bar
"""
                        ]
        , test "can disambiguate by type via record access function" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : String, foo : Int, baz : Int }

func : Int
func = { bar = 1, foo = 2, baz = 3 } |> .bar
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 8 }, end = { row = 7, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : String, foo : Int, baz : Int }

func : Int
func = { foo = 2, bar = 1, baz = 3 } |> .bar
"""
                        ]
        , test "can disambiguate by type with record types" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : { a : Int, b : String }, bar : Int, baz : Int }
type alias B = { bar : Int, foo : {a : String, b : String }, baz : Int }

func : { bar : Int, foo : { a : Int, b : String }, baz : Int } -> Bool
func { bar, foo, baz } = True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : { a : Int, b : String }, bar : Int, baz : Int }
type alias B = { bar : Int, foo : {a : String, b : String }, baz : Int }

func : { foo : { a : Int, b : String }, bar : Int, baz : Int } -> Bool
func { bar, foo, baz } = True
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : { a : Int, b : String }, bar : Int, baz : Int }
type alias B = { bar : Int, foo : {a : String, b : String }, baz : Int }

func : { bar : Int, foo : { a : Int, b : String }, baz : Int } -> Bool
func { foo, bar, baz } = True
"""
                        ]
        ]


operatorSupport : Test
operatorSupport =
    describe "operator support"
        [ test "understands |> operator application" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = { bar = 2, foo = 1, baz = 3 } |> foo
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 10, column = 8 }, end = { row = 10, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = { foo = 1, bar = 2, baz = 3 } |> foo
"""
                        ]
        , test "handles parentheses" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = { bar = 2, foo = 1, baz = 3 } |> (foo)
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 10, column = 8 }, end = { row = 10, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = { foo = 1, bar = 2, baz = 3 } |> (foo)
"""
                        ]
        , test "understands <| operator application" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = foo <| { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 10, column = 15 }, end = { row = 10, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a = True

func : Bool
func = foo <| { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        ]


dependencySupport : Test
dependencySupport =
    describe "dependency support"
        [ test "will sort based on type alias in dependency with annotation" <|
            \() ->
                """module A exposing (..)

import Parser exposing (DeadEnd, Problem (..))

a : DeadEnd
a = { problem = BadRepeat, col = 1, row = 2 }
"""
                    |> Review.Test.runWithProjectData (projectWithElmCore |> addDependency elmParser) (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Parser exposing (DeadEnd, Problem (..))

a : DeadEnd
a = { row = 2 , col = 1, problem = BadRepeat}
"""
                        ]
        , test "will sort based on type alias in dependency without" <|
            \() ->
                """module A exposing (..)

import Parser exposing (DeadEnd, Problem (..))

a = { problem = BadRepeat, col = 1, row = 2 }
"""
                    |> Review.Test.runWithProjectData (projectWithElmCore |> addDependency elmParser) (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Parser exposing (DeadEnd, Problem (..))

a = { row = 2 , col = 1, problem = BadRepeat}
"""
                        ]
        , test "will disambiguate based on dependency function signature (including list type)" <|
            \() ->
                """module A exposing (..)

import Parser exposing (deadEndsToString, DeadEnd, Problem (..))

type alias MyDeadEnd = { col : Int, row : Int, problem : Problem }

a = deadEndsToString [ { problem = BadRepeat, col = 1, row = 2 } ]
"""
                    |> Review.Test.runWithProjectData (projectWithElmCore |> addDependency elmParser) (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 24 }, end = { row = 7, column = 25 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Parser exposing (deadEndsToString, DeadEnd, Problem (..))

type alias MyDeadEnd = { col : Int, row : Int, problem : Problem }

a = deadEndsToString [ { row = 2 , col = 1, problem = BadRepeat} ]
"""
                        ]
        , test "will disambiguate based on field type" <|
            \() ->
                """module A exposing (..)

import Parser exposing (deadEndsToString, DeadEnd, Problem (..))

type alias MyDeadEnd = { col : Int, row : Int, problem : String }

a : { col : Int, row : Int, problem : Problem }
a = { problem = BadRepeat, col = 1, row = 2 }
"""
                    |> Review.Test.runWithProjectData (projectWithElmCore |> addDependency elmParser) (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Parser exposing (deadEndsToString, DeadEnd, Problem (..))

type alias MyDeadEnd = { col : Int, row : Int, problem : String }

a : { col : Int, row : Int, problem : Problem }
a = { row = 2 , col = 1, problem = BadRepeat}
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Parser exposing (deadEndsToString, DeadEnd, Problem (..))

type alias MyDeadEnd = { col : Int, row : Int, problem : String }

a : { row : Int, col : Int, problem : Problem }
a = { problem = BadRepeat, col = 1, row = 2 }
"""
                        ]
        ]


genericRecordSupport : Test
genericRecordSupport =
    describe "generic records"
        [ test "without additional fields" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a : Generic {}
a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a : Generic {}
a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "without additional fields without type annotation" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "with additional fields" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a : Generic { x : Int, y : Int, z : Int }
a = { x = 0, bar = 2, z = 3, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a : Generic { x : Int, y : Int, z : Int }
a = { x = 0, y = 2, z = 3, foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "with additional fields last" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a : Generic { x : Int, y : Int, z : Int }
a = { x = 0, bar = 2, z = 3, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> sortGenericFieldsLast
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a : Generic { x : Int, y : Int, z : Int }
a = { foo = 1, bar = 2, baz = 3 , x = 0, y = 2, z = 3}
"""
                        ]
        , test "with additional fields without type signature" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a = { x = 0, bar = 2, z = 3, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }

a = { x = 0, y = 2, z = 3, foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "with additional fields that have canonical order" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias A = { yi : Int, er : Int, san : Int }

a : Generic A
a = { san = 0, bar = 2, yi = 3, er = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias A = { yi : Int, er : Int, san : Int }

a : Generic A
a = { yi = 3, er = 2, san = 0, foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "with additional fields that have canonical order last" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias A = { yi : Int, er : Int, san : Int }

a : Generic A
a = { san = 0, bar = 2, yi = 3, er = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> sortGenericFieldsLast
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias A = { yi : Int, er : Int, san : Int }

a : Generic A
a = { foo = 1, bar = 2, baz = 3 , yi = 3, er = 2, san = 0}
"""
                        ]
        , test "with additional fields that have canonical order without annotation" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias A = { yi : Int, er : Int, san : Int }

a = { san = 0, bar = 2, yi = 3, er = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias A = { yi : Int, er : Int, san : Int }

a = { yi = 3, er = 2, san = 0, foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "nested generics that have canonical order" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }

a : Generic (Generic2 {})
a = { san = 0, bar = 2, yi = 3, er = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }

a : Generic (Generic2 {})
a = { yi = 3, er = 2, san = 0, foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "nested generics that have canonical order with additional nesting" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }
type alias A = { y : Int, x : Int }

a : Generic (Generic2 A)
a = { san = 0, bar = 2, x = 6, yi = 3, er = 2, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }
type alias A = { y : Int, x : Int }

a : Generic (Generic2 A)
a = { y = 2, x = 6, yi = 3, er = 2, san = 0, foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "nested generics that have canonical order with additional nesting at end" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }
type alias A = { y : Int, x : Int }

a : Generic (Generic2 A)
a = { san = 0, bar = 2, x = 6, yi = 3, er = 2, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> sortGenericFieldsLast
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }
type alias A = { y : Int, x : Int }

a : Generic (Generic2 A)
a = { foo = 1, bar = 2, baz = 3 , yi = 3, er = 2, san = 0, y = 2, x = 6}
"""
                        ]
        , test "nested generics without canonical order" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }

a : Generic (Generic2 { x : Int, y : Int })
a = { san = 0, bar = 2, x = 6, yi = 3, er = 2, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }

a : Generic (Generic2 { x : Int, y : Int })
a = { x = 6, y = 2, yi = 3, er = 2, san = 0, foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "nested generics without canonical order ambiguous" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }

a = { san = 0, bar = 2, x = 6, yi = 3, er = 2, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ ambiguousRecordError
                            [ "A.Generic (A.Generic2)", "A.Generic2 (A.Generic)" ]
                            "{ san = 0, bar = 2, x = 6, yi = 3, er = 2, y = 2, foo = 1, baz = 3 }"
                        ]
        , test "nested generics without canonical order report only" <|
            \() ->
                """module A exposing (..)

type alias Generic rec = { rec | foo : Int, bar : Int, baz : Int }
type alias Generic2 rec = { rec | yi : Int, er : Int, san : Int }

a : Generic (Generic2 { x : Int, y : Int })
a = { san = 0, bar = 2, x = 6, yi = 3, er = 2, y = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportUnknownRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unknownRecordError "{ san = 0, bar = 2, x = 6, yi = 3, er = 2, y = 2, foo = 1, baz = 3 }"
                        , unknownRecordError "{ x : Int, y : Int }"
                        ]
        , test "prefers canonical match over generic + unknown" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias Generic rec = { rec | bar : Int, foo : Int }

a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias Generic rec = { rec | bar : Int, foo : Int }

a = { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "does not prefer canonical match over generic + canonical" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { baz : Int }
type alias Generic rec = { rec | bar : Int, foo : Int }

a = { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ ambiguousRecordError [ "A.A", "A.Generic (A.B)" ] "{ bar = 2, foo = 1, baz = 3 }" ]
        , test "does not recurse infinitely when not all fields must be present" <|
            \() ->
                """module A exposing (..)

type alias Generic a =
    { a | unrelated : Int, foo : Int }

func x =
    case x of
        { bar, baz, foo } ->
            True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "handles weird Elm nested generic behavior" <|
            \() ->
                """module A exposing (..)

type alias Gen1 a =
    { a | y : Int, x : Int }


type alias Gen2 b =
    { b | x : Char, y : String }

type alias OtherRec = { y : String, x : Char }

a : Gen2 (Gen1 { x : String, y : Float })
a =
    { y = "bar", x = 'a' }
"""
                    |> Review.Test.run
                        (defaults
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 14, column = 5 }, end = { row = 14, column = 6 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias Gen1 a =
    { a | y : Int, x : Int }


type alias Gen2 b =
    { b | x : Char, y : String }

type alias OtherRec = { y : String, x : Char }

a : Gen2 (Gen1 { x : String, y : Float })
a =
    { x = 'a' , y = "bar"}
"""
                        ]
        ]


localBindingSupport : Test
localBindingSupport =
    describe "disambiguates using local bindings"
        [ test "possible because of type annotation in let block" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func =
    let
        foo : A -> Bool
        foo _ = True
    in
    foo { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 12, column = 9 }, end = { row = 12, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : Bool
func =
    let
        foo : A -> Bool
        foo _ = True
    in
    foo { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "possible because of type annotation on function arg" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : (A -> Bool) -> Bool
func foo = foo { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 16 }, end = { row = 7, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : (A -> Bool) -> Bool
func foo = foo { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "possible because of lambda arg" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : (A -> Bool) -> Bool
func = \\foo -> foo { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 20 }, end = { row = 7, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : (A -> Bool) -> Bool
func = \\foo -> foo { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "works with record pattern" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : { field : Int, foo : (A -> Bool) } -> Bool
func { field, foo } = foo { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 7, column = 27 }, end = { row = 7, column = 28 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : { field : Int, foo : (A -> Bool) } -> Bool
func { field, foo } = foo { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        , test "works with named pattern" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }
type Custom = Custom In (A -> Bool)

func : Custom -> Bool
func (Custom field foo) = foo { bar = 2, foo = 1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 31 }, end = { row = 8, column = 32 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }
type Custom = Custom In (A -> Bool)

func : Custom -> Bool
func (Custom field foo) = foo { foo = 1, bar = 2, baz = 3 }
"""
                        ]
        ]


simpleTypeInferenceSupport : Test
simpleTypeInferenceSupport =
    describe "infers record field types"
        [ test "can disambiguate by type" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : List (List Int), baz : Char }
type alias B = { bar : List (List String), foo : Int, baz : Char }

func = { bar = [ [], ([3, 4]) ], foo = 3, baz = '2' }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : List (List Int), baz : Char }
type alias B = { bar : List (List String), foo : Int, baz : Char }

func = { foo = 3, bar = [ [], ([3, 4]) ], baz = '2' }
"""
                        ]
        , test "does not incorrectly infer Nothings" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Maybe Int, bar : Maybe Float }

a = { foo = Nothing, bar = Nothing }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "does not incorrectly infer nested Nothings" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Maybe Int, bar : Maybe Float }, er : Int }

a = { yi = { foo = Nothing, bar = Nothing }, er = 2 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "infers lambdas" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int -> Int, baz : Char }
type alias B = { bar : Int, foo : Int, baz : Char }

func = { bar = \\i -> i + 1, foo = 3, baz = '2' }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int -> Int, baz : Char }
type alias B = { bar : Int, foo : Int, baz : Char }

func = { foo = 3, bar = \\i -> i + 1, baz = '2' }
"""
                        ]
        , test "can disambiguate by type with record types" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : { a : Int, b : String }, bar : Int, baz : Int }
type alias B = { bar : Int, foo : {a : String, b : String }, baz : Int }

func = { bar = 2, foo = { a = 3, b = "b" }, baz = { a = 2, b = "r" }.a }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : { a : Int, b : String }, bar : Int, baz : Int }
type alias B = { bar : Int, foo : {a : String, b : String }, baz : Int }

func = { foo = { a = 3, b = "b" }, bar = 2, baz = { a = 2, b = "r" }.a }
"""
                        ]
        , test "infers let destructuring types" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : String, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func =
    let
        { bar, foo, baz } = A 1 "string" 3
    in
    { foo = foo, bar = bar, baz = baz }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 9 }, end = { row = 8, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : String, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func =
    let
        { foo, bar, baz } = A 1 "string" 3
    in
    { foo = foo, bar = bar, baz = baz }
"""
                        ]
        , test "infers case pattern types" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : String, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : String -> Bool
func s =
    case {foo = 1, bar = s, baz = 2} of
        {bar, foo, baz} ->
            True
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 9, column = 9 }, end = { row = 9, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : String, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : String -> Bool
func s =
    case {foo = 1, bar = s, baz = 2} of
        {foo, bar, baz} ->
            True
"""
                        ]
        , test "assigns case pattern bindings" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : String, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : String -> Bool
func s =
    case s of
        "True" -> True
        "False" -> False
        str -> {bar = str, foo = 1, baz = 2}
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 11, column = 16 }, end = { row = 11, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : String, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : String -> Bool
func s =
    case s of
        "True" -> True
        "False" -> False
        str -> {foo = 1, bar = str, baz = 2}
"""
                        ]
        , test "infers record update with binding" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

r : A
r = { foo = 1, bar = 2, baz = 3 }

func =
    { r | baz = 1, foo = 2 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 10, column = 5 }, end = { row = 10, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

r : A
r = { foo = 1, bar = 2, baz = 3 }

func =
    { r | foo = 2 , baz = 1}
"""
                        ]
        , test "unifies record types" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : String, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func r =
    case List.head [ { r | baz = 1 }, { r | bar = 1 }, { r | foo = "string" } ] of
        Just { bar, baz, foo } -> True
        Nothing -> False
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 8, column = 14 }, end = { row = 8, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias A = { foo : String, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func r =
    case List.head [ { r | baz = 1 }, { r | bar = 1 }, { r | foo = "string" } ] of
        Just { foo, bar, baz } -> True
        Nothing -> False
"""
                        ]
        , test "assigns type vars when necessary" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : A
func =
    identity { foo = 1, bar = 2, baz = 3}
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "assigns type vars when necessary 2" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }
type alias B = { bar : Int, foo : Int, baz : Int }

func : a -> A
func =
    always { foo = 1, bar = 2, baz = 3}
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        ]


subrecords : Test
subrecords =
    describe "subrecords"
        [ test "are sorted by default in larger record" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { er = 1, yi = { bar = 2, foo = 1, baz = 3 } }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { yi = { bar = 2, foo = 1, baz = 3 } , er = 1}
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { er = 1, yi = { foo = 1, bar = 2, baz = 3 } }
"""
                        ]
        , test "are not sorted by default from constructor when not part of constructor" <|
            \() ->
                """module A exposing (..)

type A = A { foo : Int, bar : Int, baz : Int }

func = { bar = 2, baz = 3, foo = 1 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "are sorted from constructor when not part of constructor with setting" <|
            \() ->
                """module A exposing (..)

type alias Rec = { yi : { bar : Int, baz : Int, foo : Int }, er : Int }

type A = A { foo : Int, bar : Int, baz : Int }

func = { foo = 1, bar = 2, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatCustomTypeRecordsAsCanonical
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "are sorted by default in larger record with type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : A
func = { er = 1, yi = { bar = 2, foo = 1, baz = 3 } }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : A
func = { yi = { bar = 2, foo = 1, baz = 3 } , er = 1}
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 23 }, end = { row = 6, column = 24 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : A
func = { er = 1, yi = { foo = 1, bar = 2, baz = 3 } }
"""
                        ]
        , test "are sorted by default in sub sub record" <|
            \() ->
                """module A exposing (..)

type alias A = { outer : { yi : { foo : Int, bar : Int, baz : Int }, er : Int } }

func = { outer = { er = 1, yi = { bar = 2, foo = 1, baz = 3 } } }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { outer : { yi : { foo : Int, bar : Int, baz : Int }, er : Int } }

func = { outer = { yi = { bar = 2, foo = 1, baz = 3 } , er = 1} }
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 33 }, end = { row = 5, column = 34 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { outer : { yi : { foo : Int, bar : Int, baz : Int }, er : Int } }

func = { outer = { er = 1, yi = { foo = 1, bar = 2, baz = 3 } } }
"""
                        ]
        , test "are sorted by default in larger record with nested expression" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : ( Int, List { foo : Int, bar : Int, baz : Int }), er : Int }

func = { yi = (0, [ { bar = 2, foo = 1, baz = 3 } ]), er = 1 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 21 }, end = { row = 5, column = 22 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : ( Int, List { foo : Int, bar : Int, baz : Int }), er : Int }

func = { yi = (0, [ { foo = 1, bar = 2, baz = 3 } ]), er = 1 }
"""
                        ]
        , test "are sorted by default in type annotations" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : { er : Int, yi : { bar : Int, foo : Int, baz : Int } }
func = { yi = { foo = 1, bar = 2, baz = 3 }, er = 1 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : { yi : { bar : Int, foo : Int, baz : Int } , er : Int}
func = { yi = { foo = 1, bar = 2, baz = 3 }, er = 1 }
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : { er : Int, yi : { foo : Int, bar : Int, baz : Int } }
func = { yi = { foo = 1, bar = 2, baz = 3 }, er = 1 }
"""
                        ]
        , test "are sorted by default in sub sub record of type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { outer : { yi : { foo : Int, bar : Int, baz : Int }, er : Int } }

func : { outer : { er : Int, yi : { bar : Int, foo : Int, baz : Int } } }
func = { outer = { yi = { foo = 1, bar = 2, baz = 3 }, er = 1 } }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { outer : { yi : { foo : Int, bar : Int, baz : Int }, er : Int } }

func : { outer : { yi : { bar : Int, foo : Int, baz : Int } , er : Int} }
func = { outer = { yi = { foo = 1, bar = 2, baz = 3 }, er = 1 } }
"""
                        , unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 35 }, end = { row = 5, column = 36 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { outer : { yi : { foo : Int, bar : Int, baz : Int }, er : Int } }

func : { outer : { er : Int, yi : { foo : Int, bar : Int, baz : Int } } }
func = { outer = { yi = { foo = 1, bar = 2, baz = 3 }, er = 1 } }
"""
                        ]
        , test "are sorted by default in larger record with nested expression in type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : ( Int, List { foo : Int, bar : Int, baz : Int }), er : Int }

func : { yi : (Int, List { bar : Int, foo : Int, baz : Int }), er : Int }
func = { yi = (0, []), er = 1 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 26 }, end = { row = 5, column = 27 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : ( Int, List { foo : Int, bar : Int, baz : Int }), er : Int }

func : { yi : (Int, List { foo : Int, bar : Int, baz : Int }), er : Int }
func = { yi = (0, []), er = 1 }
"""
                        ]
        , test "are not sorted with setting in larger record" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { er = 1, yi = { bar = 2, baz = 3, foo = 1 } }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatSubrecordsAsUnknown
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { yi = { bar = 2, baz = 3, foo = 1 } , er = 1}
"""
                        ]
        , test "are not sorted with setting in larger record with type annotation" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : A
func = { er = 1, yi = { bar = 2, baz = 3, foo = 1 } }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatSubrecordsAsUnknown
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func : A
func = { yi = { bar = 2, baz = 3, foo = 1 } , er = 1}
"""
                        ]
        , test "are not sorted for custom types with setting" <|
            \() ->
                """module A exposing (..)

type Custom
    = A { foo : Int, bar : Int, baz : Int }
    | B { bar : Int, foo : Int, baz : Int }

a = A { bar = 2, baz = 3, foo = 1 }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatSubrecordsAsUnknown
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "are sorted with setting when not in context from alias" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { bar = 2, baz = 3, foo = 1 }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatAllSubrecordsAsCanonical
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { foo = 1 , bar = 2, baz = 3}
"""
                        ]
        , test "are sorted with setting when not in context from constructor" <|
            \() ->
                """module A exposing (..)

type A = A { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { bar = 2, baz = 3, foo = 1 }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatAllSubrecordsAsCanonical
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type A = A { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

func = { foo = 1 , bar = 2, baz = 3}
"""
                        ]
        , test "handle ambiguity with subrecords with setting" <|
            \() ->
                """module A exposing (..)

type Custom
    = A { foo : Int, bar : Int, baz : Int }
    | B { bar : Int, foo : Int, baz : Int }

a = { bar = 2, baz = 3, foo = 1 }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatAllSubrecordsAsCanonical
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ ambiguousRecordError [ "A.A arg0", "A.B arg0" ] "{ bar = 2, baz = 3, foo = 1 }"
                        ]
        , test "do not take priority" <|
            \() ->
                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }
type alias B = { baz : Int, bar : Int, foo : Int }

func = { foo = 1, bar = 2, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> treatAllSubrecordsAsCanonical
                            |> reportAmbiguousRecordsWithoutFix
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 6, column = 8 }, end = { row = 6, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { yi : { foo : Int, bar : Int, baz : Int }, er : Int }
type alias B = { baz : Int, bar : Int, foo : Int }

func = { baz = 3 , bar = 2, foo = 1}
"""
                        ]
        ]


typecheckUnambiguous : Test
typecheckUnambiguous =
    describe "unambiguous records"
        [ test "are not type-checked by default" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

func = { bar = 2, foo = 1.1, baz = 3 }
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

func = { foo = 1.1, bar = 2, baz = 3 }
"""
                        ]
        , test "are type-checked with option" <|
            \() ->
                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

func = { bar = 2, foo = 1.1, baz = 3 }
"""
                    |> Review.Test.run
                        (defaults
                            |> typecheckAllRecords
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ unsortedError
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type alias A = { foo : Int, bar : Int, baz : Int }

func = { bar = 2, baz = 3 , foo = 1.1}
"""
                        ]
        ]


unsortedError : Review.Test.ExpectedError
unsortedError =
    Review.Test.error
        { message = "Record fields are not sorted."
        , details =
            [ "Record fields were found out of order.  They should be sorted as specified in the rule configuration."
            ]
        , under = "{"
        }


unknownRecordError : String -> Review.Test.ExpectedError
unknownRecordError under =
    Review.Test.error
        { message = "Unknown record encountered."
        , details =
            [ "This record did not correspond with any known alias or custom type argument record, so whether or not its fields are sorted could not be determined!"
            , "Create a type alias for this record type, or remove reportUnknownRecordsWithoutFix from your rule configuration."
            ]
        , under = under
        }


ambiguousRecordError : List String -> String -> Review.Test.ExpectedError
ambiguousRecordError matching under =
    Review.Test.error
        { message = "Ambiguous record encountered."
        , details =
            [ "This record could be one of several possible record aliases, so whether or not its fields are sorted could not be determined!"
            , "Try adding a type annotation, or remove reportAmbiguousRecordsWithoutFix from your rule configuration."
            , "The record matched the following possible aliases: " ++ String.join ", " matching
            ]
        , under = under
        }
