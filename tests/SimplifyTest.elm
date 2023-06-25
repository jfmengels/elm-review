module SimplifyTest exposing (all)

import Review.Rule exposing (Rule)
import Review.Test
import Simplify exposing (defaults, ignoreCaseOfForTypes, rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Simplify"
        [ configurationTests
        , qualifyTests
        , lambdaReduceTests
        , identityTests
        , alwaysTests
        , booleanTests
        , caseOfTests
        , booleanCaseOfTests
        , ifTests
        , duplicatedIfTests
        , recordUpdateTests
        , numberTests
        , fullyAppliedPrefixOperatorTests
        , appliedLambdaTests
        , usingPlusPlusTests
        , stringSimplificationTests
        , listSimplificationTests
        , maybeTests
        , resultTests
        , setSimplificationTests
        , dictSimplificationTests
        , cmdTests
        , subTests
        , parserTests
        , jsonDecodeTests
        , htmlAttributesTests
        , recordAccessTests
        , letTests
        ]


ruleWithDefaults : Rule
ruleWithDefaults =
    rule defaults



-- CONFIGURATION


configurationTests : Test
configurationTests =
    describe "Configuration"
        [ test "should not report configuration error if all ignored constructors exist" <|
            \() ->
                """module A exposing (..)
type B = B
type C = C
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe", "Result.Result" ] defaults)
                    |> Review.Test.expectNoErrors
        , test "should report configuration error if passed an invalid module name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "_.B" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `_.B`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed an invalid type name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "A.f" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `A.f`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed an empty type name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: ``"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed a type name without a module name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "B" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `B`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed multiple invalid types" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "_.B", "A.f", "B", "Maybe.Maybe" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `A.f`, `B`, `_.B`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report global error if ignored types were not found in the project" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "A.B", "B.C" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `A.B`, `B.C`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should not report global error if ignored type was found in the dependencies" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] defaults)
                    |> Review.Test.expectNoErrors
        ]



-- QUALIFY


qualifyTests : Test
qualifyTests =
    Test.describe "qualify"
        [ test "should respect implicit imports" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should fully qualify if import missing" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , test "should qualify if not exposed" <|
            \() ->
                """module A exposing (..)
import Set exposing (toList)
a = List.foldl f x << toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (toList)
a = Set.foldl f x
"""
                        ]
        , test "should not qualify if directly imported (exposed) explicitly" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a = foldl f x
"""
                        ]
        , test "should not qualify if directly imported (exposed) explicitly even if an alias exists" <|
            \() ->
                """module A exposing (..)
import Set as UniqueList exposing (foldl)
a = List.foldl f x << UniqueList.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as UniqueList exposing (foldl)
a = foldl f x
"""
                        ]
        , test "should not qualify if directly imported from (..)" <|
            \() ->
                """module A exposing (..)
import Set exposing (..)
a = List.foldl f x << toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (..)
a = foldl f x
"""
                        ]
        , test "should not qualify if directly imported from (..) even if an alias exists" <|
            \() ->
                """module A exposing (..)
import Set as UniqueList exposing (..)
a = List.foldl f x << toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as UniqueList exposing (..)
a = foldl f x
"""
                        ]
        , test "should qualify using alias" <|
            \() ->
                """module A exposing (..)
import Set as UniqueList
a = List.foldl f x << UniqueList.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as UniqueList
a = UniqueList.foldl f x
"""
                        ]
        , qualifyShadowingTests
        ]


qualifyShadowingTests : Test
qualifyShadowingTests =
    Test.describe "qualify shadowing"
        [ test "should qualify if imported and exposed but shadowed by module function/value declaration" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a = List.foldl f x << Set.toList
foldl = ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a = Set.foldl f x
foldl = ()
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by module variant" <|
            \() ->
                """module A exposing (..)
a = List.head []
type MaybeExists
    = Nothing
    | Just ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on an empty list will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.Nothing
type MaybeExists
    = Nothing
    | Just ()
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by declaration argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a foldl = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a foldl = Set.foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by lambda argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a = \\( _, Node _ ({ foldl } :: _) ) -> List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a = \\( _, Node _ ({ foldl } :: _) ) -> Set.foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by case pattern argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            []

        ( _, Node _ ({ foldl } :: _) ) ->
            List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            []

        ( _, Node _ ({ foldl } :: _) ) ->
            Set.foldl f x
"""
                        ]
        , test "should not qualify if imported and exposed and same binding only in different case pattern argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            List.foldl f x << Set.toList

        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            foldl f x

        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                        ]
        , test "should not qualify if imported and exposed and same binding only in case pattern argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    case List.foldl f x << Set.toList of
        ( _, Node [] _ ) ->
            []
    
        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    case foldl f x of
        ( _, Node [] _ ) ->
            []
    
        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by let declaration argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            List.foldl f x << Set.toList
    in
    doIt
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            Set.foldl f x
    in
    doIt
"""
                        ]
        , test "should not qualify if imported and exposed and same binding in argument of different let declaration" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
        
        doItBetter x =
            List.foldl f x << Set.toList
    in
    doItBetter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
        
        doItBetter x =
            foldl f x
    in
    doItBetter
"""
                        ]
        , test "should not qualify if imported and exposed and same binding in let declaration argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
    in
    List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
    in
    foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by let destructured binding" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        ( _, Node _ ({ foldl } :: _) ) =
            something
    in
    List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        ( _, Node _ ({ foldl } :: _) ) =
            something
    in
    Set.foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by let declaration name" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        foldl init =
            FoldingMachine.foldl init
    in
    List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        foldl init =
            FoldingMachine.foldl init
    in
    Set.foldl f x
"""
                        ]
        , test "should not qualify if imported and exposed and same binding in a different branches" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    if condition then
        \\foldl -> doIt
    else
        [ \\foldl -> doIt
        , (\\foldl -> doIt) >> (\\x -> List.foldl f x << Set.toList)
        ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    if condition then
        \\foldl -> doIt
    else
        [ \\foldl -> doIt
        , (\\foldl -> doIt) >> (\\x -> foldl f x)
        ]
"""
                        ]
        ]



-- LAMBDA REDUCE


lambdaReduceTests : Test
lambdaReduceTests =
    describe "lambda reduce"
        [ test "should detect (\\error -> Err error) as Err function" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << (\\error -> Err error)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> error |> Err) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> error |> Err)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> Err <| error) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> Err <| error)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error sorry -> (error |> Err) <| sorry) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> \\sorry -> (error |> Err) <| sorry)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> \\sorry -> (error |> Err) <| sorry) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> \\sorry -> (error |> Err) <| sorry)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\result -> Result.mapError f result) as Result.mapError f call" <|
            \() ->
                """module A exposing (..)
a = (\\result -> Result.mapError f result) << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\result -> \\sorry -> (result |> Result.mapError f) <| sorry) as Result.mapError f call" <|
            \() ->
                """module A exposing (..)
a = (\\result -> \\sorry -> (result |> Result.mapError f) <| sorry) << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
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
                            , under = "always g"
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
                            , under = "always g"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
"""
                        ]
        ]



-- BOOLEANS


booleanTests : Test
booleanTests =
    describe "Booleans"
        [ test "should not report unsimplifiable condition" <|
            \() ->
                """module A exposing (..)
a = x || y
b = y && z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , orTests
        , andTests
        , notTests
        , equalTests
        ]


alwaysSameDetails : List String
alwaysSameDetails =
    [ "This condition will always result in the same value. You may have hardcoded a value or mistyped a condition."
    ]


unnecessaryMessage : String
unnecessaryMessage =
    "Part of the expression is unnecessary"


unnecessaryDetails : List String
unnecessaryDetails =
    [ "A part of this condition is unnecessary. You can remove it and it would not impact the behavior of the program."
    ]


sameThingOnBothSidesDetails : String -> List String
sameThingOnBothSidesDetails value =
    [ "Based on the values and/or the context, we can determine that the value of this operation will always be " ++ value ++ "."
    ]


comparisonIsAlwaysMessage : String -> String
comparisonIsAlwaysMessage value =
    "Comparison is always " ++ value


orTests : Test
orTests =
    describe "||"
        [ test "should simplify 'True || x' to True" <|
            \() ->
                """module A exposing (..)
a = True || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = alwaysSameDetails
                            , under = "True || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'x || True' to x" <|
            \() ->
                """module A exposing (..)
a = x || True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x || True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'False || x' to x" <|
            \() ->
                """module A exposing (..)
a = False || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "False || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x || False' to x" <|
            \() ->
                """module A exposing (..)
a = x || False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x || False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around False" <|
            \() ->
                """module A exposing (..)
a = x || (False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x || (False)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around True" <|
            \() ->
                """module A exposing (..)
a = (True) || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = alwaysSameDetails
                            , under = "(True) || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (True)
"""
                        ]
        , test "should simply x || x to x" <|
            \() ->
                """module A exposing (..)
a = x || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simply x || y || x to x || y" <|
            \() ->
                """module A exposing (..)
a = x || y || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || y
"""
                        ]
        , test "should simply x || x || y to x || y" <|
            \() ->
                """module A exposing (..)
a = x || x || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || y
"""
                        ]
        , test "should simply x || (x) || y to x || False || y" <|
            \() ->
                """module A exposing (..)
a = x || (x) || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || (False) || y
"""
                        ]
        , test "should simply x || (x || y) to x || (False || y)" <|
            \() ->
                """module A exposing (..)
a = x || (x || y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || (False || y)
"""
                        ]
        , test "should simply (x || y) || (z || x) to (x || y) || (z)" <|
            \() ->
                """module A exposing (..)
a = (x || y) || (z || x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x || y) || (z)
"""
                        ]
        , test "should simply x && x to x" <|
            \() ->
                """module A exposing (..)
a = x && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (&&) operator, therefore one of them can be removed."
                                ]
                            , under = " && x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simply x && (x && y) to x && (True && y)" <|
            \() ->
                """module A exposing (..)
a = x && (x && y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (&&) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x && (True && y)
"""
                        ]
        ]


andTests : Test
andTests =
    describe "&&"
        [ test "should simplify 'True && x' to x" <|
            \() ->
                """module A exposing (..)
a = True && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "True && x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x && True' to x" <|
            \() ->
                """module A exposing (..)
a = x && True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x && True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'False && x' to False" <|
            \() ->
                """module A exposing (..)
a = False && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = alwaysSameDetails
                            , under = "False && x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'x && False' to False" <|
            \() ->
                """module A exposing (..)
a = x && False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = alwaysSameDetails
                            , under = "x && False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


notTests : Test
notTests =
    describe "not calls"
        [ test "should simplify 'not True' to False" <|
            \() ->
                """module A exposing (..)
a = not True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not False' to True" <|
            \() ->
                """module A exposing (..)
a = not False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to True"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'not (True)' to False" <|
            \() ->
                """module A exposing (..)
a = not (True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not (True)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not <| True' to False" <|
            \() ->
                """module A exposing (..)
a = not <| True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not <| True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'True |> not' to False" <|
            \() ->
                """module A exposing (..)
a = True |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "True |> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify not >> not to identity" <|
            \() ->
                """module A exposing (..)
a = not >> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> not >> not to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> not >> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify not >> not >> a to identity >> a" <|
            \() ->
                """module A exposing (..)
a = not >> not >> a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity >> a
"""
                        ]
        , test "should simplify not << not to identity" <|
            \() ->
                """module A exposing (..)
a = not << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify not << not << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = not << not << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << not << not to a << identity" <|
            \() ->
                """module A exposing (..)
a = a << not << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a << identity
"""
                        ]
        , test "should simplify (not >> a) << not to a" <|
            \() ->
                """module A exposing (..)
a = (not >> a) << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> a) << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should not simplify (not << a) << not" <|
            \() ->
                """module A exposing (..)
a = (not << a) << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'not (not x)' to x" <|
            \() ->
                """module A exposing (..)
a = not (not x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not (not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x)
"""
                        ]
        , test "should simplify 'x |> not |> not' to x" <|
            \() ->
                """module A exposing (..)
a = x |> not |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not |> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify '(x |> not) |> not' to x" <|
            \() ->
                """module A exposing (..)
a = (x |> not) |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not) |> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x)
"""
                        ]
        , test "should simplify '(not <| x) |> not' to x" <|
            \() ->
                """module A exposing (..)
a = (not <| x) |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not <| x) |> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x)
"""
                        ]
        , test "should simplify 'not x |> not' to x" <|
            \() ->
                """module A exposing (..)
a = not x |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not x |> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'not <| not x' to x" <|
            \() ->
                """module A exposing (..)
a = not <| not x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not <| not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]



-- CASE OF


caseOfTests : Test
caseOfTests =
    describe "Case of"
        [ test "should not report case of when the body of the branches are different" <|
            \() ->
                """module A exposing (..)
a = case value of
      A -> 1
      B -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace case of with a single wildcard case by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      _ -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not replace case of with a single case by the body of the case" <|
            \() ->
                """module A exposing (..)
type B = C
a = case value of
      C -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors

        -- TODO Create a project with a union with a single constructor
        --        , test "should not replace case of with a single case when the constructor is ignored" <|
        --            \() ->
        --                """module A exposing (..)
        --type B = C
        --a = case value of
        --      C -> x
        --"""
        --                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "A.B" ] <| defaults)
        --                    |> Review.Test.expectNoErrors
        , test "should replace case of with multiple cases that have the same body" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not replace case of with a single case when the constructor from a dependency is ignored" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| defaults)
                    |> Review.Test.expectNoErrors
        , test "should not replace case of with multiple cases when all constructors of ignored type are used" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| defaults)
                    |> Review.Test.expectNoErrors
        , test "should replace case of with multiple cases when not all constructors of ignored type are used" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      _ -> x
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| defaults)
                    |> Review.Test.expectNoErrors
        , test "should not replace case of with a single case with ignored arguments by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B C) -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace case of where a pattern introduces a variable" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B c) -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace boolean case of with the same body by that body" <|
            \() ->
                """module A exposing (..)
a = case value of
      True -> x
      False -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace case expression that destructures a tuple by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        ( x, y ) ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "( x, y )"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let ( x, y ) = value
    in
            1
"""
                        ]
        , test "should replace case expression that destructures a record by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        { x, y } ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "{ x, y }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let { x, y } = value
    in
            1
"""
                        ]
        , test "should replace case expression that destructures a variable by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        var ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "var"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let var = value
    in
            1
"""
                        ]
        ]


booleanCaseOfMessage : String
booleanCaseOfMessage =
    "Replace `case..of` by an `if` condition"


booleanCaseOfDetails : List String
booleanCaseOfDetails =
    [ "The idiomatic way to check for a condition is to use an `if` expression."
    , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
    ]


booleanCaseOfTests : Test
booleanCaseOfTests =
    describe "Boolean case of"
        [ test "should not report pattern matches for non-boolean values" <|
            \() ->
                """module A exposing (..)
a = case thing of
      Thing -> 1
      Bar -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report pattern matches when the evaluated expression is a tuple of with a boolean" <|
            \() ->
                """module A exposing (..)
a = case ( bool1, bool2 ) of
      ( True, True ) -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report pattern matches when one of the patterns is a bool constructor (True and False)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      False -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (on multiple lines)" <|
            \() ->
                """module A exposing (..)
a =
    case bool of
        True ->
            1
        False ->
            2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    if bool then 1
        else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and True)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      True -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (True and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when one of the patterns starts with `Basics.`" <|
            \() ->
                """module A exposing (..)
a = case bool of
      Basics.True -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "Basics.True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when the constructor seems to be for booleans but comes from an unknown module" <|
            \() ->
                """module A exposing (..)
a = case bool of
      OtherModule.True -> 1
      _ -> 2

b = case bool of
      OtherModule.False -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]



-- NUMBER


numberTests : Test
numberTests =
    describe "Number tests"
        [ plusTests
        , minusTests
        , multiplyTests
        , divisionTests
        , negationTest
        , basicsNegateTests
        , comparisonTests
        ]


plusTests : Test
plusTests =
    describe "(+)"
        [ test "should not simplify (+) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b + 1
b = 2 + 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n + 0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary addition with 0"
                            , details = [ "Adding 0 does not change the value of the number." ]
                            , under = " + 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n + 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary addition with 0"
                            , details = [ "Adding 0 does not change the value of the number." ]
                            , under = " + 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 + n to n" <|
            \() ->
                """module A exposing (..)
a = 0 + n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary addition with 0"
                            , details = [ "Adding 0 does not change the value of the number." ]
                            , under = "0 + "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        ]


minusTests : Test
minusTests =
    describe "(-)"
        [ test "should not simplify (-) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b - 1
b = 2 - 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n - 0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtraction with 0"
                            , details = [ "Subtracting 0 does not change the value of the number." ]
                            , under = " - 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n - 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtraction with 0"
                            , details = [ "Subtracting 0 does not change the value of the number." ]
                            , under = " - 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 - n to -n" <|
            \() ->
                """module A exposing (..)
a = 0 - n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtracting from 0"
                            , details = [ "You can negate the expression on the right like `-n`." ]
                            , under = "0 - "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -n
"""
                        ]
        , test "should simplify 0 - List.length list to -(List.length list)" <|
            \() ->
                """module A exposing (..)
a = 0 - List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtracting from 0"
                            , details = [ "You can negate the expression on the right like `-n`." ]
                            , under = "0 - "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -(List.length list)
"""
                        ]
        ]


multiplyTests : Test
multiplyTests =
    describe "(*)"
        [ test "should not simplify (*) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b * 2
b = 2 * 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n * 1 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary multiplication by 1"
                            , details = [ "Multiplying by 1 does not change the value of the number." ]
                            , under = " * 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n * 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary multiplication by 1"
                            , details = [ "Multiplying by 1 does not change the value of the number." ]
                            , under = " * 1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 1 * n to n" <|
            \() ->
                """module A exposing (..)
a = 1 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary multiplication by 1"
                            , details = [ "Multiplying by 1 does not change the value of the number." ]
                            , under = "1 * "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should report but not fix n * 0" <|
            \() ->
                """module A exposing (..)
a = n * 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = " * 0"
                            }
                        ]
        , test "should report but not fix simplify n * 0.0" <|
            \() ->
                """module A exposing (..)
a = n * 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = " * 0.0"
                            }
                        ]
        , test "should report but not fix 0 * n" <|
            \() ->
                """module A exposing (..)
a = 0 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "0 * "
                            }
                        ]
        , test "should report but not fix 0.0 * n" <|
            \() ->
                """module A exposing (..)
a = 0.0 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "0.0 * "
                            }
                        ]
        ]


divisionTests : Test
divisionTests =
    describe "(/)"
        [ test "should not simplify (/) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = 1 / 2
b = 2 / 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n / 1 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary division by 1"
                            , details = [ "Dividing by 1 does not change the value of the number." ]
                            , under = " / 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n / 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary division by 1"
                            , details = [ "Dividing by 1 does not change the value of the number." ]
                            , under = " / 1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        ]


negationTest : Test
negationTest =
    describe "Unary negation"
        [ test "should not report negation used in okay situations" <|
            \() ->
                """module A exposing (..)
a = -1
a = -(-1 + 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify -(-n) to n" <|
            \() ->
                """module A exposing (..)
a = -(-n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            , under = "-(-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify -(-(f n)) to (f n)" <|
            \() ->
                """module A exposing (..)
a = -(-(f n))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            , under = "-(-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        ]


basicsNegateTests : Test
basicsNegateTests =
    describe "Basics.negate"
        [ test "should simplify negate >> negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate >> negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> negate >> negate to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> negate >> negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify negate >> negate >> a to identity >> a" <|
            \() ->
                """module A exposing (..)
a = negate >> negate >> a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity >> a
"""
                        ]
        , test "should simplify negate << negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify negate << negate << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = negate << negate << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << negate << negate to a << identity" <|
            \() ->
                """module A exposing (..)
a = a << negate << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a << identity
"""
                        ]
        , test "should simplify (negate >> a) << negate to a" <|
            \() ->
                """module A exposing (..)
a = (negate >> a) << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> a) << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should negate simplify (negate << a) << negate" <|
            \() ->
                """module A exposing (..)
a = (negate << a) << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'negate <| negate x' to x" <|
            \() ->
                """module A exposing (..)
a = negate <| negate x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate <| negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]


comparisonTests : Test
comparisonTests =
    describe "Comparison operators"
        [ lessThanTests
        ]


lessThanTests : Test
lessThanTests =
    describe "<"
        [ test "should simplify 1 < 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 < 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = comparisonIsAlwaysMessage "True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "1 < 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 1 < 2 + 3 to False" <|
            \() ->
                """module A exposing (..)
a = 1 < 2 + 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = comparisonIsAlwaysMessage "True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "1 < 2 + 3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 2 < 1 to False" <|
            \() ->
                """module A exposing (..)
a = 2 < 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = comparisonIsAlwaysMessage "False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "2 < 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 > 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 > 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = comparisonIsAlwaysMessage "False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 > 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 >= 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = comparisonIsAlwaysMessage "False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 >= 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 <= 2 to True" <|
            \() ->
                """module A exposing (..)
a = 1 <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = comparisonIsAlwaysMessage "True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "1 <= 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        ]


equalTests : Test
equalTests =
    describe "(==)"
        [ test "should not simplify values that can't be determined" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x == True to x" <|
            \() ->
                """module A exposing (..)
a = x == True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "x == True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify x == False" <|
            \() ->
                """module A exposing (..)
a = x == False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify True == x to x" <|
            \() ->
                """module A exposing (..)
a = True == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "True == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify False == x" <|
            \() ->
                """module A exposing (..)
a = False == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify x /= True" <|
            \() ->
                """module A exposing (..)
a = x /= True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x /= False to x" <|
            \() ->
                """module A exposing (..)
a = x /= False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "x /= False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify True /= x" <|
            \() ->
                """module A exposing (..)
a = True /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify False /= x to x" <|
            \() ->
                """module A exposing (..)
a = False /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "False /= x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify not x == not y to x == y" <|
            \() ->
                """module A exposing (..)
a = not x == not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary negation on both sides"
                            , details = [ "Since both sides are negated using `not`, they are redundant and can be removed." ]
                            , under = "not x == not y"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  x ==  y
"""
                        ]
        , test "should simplify not x /= not y to x /= y" <|
            \() ->
                """module A exposing (..)
a = not x /= not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary negation on both sides"
                            , details = [ "Since both sides are negated using `not`, they are redundant and can be removed." ]
                            , under = "not x /= not y"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  x /=  y
"""
                        ]
        , test "should simplify x == x to True" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify x == (x) to True" <|
            \() ->
                """module A exposing (..)
a = x == (x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == (x)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify x /= x to False" <|
            \() ->
                """module A exposing (..)
a = x /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x /= x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify more complex calls (function call and lambda)" <|
            \() ->
                """module A exposing (..)
a = List.map (\\a -> a.value) things == List.map (\\a -> a.value) things
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "List.map (\\a -> a.value) things == List.map (\\a -> a.value) things"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (f <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b) == (f <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (f b <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (f b <| c)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (b |> f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b) == (b |> f)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> f b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (c |> f b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> (b |> f))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (c |> (b |> f))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in let expression" <|
            \() ->
                """module A exposing (..)
a = (let x = 1 in f b c) == (c |> (let x = 1 in f b))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(let x = 1 in f b c) == (c |> (let x = 1 in f b))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in if expression" <|
            \() ->
                """module A exposing (..)
a = (if cond then f b c else g d c) == (c |> (if cond then f b else g d))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(if cond then f b c else g d c) == (c |> (if cond then f b else g d))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in case expression" <|
            \() ->
                """module A exposing (..)
a = (case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = """(case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(b.c) == (.c b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison using pipeline" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(b.c) == (.c <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different literals to False" <|
            \() ->
                """module A exposing (..)
a = "a" == "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "\"a\" == \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different char literals to False" <|
            \() ->
                """module A exposing (..)
a = 'a' == 'b'
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "'a' == 'b'"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify inequality of different literal comparisons to True" <|
            \() ->
                """module A exposing (..)
a = "a" /= "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "\"a\" /= \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different number literal comparisons to False" <|
            \() ->
                """module A exposing (..)
a = 1 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (integer left)" <|
            \() ->
                """module A exposing (..)
a = 1 == 2.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 == 2.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (float left)" <|
            \() ->
                """module A exposing (..)
a = 1.0 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1.0 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (hex left)" <|
            \() ->
                """module A exposing (..)
a = 0x10 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "0x10 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (addition left)" <|
            \() ->
                """module A exposing (..)
a = 1 + 3 == 2 + 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 + 3 == 2 + 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (subtraction left)" <|
            \() ->
                """module A exposing (..)
a = 1 - 3 == 2 - 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 - 3 == 2 - 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (multiplication left)" <|
            \() ->
                """module A exposing (..)
a = 2 * 3 == 2 * 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "2 * 3 == 2 * 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (division left)" <|
            \() ->
                """module A exposing (..)
a = 1 / 3 == 2 / 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 / 3 == 2 / 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of same value (() left)" <|
            \() ->
                """module A exposing (..)
a = () == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "() == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of same value (() right)" <|
            \() ->
                """module A exposing (..)
a = x == ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == ()"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of lists (different lengths)" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "[ 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same lengths but different values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "[ 1, 2 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 - 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "[ 1, 2 - 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different integers comparisons to False (wrapped in parens)" <|
            \() ->
                """module A exposing (..)
a = (1) == (2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "(1) == (2)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of tuples" <|
            \() ->
                """module A exposing (..)
a = ( 1, 2 ) == ( 1, 1 )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "( 1, 2 ) == ( 1, 1 )"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of records" <|
            \() ->
                """module A exposing (..)
a = { a = 1, b = 2 } == { b = 1, a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ a = 1, b = 2 } == { b = 1, a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and different field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ x | a = 1 } == { x | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "{ x | a = 1 } == { x | a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify equality of record updates with same base values and different fields" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify equality of record updates (different base values)" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ x | a = 1 } == { y | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should not simplify equality of record updates with same field values but different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify equality of record updates with non-corresponding fields but otherwise similar field values and different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1, b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify comparison of values for which we don't know if they're equal" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should normalize module names" <|
            \() ->
                [ """module A exposing (..)
import B exposing (b)
a = B.b == b
""", """module Other exposing (..)
b = 1
""" ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Comparison is always True"
                                , details = sameThingOnBothSidesDetails "True"
                                , under = "B.b == b"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B exposing (b)
a = True
"""
                            ]
                          )
                        ]
        , test "should simplify function calls with the same function and similar arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == map fn (2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "List.map fn 1 == map fn (2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import List exposing (map)
a = True
"""
                        ]
        , test "should not simplify function calls of the same function but with different arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == List.map fn 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify if expressions that look like each other" <|
            \() ->
                """module A exposing (..)
a = (if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify if expressions that don't look like each other" <|
            \() ->
                """module A exposing (..)
a = (if a then 2 else 3) == (if a then 1 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify negations that look like each other" <|
            \() ->
                """module A exposing (..)
a = -1 == -(2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "-1 == -(2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record accesses that look like each other" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 2 - 1 }).a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "({ a = 1 }).a == ({ a = 2 - 1 }).a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        , Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value" ]
                            , under = "({ a = 2 - 1 }).a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ({ a = 1 }).a == (2 - 1)
"""
                        , Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value" ]
                            , under = "({ a = 1 }).a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 2 - 1 }).a
"""
                        ]
        , test "should simplify operator expressions" <|
            \() ->
                """module A exposing (..)
a = (1 |> fn) == (2 - 1 |> fn)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(1 |> fn) == (2 - 1 |> fn)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify with different fields" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 1 }).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value" ]
                            , under = "({ a = 1 }).a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 1 }).b
"""
                        ]
        ]



-- IF


ifTests : Test
ifTests =
    describe "if expressions"
        [ test "should remove the else branch when a condition is True" <|
            \() ->
                """module A exposing (..)
a = if True then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should remove the if branch when a condition is False" <|
            \() ->
                """module A exposing (..)
a = if False then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should not remove anything if the condition is not statically knowable" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should only keep the condition if then is True and else is False" <|
            \() ->
                """module A exposing (..)
a = if condition then True else False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the same as the condition"
                            , details = [ "The expression can be replaced by the condition." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = condition
"""
                        ]
        , test "should only keep the negated condition if then is False and else is True" <|
            \() ->
                """module A exposing (..)
a = if condition then False else True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the inverse of the condition"
                            , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (condition)
"""
                        ]
        , test "should replace the expression by the branch if both branches have the same value" <|
            \() ->
                """module A exposing (..)
a = if condition then x else x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The values in both branches is the same."
                            , details = [ "The expression can be replaced by the contents of either branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace the expression by the branch if both branches are known to be equal" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2 - 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The values in both branches is the same."
                            , details = [ "The expression can be replaced by the contents of either branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        ]



-- DUPLICATED IF CONDITIONS


duplicatedIfTests : Test
duplicatedIfTests =
    describe "Duplicated if conditions"
        [ test "should not remove nested conditions if they're not duplicate" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove duplicate nested conditions (x inside the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (x inside the then, with parens)" <|
            \() ->
                """module A exposing (..)
a =
  if (x) then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if (x) then
    1
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (not x inside the top condition)" <|
            \() ->
                """module A exposing (..)
a =
  if not x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not x then
    2
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (not <| x inside the top condition)" <|
            \() ->
                """module A exposing (..)
a =
  if not <| x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not <| x then
    2
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (not x inside the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if not x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (x inside the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (not x inside the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    1
  else
    if not x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to True"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    if True then
      2
    else
      3
"""
                        ]
        , test "should remove duplicate nested conditions (x part of a nested condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if x && y then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Part of the expression is unnecessary"
                            , details = [ "A part of this condition is unnecessary. You can remove it and it would not impact the behavior of the program." ]
                            , under = "x && y"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove duplicate deeply nested conditions" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if y then
      if x then
        1
      else
        2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      3
  else
    4
"""
                        ]
        , test "should remove duplicate nested conditions (x part of the top && condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x && y then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x && y then
    1
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (x part of the top || condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x || y then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x || y then
    1
  else
    3
"""
                        ]
        , test "should not remove condition when we don't have enough information (x part of the top && condition, other condition in the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x && y then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not remove condition when we don't have enough information (x part of the top || condition, other condition in the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x || y then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove branches where the condition always matches" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    if x == 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == 1"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    if True then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (strings == with different values)" <|
            \() ->
                """module A exposing (..)
a =
  if x == "a" then
    if x == "b" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x == \"b\""
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 8 }, end = { row = 4, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == "a" then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (not function or value)" <|
            \() ->
                """module A exposing (..)
a =
  if item.name == "Aged Brie" then
    if item.name == "Sulfuras, Hand of Ragnaros" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "item.name == \"Sulfuras, Hand of Ragnaros\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if item.name == "Aged Brie" then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    if x == 2 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (strings)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= "a" then
    if x == "a" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= "a" then
    2
  else
    3
"""
                        ]
        , test "should not spread inferred things from one branch to another" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    1
  else if x == 2 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove branches where the condition always matches (/=)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    if x /= 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (/= in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    1
  else if x /= 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x /= 1"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 11 }, end = { row = 5, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else if False then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (== in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (/= <then> == in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    if x == 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (literal on the left using ==, second if)" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    1
  else if 1 == x then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition never matches (literal on the left using ==, first if)" <|
            \() ->
                """module A exposing (..)
a =
  if 1 == x then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if 1 == x then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition always matches (literal on the left using /=)" <|
            \() ->
                """module A exposing (..)
a =
  if 1 /= x then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if 1 /= x then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (&&)" <|
            \() ->
                """module A exposing (..)
a =
  if a && b then
    1
  else if a && b then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a && b then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> a)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if a then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> a && b)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if a && b then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        -- TODO Order of the errors seem to matter here. Should be fixed in `elm-review`
                        [ Review.Test.error
                            { message = "Comparison is always False"
                            , details = alwaysSameDetails
                            , under = "a && b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else if a then
    2
  else
    3
"""
                        , Review.Test.error
                            { message = "Comparison is always False"
                            , details = alwaysSameDetails
                            , under = "a && b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else if b then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition may not match (a && b --> a --> b)" <|
            \() ->
                """module A exposing (..)
a =
  if a && b then
    1
  else if a then
    if b then
      2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a && b then
    1
  else if a then
    3
  else
    4
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> <then> a --> b)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    if not a then
      if b then
        1
      else
        2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    if not a then
      1
    else
      3
  else
    4
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> not a)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if not a then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to True"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition may not match (not (a || b) --> not a --> not b)" <|
            \() ->
                -- TODO Probably best to normalize inside Evaluate.getBoolean?
                """module A exposing (..)
a =
  if not (a || b) then
    1
  else if not a then
    if b then
      2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not (a || b) then
    1
  else if not a then
    2
  else
    4
"""
                        ]

        --        ,   test "should not lose information as more conditions add up" <|
        --                \() ->
        --                    """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a == f b then
        --      if a == 1 then
        --        1
        --      else
        --        2
        --    else
        --      3
        --  else
        --    4
        --"""
        --                        |> Review.Test.run ruleWithDefaults
        --                        |> Review.Test.expectErrors
        --                            [ Review.Test.error
        --                                { message = "The condition will always evaluate to True"
        --                                , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
        --                                , under = "if"
        --                                }
        --                                |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
        --                                |> Review.Test.whenFixed """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a == 1 then
        --        1
        --      else
        --        2
        --  else
        --    4
        --"""
        --                            , Review.Test.error
        --                                { message = "The condition will always evaluate to True"
        --                                , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
        --                                , under = "if"
        --                                }
        --                                |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
        --                                |> Review.Test.whenFixed """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a /= 2 then
        --      1
        --    else
        --      3
        --  else
        --    4
        --"""
        --                            ]
        -- TODO
        -- Unhappy && and || cases:
        --   if a && b then ... else <not a || not b>
        --   if a || b then ... else <not a && not b>
        ]



-- RECORD UPDATE


recordUpdateTests : Test
recordUpdateTests =
    describe "Record update"
        [ test "should not simplify when assigning a different field or a value" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = b.c, e = c.e, f = g b.f, g = b.g.h }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify when assigning a field in a non-update record assignment" <|
            \() ->
                """module A exposing (..)
a = { d = b.d, c = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove the updates that assigns the previous value of a field to itself (first)" <|
            \() ->
                """module A exposing (..)
a = { b | d = b.d, c = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1 }
"""
                        ]
        , test "should remove the update record syntax when it assigns the previous value of a field to itself and it is the only assignment" <|
            \() ->
                """module A exposing (..)
a = { b | d = b.d }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (not first)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = b.d }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (using parens)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = (b.d) }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        ]



-- FULLY APPLIED PREFIX OPERATOR


fullyAppliedPrefixOperatorMessage : String
fullyAppliedPrefixOperatorMessage =
    "Use the infix form (a + b) over the prefix form ((+) a b)"


fullyAppliedPrefixOperatorDetails : List String
fullyAppliedPrefixOperatorDetails =
    [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used."
    ]


fullyAppliedPrefixOperatorTests : Test
fullyAppliedPrefixOperatorTests =
    describe "Fully applied prefix operators"
        [ test "should not report a lonely operator" <|
            \() ->
                """
module A exposing (..)
a = (++)
b = (::)
c = (//)
d = (+)
e = (/)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in infix position" <|
            \() ->
                """
module A exposing (..)
a = y ++ z
b = y :: z
c = y // z
d = y + z
e = y / z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in prefix position with one argument" <|
            \() ->
                """
module A exposing (..)
a = (++) z
b = (::) z
c = (//) z
d = (+) z
e = (/) z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace (++) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (++) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y ++ z
"""
                        ]
        , test "should replace (::) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (::) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(::)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y :: z
"""
                        ]
        , test "should replace (//) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (//) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(//)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y // z
"""
                        ]
        , test "should replace (+) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (+) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(+)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y + z
"""
                        ]
        , test "should replace (/) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (/) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(/)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y / z
"""
                        ]
        , test "should replace infix operator with 2 arguments, used on several lines" <|
            \() ->
                """module A exposing (..)
a =
    (++)
        y
        z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    y
        ++ z
"""
                        ]
        , test "should replace infix operator with 2 arguments wrapped in parens and braces" <|
            \() ->
                """module A exposing (..)
a =
    (++) (y + 1)
        [z]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    (y + 1)
        ++ [z]
"""
                        ]
        ]


appliedLambdaTests : Test
appliedLambdaTests =
    describe "Applied lambda functions"
        [ test "should not report okay function/lambda calls" <|
            \() ->
                """
module A exposing (..)
a = f ()
b = (\\x y -> x + y) n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace (\\() -> x) () by x" <|
            \() ->
                """module A exposing (..)
a = (\\() -> x) ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (x)$
""" |> String.replace "$" " ")
                        ]
        , test "should replace (\\_ -> x) a by x" <|
            \() ->
                """module A exposing (..)
a = (\\_ -> x) a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (x)$
""" |> String.replace "$" " ")
                        ]
        , test "should replace (\\() y -> x) () by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\() y -> x) ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (\\y -> x)$
""" |> String.replace "$" " ")
                        ]
        , test "should replace (\\_ y -> x) a by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\_ y -> x) a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (\\y -> x)$
""" |> String.replace "$" " ")
                        ]
        ]



-- (++)


usingPlusPlusTests : Test
usingPlusPlusTests =
    describe "(++)"
        [ test "should not report a single list literal" <|
            \() ->
                """module A exposing (..)
a = []
b = [1]
c = [ "string", "foo", "bar" ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report simple strings" <|
            \() ->
                """module A exposing (..)
a = "abc" ++ value
b = \"\"\"123\"\"\"
c = \"\"\"multi
line
string
\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace "a" ++ "" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary concatenation with an empty string"
                            , details = [ "You should remove the concatenation with the empty string." ]
                            , under = "\"\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
        , test """should not report x ++ "" (because this can lead to better performance)""" <|
            \() ->
                """module A exposing (..)
a = x ++ ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace "" ++ "a" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "" ++ "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary concatenation with an empty string"
                            , details = [ "You should remove the concatenation with the empty string." ]
                            , under = "\"\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
        , test "should report concatenating two list literals" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] ++ [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "[ 1 ] ++ [ 2, 3 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 , 2, 3 ]
"""
                        ]
        , test "should report concatenating two list literals, even they contain variables" <|
            \() ->
                """module A exposing (..)
a = [ a, 1 ] ++ [ b, 2 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "[ a, 1 ] ++ [ b, 2 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a, 1 , b, 2 ]
"""
                        ]
        , test "should report concatenating an empty list and something" <|
            \() ->
                """module A exposing (..)
a = [] ++ something
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Concatenating with a single list doesn't have any effect"
                            , details = [ "You should remove the concatenation with the empty list." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should report concatenating something and an empty list" <|
            \() ->
                """module A exposing (..)
a = something ++ []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Concatenating with a single list doesn't have any effect"
                            , details = [ "You should remove the concatenation with the empty list." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should replace [b] ++ c by b :: c" <|
            \() ->
                """module A exposing (..)
a = [ b ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Should use (::) instead of (++)"
                            , details = [ "Concatenating a list with a single value is the same as using (::) on the list with the value." ]
                            , under = "[ b ] ++ c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b :: c
"""
                        ]
        , test "should replace [ f n ] ++ c by (f n) :: c" <|
            \() ->
                """module A exposing (..)
a = [ f n ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Should use (::) instead of (++)"
                            , details = [ "Concatenating a list with a single value is the same as using (::) on the list with the value." ]
                            , under = "[ f n ] ++ c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n) :: c
"""
                        ]
        , test "should not replace [b] ++ c when on the right of a ++ operator" <|
            \() ->
                """module A exposing (..)
a = left ++ [ b ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace [b] ++ c when on the right of a ++ operator but inside parens" <|
            \() ->
                """module A exposing (..)
a = left ++ ([ b ] ++ c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]



-- STRING


stringSimplificationTests : Test
stringSimplificationTests =
    describe "String"
        [ stringFromListTests
        , stringIsEmptyTests
        , stringLengthTests
        , concatTests
        , joinTests
        , stringRepeatTests
        , stringReplaceTests
        , stringWordsTests
        , stringLinesTests
        , stringReverseTests
        , stringSliceTests
        , stringRightTests
        , stringLeftTests
        ]


stringIsEmptyTests : Test
stringIsEmptyTests =
    describe "String.isEmpty"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty
b = String.isEmpty value
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.isEmpty \"\" by True" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to String.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.isEmpty \"a\" by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to String.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


stringLengthTests : Test
stringLengthTests =
    describe "String.length"
        [ test "should not report String.length that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.length
b = String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.length \"\" by 0" <|
            \() ->
                """module A exposing (..)
a = String.length ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 0"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace String.length \"abc\" by 3" <|
            \() ->
                """module A exposing (..)
a = String.length "abc"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 3"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace String.length \"a\\t🚀b\\c🇲🇻\\u{000D}\\r\" by 13" <|
            \() ->
                """module A exposing (..)
a = String.length "a\\t🚀b\\\\c🇲🇻\\u{000D}\\r"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 13"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = 13
"""
                        ]
        , test "should replace String.length \"\"\"a\\t🚀b\\c🇲🇻\\u{000D}\\r\"\"\" by 13" <|
            \() ->
                """module A exposing (..)
a = String.length \"\"\"a\\t🚀b\\\\c🇲🇻\\u{000D}\\r\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 13"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = 13
"""
                        ]
        ]


concatTests : Test
concatTests =
    describe "String.concat"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.concat list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.concat [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.concat []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.concat on an empty list will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        ]


joinTests : Test
joinTests =
    describe "String.join"
        [ test "should not report String.join that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.join b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.join b [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.join b []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.join on an empty list will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.join "" list by String.concat list""" <|
            \() ->
                """module A exposing (..)
a = String.join "" list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use String.concat instead"
                            , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat list
"""
                        ]
        , test """should replace String.join "" by String.concat""" <|
            \() ->
                """module A exposing (..)
a = String.join ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use String.concat instead"
                            , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat
"""
                        ]
        , test """should replace list |> String.join "" by list |> String.concat""" <|
            \() ->
                """module A exposing (..)
a = list |> String.join ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use String.concat instead"
                            , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> String.concat
"""
                        ]
        ]


stringRepeatTests : Test
stringRepeatTests =
    describe "String.repeat"
        [ test "should not report String.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.repeat n str
b = String.repeat 5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.repeat n "" by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.repeat n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.repeat with an empty string will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.repeat 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.repeat 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat will result in an empty string"
                            , details = [ "Using String.repeat with a number less than 1 will result in an empty string. You can replace this call by an empty string." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.repeat 0 by (always "")""" <|
            \() ->
                """module A exposing (..)
a = String.repeat 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat will result in an empty string"
                            , details = [ "Using String.repeat with a number less than 1 will result in an empty string. You can replace this call by an empty string." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.repeat -5 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.repeat -5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat will result in an empty string"
                            , details = [ "Using String.repeat with a number less than 1 will result in an empty string. You can replace this call by an empty string." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.repeat 1 str by str" <|
            \() ->
                """module A exposing (..)
a = String.repeat 1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat 1 won't do anything"
                            , details = [ "Using String.repeat with 1 will result in the second argument." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  str
"""
                        ]
        ]


stringReplaceTests : Test
stringReplaceTests =
    describe "String.replace"
        [ test "should not report String.replace that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.replace n str
b = String.replace 5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.replace n n by identity" <|
            \() ->
                """module A exposing (..)
a = String.replace n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The result of String.replace will be the original string"
                            , details = [ "The pattern to replace and the replacement are equal, therefore the result of the String.replace call will be the original string." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.replace n n x by x" <|
            \() ->
                """module A exposing (..)
a = String.replace n n x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The result of String.replace will be the original string"
                            , details = [ "The pattern to replace and the replacement are equal, therefore the result of the String.replace call will be the original string." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace String.replace x y \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.replace x y ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The result of String.replace will be the empty string"
                            , details = [ "Replacing anything on an empty string results in an empty string." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.replace x y z by z when we know what the value will be and that it is unchanged" <|
            \() ->
                """module A exposing (..)
a = String.replace "x" "y" "z"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The result of String.replace will be the original string"
                            , details = [ "The replacement doesn't haven't any noticeable impact. You can remove the call to String.replace." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "z"
"""
                        ]
        , test "should replace z |> String.replace x y z by z when we know what the value will be and that it is unchanged" <|
            \() ->
                """module A exposing (..)
a = "z" |> String.replace "x" "y"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The result of String.replace will be the original string"
                            , details = [ "The replacement doesn't haven't any noticeable impact. You can remove the call to String.replace." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "z"
"""
                        ]
        , test "should not replace String.replace x y z by z when we know what the value will be but it will be different" <|
            \() ->
                """module A exposing (..)
a = String.replace "x" "y" "xz"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not remove String.replace call when the replacement is \u{000D}" <|
            \() ->
                """module A exposing (..)
a = \"\"\"
foo
bar
\"\"\"
                    |> String.replace "\\r" ""

b = \"\"\"
foo
bar
\"\"\"
                    |> String.replace "\\u{000D}" ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


stringWordsTests : Test
stringWordsTests =
    describe "String.words"
        [ test "should not report String.words that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.words
b = String.words str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.words "" by []""" <|
            \() ->
                """module A exposing (..)
a = String.words ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.words on an empty string will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "String.words"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


stringLinesTests : Test
stringLinesTests =
    describe "String.lines"
        [ test "should not report String.lines that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.lines
b = String.lines str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.lines "" by []""" <|
            \() ->
                """module A exposing (..)
a = String.lines ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.lines on an empty string will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "String.lines"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


stringFromListTests : Test
stringFromListTests =
    describe "String.fromList"
        [ test "should not report String.fromList that contains a variable" <|
            \() ->
                """module A exposing (..)
a = String.fromList
b = String.fromList str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.fromList [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling String.fromList [] will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.fromList [ a ] by String.fromChar a" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling String.fromList with a list with a single char is the same as String.fromChar with the contained char"
                            , details = [ "You can replace this call by String.fromChar with the contained char." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar a
"""
                        ]
        , test "should replace String.fromList [ f a ] by String.fromChar (f a)" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling String.fromList with a list with a single char is the same as String.fromChar with the contained char"
                            , details = [ "You can replace this call by String.fromChar with the contained char." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar (f b)
"""
                        ]
        ]


stringReverseTests : Test
stringReverseTests =
    describe "String.reverse"
        [ test "should not report String.reverse that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.reverse
b = String.reverse str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.reverse \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.reverse ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.reverse on an empty string will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.reverse <| String.reverse <| x by x" <|
            \() ->
                """module A exposing (..)
a = String.reverse <| String.reverse <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double reversal"
                            , details = [ "Composing `reverse` with `reverse` cancel each other out." ]
                            , under = "String.reverse <| String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]


stringSliceTests : Test
stringSliceTests =
    describe "String.slice"
        [ test "should not report String.slice that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.slice b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.slice 0 n" <|
            \() ->
                """module A exposing (..)
a = String.slice 0
b = String.slice 0 n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.slice b 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice b 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.slice with end index 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice b 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice b 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.slice with end index 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice n n by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.slice with equal start and end index will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice n n str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice n n str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.slice with equal start and end index will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice a z \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice a z ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.slice on an empty string will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice 0 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice 0 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.slice with end index 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice 0 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice 0 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.slice with end index 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice with natural start >= natural end by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice 2 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to String.slice will result in \"\""
                            , details = [ "You can replace this slice operation by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice with negative start >= negative end by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice -1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to String.slice will result in \"\""
                            , details = [ "You can replace this slice operation by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should not report String.slice with negative start, natural end" <|
            \() ->
                """module A exposing (..)
a = String.slice -1 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        , test "should not report String.slice with natural start, negative end" <|
            \() ->
                """module A exposing (..)
a = String.slice 1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        ]


stringLeftTests : Test
stringLeftTests =
    describe "String.left"
        [ test "should not report String.left that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.left b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.left 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.left with length 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.left 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.left with length 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.left -literal by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.left with negative length will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.left n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.left on an empty string will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        ]


stringRightTests : Test
stringRightTests =
    describe "String.right"
        [ test "should not report String.right that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.right b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.right 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.right with length 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.right 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.right with length 0 will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.right -literal str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right -1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.right with negative length will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.right -literal by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.right with negative length will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.right n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.right on an empty string will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        ]



-- LIST


listSimplificationTests : Test
listSimplificationTests =
    describe "List"
        [ usingConsTests
        , listAppendTests
        , usingListConcatTests
        , listConcatMapTests
        , listHeadTests
        , listTailTests
        , listMemberTests
        , listMapTests
        , listFilterTests
        , listFilterMapTests
        , listIndexedMapTests
        , listIsEmptyTests
        , listSumTests
        , listProductTests
        , listMinimumTests
        , listMaximumTests
        , listFoldlTests
        , listFoldrTests
        , listAllTests
        , listAnyTests
        , listRangeTests
        , listLengthTests
        , listRepeatTests
        , listPartitionTests
        , listSortTests
        , listSortByTests
        , listSortWithTests
        , listReverseTests
        , listTakeTests
        , listDropTests
        , listIntersperseTests
        , listMap2Tests
        , listMap3Tests
        , listMap4Tests
        , listMap5Tests
        , listUnzipTests
        ]


usingConsTests : Test
usingConsTests =
    describe "(::)"
        [ test "should not report using :: to a variable or expression" <|
            \() ->
                """module A exposing (..)
a = 1 :: list
b = 1 :: foo bar
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report using :: to a list literal" <|
            \() ->
                """module A exposing (..)
a = 1 :: [ 2, 3]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1, 2, 3]
"""
                        ]
        , test "should report using :: to an empty list literal" <|
            \() ->
                """module A exposing (..)
a = 1 :: []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 ]
"""
                        ]
        ]


listAppendTests : Test
listAppendTests =
    describe "List.append"
        [ test "should not report List.append with a list variable" <|
            \() ->
                """module A exposing (..)
a = List.append
b = List.append [ 1 ]
c = List.append [ 1 ] ys
d = List.append xs [ 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.append applied on two list literals" <|
            \() ->
                """module A exposing (..)
a = List.append [b] [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending literal lists could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append applied on two list literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = List.append [ b, z ] [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending literal lists could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, z ,c,d,0]
"""
                        ]
        , test "should report List.append <| on two list literals" <|
            \() ->
                """module A exposing (..)
a = List.append [b] <| [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending literal lists could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append |> on two list literals" <|
            \() ->
                """module A exposing (..)
a = [c,d,0] |> List.append [b]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending literal lists could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append |> on two list literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = [c,d,0] |> List.append [ b, z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending literal lists could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, z ,c,d,0]
"""
                        ]
        , test "should replace List.append [] ys by ys" <|
            \() ->
                """module A exposing (..)
a = List.append [] ys
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending [] doesn't have any effect"
                            , details = [ "You can remove the List.append function and the []." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ys
"""
                        ]
        , test "should replace List.append [] <| ys by ys" <|
            \() ->
                """module A exposing (..)
a = List.append [] <| ys
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending [] doesn't have any effect"
                            , details = [ "You can remove the List.append function and the []." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ys
"""
                        ]
        , test "should replace ys |> List.append [] by ys" <|
            \() ->
                """module A exposing (..)
a = ys |> List.append []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending [] doesn't have any effect"
                            , details = [ "You can remove the List.append function and the []." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ys
"""
                        ]
        , test "should replace List.append [] by identity" <|
            \() ->
                """module A exposing (..)
a = List.append []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending [] doesn't have any effect"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.append xs [] by xs" <|
            \() ->
                """module A exposing (..)
a = List.append xs []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending [] doesn't have any effect"
                            , details = [ "You can remove the List.append function and the []." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = xs
"""
                        ]
        , test "should replace List.append xs <| [] by xs" <|
            \() ->
                """module A exposing (..)
a = List.append xs <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending [] doesn't have any effect"
                            , details = [ "You can remove the List.append function and the []." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = xs
"""
                        ]
        , test "should replace [] |> List.append xs by xs" <|
            \() ->
                """module A exposing (..)
a = [] |> List.append xs
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending [] doesn't have any effect"
                            , details = [ "You can remove the List.append function and the []." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = xs
"""
                        ]
        ]


usingListConcatTests : Test
usingListConcatTests =
    describe "List.concat"
        [ test "should not report List.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.concat [ foo, bar ]
b = List.concat [ [ 1 ], foo ]
c = List.concat [ foo, [ 1 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.concat with no items" <|
            \() ->
                """module A exposing (..)
a = List.concat []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concat on an empty list will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should report List.concat with a single item" <|
            \() ->
                """module A exposing (..)
a = List.concat [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.concat on a list with 1 element"
                            , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                            , under = "List.concat [ b ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (<|)" <|
            \() ->
                """module A exposing (..)
a = List.concat <| [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.concat on a list with 1 element"
                            , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                            , under = "List.concat <| [ b ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (|>)" <|
            \() ->
                """module A exposing (..)
a = [ b ] |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.concat on a list with 1 element"
                            , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                            , under = "[ b ] |> List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat that only contains list literals" <|
            \() ->
                """module A exposing (..)
a = List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  [  1, 2, 3 ,  4, 5, 6 ]
"""
                        ]
        , test "should concatenate consecutive list literals in passed to List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ], [ 4, 5, 6], [7], c, [8], [9 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Consecutive literal lists should be merged"
                            , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ,  4, 5, 6, 7], c, [8, 9 ] ]
"""
                        ]
        , test "should replace List.concat (List.map f x) by List.concatMap f x" <|
            \() ->
                """module A exposing (..)
a = List.concat (List.map f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.concat can be combined using List.concatMap"
                            , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.concatMap f x)
"""
                        ]
        , test "should replace List.concat <| List.map f <| x by List.concatMap f <| x" <|
            \() ->
                """module A exposing (..)
a = List.concat <| List.map f <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.concat can be combined using List.concatMap"
                            , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f <| x
"""
                        ]
        , test "should replace x |> List.map f |> List.concat by x |> List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = x |> List.map f |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.concat can be combined using List.concatMap"
                            , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> List.concatMap f
"""
                        ]
        ]


listConcatMapTests : Test
listConcatMapTests =
    describe "List.concatMap"
        [ test "should replace List.concatMap identity x by List.concat x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap with an identity function is the same as using List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should replace List.concatMap identity by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap with an identity function is the same as using List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat
"""
                        ]
        , test "should replace List.concatMap (\\x->x) by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->x) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap with an identity function is the same as using List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should not report List.concatMap with a non-identity lambda" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->y) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.concatMap without an identity function by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.concatMap with no items" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an empty list will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.concatMap (always []) x by []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always []) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap will result in on an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.concatMap (always []) by always []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap will result in on an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.concatMap (\\_ -> [a]) x by List.map (\\_ -> a) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> ([a])) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "The function passed to List.concatMap always returns a list with a single element." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> a) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> List.singleton a) x by List.map (\\_ -> a) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> List.singleton a) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "The function passed to List.concatMap always returns a list with a single element." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ ->  a) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> if cond then [a] else [b]) x by List.map (\\_ -> if cond then a else b) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> if cond then [a] else [b]) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "The function passed to List.concatMap always returns a list with a single element." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> if cond then a else b) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> case y of A -> [a] ; B -> [b]) x by List.map (\\_ -> case y of A -> a ; B -> b) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap
    (\\_ ->
        case y of
            A -> [a]
            B -> [b]
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "The function passed to List.concatMap always returns a list with a single element." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map
    (\\_ ->
        case y of
            A -> a
            B -> b
    ) x
"""
                        ]
        , test "should replace List.concatMap f [ a ] by f a" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  f a
"""
                        ]
        , test "should replace List.concatMap f <| [ b c ] by f <| (b c)" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f <| [ b c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  f <| (b c)
"""
                        ]
        , test "should replace List.concatMap f <| [ c ] by c |> f" <|
            \() ->
                """module A exposing (..)
a = [ c ] |> List.concatMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c |>  f
"""
                        ]
        , test "should replace List.concatMap f <| [ b c ] by (b c) |> f" <|
            \() ->
                """module A exposing (..)
a = [ b c ] |> List.concatMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b c) |>  f
"""
                        ]
        , test "should replace List.map f >> List.concat by List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = List.map f >> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.concat can be combined using List.concatMap"
                            , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f
"""
                        ]
        , test "should replace List.concat << List.map f by List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = List.concat << List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.concat can be combined using List.concatMap"
                            , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f
"""
                        ]
        ]


listHeadTests : Test
listHeadTests =
    describe "List.head"
        [ test "should not report List.head used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.head
b = List.head list
c = List.head (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.head [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.head []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on an empty list will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.head (List.singleton a) by Just (a)" <|
            \() ->
                """module A exposing (..)
a = List.head (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (b)
"""
                        ]
        , test "should replace List.head <| List.singleton a by Just <| a" <|
            \() ->
                """module A exposing (..)
a = List.head <| List.singleton b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| b
"""
                        ]
        , test "should replace List.singleton a |> List.head by a |> Just" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> List.head
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> Just
"""
                        ]
        , test "should replace List.head [ a ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.head [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head [ f a ] by Just (f a)" <|
            \() ->
                """module A exposing (..)
a = List.head [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f b)
"""
                        ]
        , test "should replace List.head [ a, b, c ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.head [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head [ f a, b, c ] by Just (f a)" <|
            \() ->
                """module A exposing (..)
a = List.head [ f b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f b)
"""
                        ]
        , test "should replace List.head (a :: bToZ) by Just (a)" <|
            \() ->
                """module A exposing (..)
a = List.head (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (b)
"""
                        ]
        ]


listTailTests : Test
listTailTests =
    describe "List.tail"
        [ test "should not report List.tail used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.tail
b = List.tail list
c = List.tail (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.tail [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.tail []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.tail on an empty list will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.tail (List.singleton a) by Just []" <|
            \() ->
                """module A exposing (..)
a = List.tail (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.tail on a list with a single element will result in Just the empty list"
                            , details = [ "You can replace this call by Just the empty list." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just []
"""
                        ]
        , test "should replace List.tail <| List.singleton a by Just <| []" <|
            \() ->
                """module A exposing (..)
a = List.tail <| List.singleton b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.tail on a list with a single element will result in Just the empty list"
                            , details = [ "You can replace this call by Just the empty list." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| []
"""
                        ]
        , test "should replace List.singleton a |> List.tail by [] |> Just" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> List.tail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.tail on a list with a single element will result in Just the empty list"
                            , details = [ "You can replace this call by Just the empty list." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [] |> Just
"""
                        ]
        , test "should replace List.tail [ a ] by Just []" <|
            \() ->
                """module A exposing (..)
a = List.tail [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.tail on a list with a single element will result in Just the empty list"
                            , details = [ "You can replace this call by Just the empty list." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just []
"""
                        ]
        , test "should replace List.tail [ a, b, c ] by Just [ b, c ]" <|
            \() ->
                """module A exposing (..)
a = List.tail [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.tail on a list with some elements will result in Just the elements after the first"
                            , details = [ "You can replace this call by Just the list elements after the first." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just [ c, d ]
"""
                        ]
        , test "should replace List.tail (a :: bToZ) by Just (bToZ)" <|
            \() ->
                """module A exposing (..)
a = List.tail (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.tail on a list with some elements will result in Just the elements after the first"
                            , details = [ "You can replace this call by Just the list elements after the first." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (cToZ)
"""
                        ]
        ]


listMemberTests : Test
listMemberTests =
    describe "List.member"
        [ test "should not report List.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.member
b = List.member g
c = List.member g list
d = List.member g (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.member [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.member a []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on an empty list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.member a (List.singleton a) by True" <|
            \() ->
                """module A exposing (..)
a = List.member b (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member b (List.singleton a) by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on an list with a single element is equivalent to directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the list element are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member a [ a ] by True" <|
            \() ->
                """module A exposing (..)
a = List.member b [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member b [ a ] by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on an list with a single element is equivalent to directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the list element are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b <| [ a ] by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c <| [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on an list with a single element is equivalent to directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the list element are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b [ f a ] by b == (f a)" <|
            \() ->
                """module A exposing (..)
a = List.member c [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on an list with a single element is equivalent to directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the list element are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == (f b)
"""
                        ]
        , test "should replace [ a ] |> List.member b by a == b" <|
            \() ->
                """module A exposing (..)
a = [ b ] |> List.member c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on an list with a single element is equivalent to directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the list element are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b == c
"""
                        ]
        , test "should replace List.member c [ a, b, c ] by True" <|
            \() ->
                """module A exposing (..)
a = List.member d [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report List.member d [ a, b, c ]" <|
            \() ->
                """module A exposing (..)
a = List.member e [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.member a (a :: bToZ) by True" <|
            \() ->
                """module A exposing (..)
a = List.member b (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member c (a :: b :: c :: dToZ) by True" <|
            \() ->
                """module A exposing (..)
a = List.member d (b :: c :: d :: eToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report List.member d (a :: b :: c :: dToZ)" <|
            \() ->
                """module A exposing (..)
a = List.member e (b :: c :: d :: eToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


listMapTests : Test
listMapTests =
    describe "List.map"
        [ test "should not report List.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.map
b = List.map f
c = List.map f x
d = List.map f (Dict.fromList dict)
e = List.map (\\( a, b ) -> a + b) (Dict.fromList dict)
f = List.map (f >> Tuple.first) (Dict.fromList dict)
d = List.map f << Dict.fromList
e = List.map (\\( a, b ) -> a + b) << Dict.fromList
f = List.map (f >> Tuple.first) << Dict.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.map f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> List.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> List.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Dict.toList >> List.map Tuple.first by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace Dict.toList >> List.map (\\( part0, _ ) -> part0) by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map (\\( part0, _ ) -> part0)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace List.map Tuple.first << Dict.toList Tuple.first by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace Dict.toList >> List.map Tuple.second by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.second is the same as using Dict.values"
                            , details = [ "Using Dict.values directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace Dict.toList >> List.map (\\( _, part1 ) -> part1) by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map (\\( _, part1 ) -> part1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.second is the same as using Dict.values"
                            , details = [ "Using Dict.values directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace List.map Tuple.second << Dict.toList Tuple.second by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.second << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.second is the same as using Dict.values"
                            , details = [ "Using Dict.values directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace List.map Tuple.first (Dict.toList dict) by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (Dict.toList dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace List.map Tuple.first (Dict.toList <| dict) by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (Dict.toList <| dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace List.map Tuple.first (dict |> Dict.toList) by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (dict |> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace List.map Tuple.first <| Dict.toList dict by Dict.keys <| dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| Dict.toList dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys <| dict
"""
                        ]
        , test "should replace List.map Tuple.first <| (Dict.toList <| dict) by Dict.keys <| dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| (Dict.toList <| dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys <| dict
"""
                        ]
        , test "should replace List.map Tuple.first <| (dict |> Dict.toList) by Dict.keys <| dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| (dict |> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys <| dict
"""
                        ]
        , test "should replace Dict.toList dict |> List.map Tuple.first by dict |> Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList dict |> List.map Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.toList, then List.map Tuple.first is the same as using Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.keys
"""
                        ]
        ]


listFilterTests : Test
listFilterTests =
    describe "List.filter"
        [ test "should not report List.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filter f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filter f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filter f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (\\x -> True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\x -> True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace always True |> List.filter by identity" <|
            \() ->
                """module A exposing (..)
a = always True |> List.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter (always False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (\\x -> False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\x -> False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filter (always False) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filter <| (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace always False |> List.filter by always []" <|
            \() ->
                """module A exposing (..)
a = always False |> List.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        ]


listFilterMapTests : Test
listFilterMapTests =
    describe "List.filterMap"
        [ test "should not report List.filterMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filterMap f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap on an empty list will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap on an empty list will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filterMap f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filterMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap on an empty list will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filterMap (always Nothing) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap (always Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filterMap <| always Nothing by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap <| always Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace always Nothing |> List.filterMap by always []" <|
            \() ->
                """module A exposing (..)
a = always Nothing |> List.filterMap
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filterMap Just x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filterMap"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filterMap"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.filterMap Just by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filterMap"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filterMap"
                            , details = [ "You can remove this call and replace it by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filterMap (\\a -> Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just a) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just a) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as using List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> a) x
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just a)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as using List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> a)
"""
                        ]
        , test "should replace List.map (\\a -> Just b) x by List.filterMap (\\a -> b) x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as using List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> b) x
"""
                        ]
        , test "should replace List.filterMap identity (List.map f x) by List.filterMap f x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity (List.map f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.filterMap f x)
"""
                        ]
        , test "should replace List.filterMap identity <| List.map f <| x by List.filterMap f <| x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity <| List.map f <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f <| x
"""
                        ]
        , test "should replace x |> List.map f |> List.filterMap identity by x |> List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = x |> List.map f |> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> List.filterMap f
"""
                        ]
        , test "should replace List.map f >> List.filterMap identity by List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = List.map f >> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f
"""
                        ]
        , test "should replace List.filterMap identity << List.map f by List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity << List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f
"""
                        ]
        , test "should replace List.filterMap identity [ Just x, Just y ] by [ x, y ]" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity [ Just x, Just y ]
b = List.filterMap f [ Just x, Just y ]
c = List.filterMap identity [ Just x, y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.filterMap identity"
                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                            , under = "List.filterMap identity"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 28 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ x, y ]
b = List.filterMap f [ Just x, Just y ]
c = List.filterMap identity [ Just x, y ]
"""
                        ]
        , test "should replace [ Just x, Just y ] |> List.filterMap identity by [ x, y ]" <|
            \() ->
                """module A exposing (..)
a = [ Just x, Just y ] |> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.filterMap identity"
                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                            , under = "List.filterMap identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ x, y ]
"""
                        ]
        ]


listIndexedMapTests : Test
listIndexedMapTests =
    describe "List.indexedMap"
        [ test "should not report List.indexedMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap f x
b = List.indexedMap (\\i value -> i + value) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.indexedMap f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.indexedMap on an empty list will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.indexedMap (\\_ y -> y) x by List.map (\\y -> y) x" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\_ y -> y) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "Using List.indexedMap while ignoring the first argument is the same thing as calling List.map." ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\y -> y) x
"""
                        ]
        , test "should replace List.indexedMap (\\_ -> f) x by List.map f x" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\_ -> f) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "Using List.indexedMap while ignoring the first argument is the same thing as calling List.map." ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f) x
"""
                        ]
        , test "should replace List.indexedMap (always f) x by List.map f x" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (always f) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "Using List.indexedMap while ignoring the first argument is the same thing as calling List.map." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f) x
"""
                        ]
        , test "should replace List.indexedMap (always <| f y) x by List.map (f y) x" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (always <| f y) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "Using List.indexedMap while ignoring the first argument is the same thing as calling List.map." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f y) x
"""
                        ]
        , test "should replace List.indexedMap (f y |> always) x by List.map (f y) x" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (f y |> always) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.map instead"
                            , details = [ "Using List.indexedMap while ignoring the first argument is the same thing as calling List.map." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f y) x
"""
                        ]
        ]


listIsEmptyTests : Test
listIsEmptyTests =
    describe "List.isEmpty"
        [ test "should not report List.isEmpty with a non-literal argument" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.isEmpty [] by True" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.isEmpty [x] by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty [x]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (x :: xs) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (x :: xs)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace x :: xs |> List.isEmpty by False" <|
            \() ->
                """module A exposing (..)
a = x :: xs |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (List.singleton x) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (List.singleton x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


listSumTests : Test
listSumTests =
    describe "List.sum"
        [ test "should not report List.sum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sum
b = List.sum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sum (a :: bToZ)
b = List.sum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sum [] by 0" <|
            \() ->
                """module A exposing (..)
a = List.sum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sum on [] will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.sum [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.sum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Summing a list with a single element will result in the element itself"
                            , details = [ "You can replace this call by the single element itself." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        ]


listProductTests : Test
listProductTests =
    describe "List.product"
        [ test "should not report List.product on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.product
b = List.product list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.product on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.product (a :: bToZ)
b = List.product [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.product [] by 1" <|
            \() ->
                """module A exposing (..)
a = List.product []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.product on [] will result in 1"
                            , details = [ "You can replace this call by 1." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should replace List.product [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.product [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with a single element will result in the element itself"
                            , details = [ "You can replace this call by the single element itself." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        ]


listMinimumTests : Test
listMinimumTests =
    describe "List.minimum"
        [ test "should not report List.minimum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.minimum
b = List.minimum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.minimum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.minimum (a :: bToZ)
b = List.minimum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.minimum [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.minimum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.minimum on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.minimum [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.minimum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a list with a single element will result in Just the element itself"
                            , details = [ "You can replace this call by Just the single element itself." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just a
"""
                        ]
        ]


listMaximumTests : Test
listMaximumTests =
    describe "List.maximum"
        [ test "should not report List.maximum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.maximum
b = List.maximum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.maximum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.maximum (a :: bToZ)
b = List.maximum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.maximum [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.maximum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.maximum on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.maximum [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.maximum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.maximum on a list with a single element will result in Just the element itself"
                            , details = [ "You can replace this call by Just the single element itself." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just a
"""
                        ]
        ]


listFoldlTests : Test
listFoldlTests =
    describe "List.foldl"
        [ test "should not report List.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\el soFar -> soFar - el) 20 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.foldl f x [] by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (\\_ -> identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\_ -> identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (\\_ a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\_ a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always <| \\a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always <| \\a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always identity) x by always x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should replace List.foldl f x (Set.toList set) by Set.foldl f x set" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x set
"""
                        ]
        , test "should replace List.foldl f x << Set.toList by Set.foldl f x" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , test "should replace Set.toList >> List.foldl f x by Set.foldl f x" <|
            \() ->
                """module A exposing (..)
a = Set.toList >> List.foldl f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , listFoldlSumTests
        , listFoldlProductTests
        , listFoldlAllTests
        , listFoldlAnyTests
        ]


listFoldlAnyTests : Test
listFoldlAnyTests =
    describe "any"
        [ test "should replace List.foldl (||) True by always True" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should replace List.foldl (||) True list by True" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.foldl (||) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (||) False list by List.any identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity list
"""
                        ]
        , test "should replace List.foldl (||) False <| list by List.any identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity <| list
"""
                        ]
        , test "should replace list |> List.foldl (||) False by list |> List.any identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x -> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x || y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x || y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y || x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y || x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (||) y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (||) y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldl (||) False is the same as using List.any identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        ]


listFoldlAllTests : Test
listFoldlAllTests =
    describe "all"
        [ test "should replace List.foldl (&&) False by always False" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.foldl (&&) False list by False" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldl will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.foldl (&&) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (&&) True <| list by List.all identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity <| list
"""
                        ]
        , test "should replace list |> List.foldl (&&) True by list |> List.all identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.all identity
"""
                        ]
        , test "should replace List.foldl (&&) True list by List.all identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity list
"""
                        ]
        , test "should replace List.foldl (\\x -> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x && y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x && y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y && x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y && x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (&&) y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (&&) y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldl (&&) True is the same as using List.all identity." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        ]


listFoldlSumTests : Test
listFoldlSumTests =
    describe "sum"
        [ test "should replace List.foldl (+) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (+) 0 list by List.sum list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum list
"""
                        ]
        , test "should replace List.foldl (+) 0 <| list by List.sum <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum <| list
"""
                        ]
        , test "should replace list |> List.foldl (+) 0 by list |> List.sum" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.sum
"""
                        ]
        , test "should replace List.foldl (\\x -> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> x + y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x + y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> y + x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y + x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (+) y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (+) y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (+) initial list by initial + (List.sum list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum list)
"""
                        ]
        , test "should replace List.foldl (+) initial <| list by initial + (List.sum <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum <| list)
"""
                        ]
        , test "should replace list |> List.foldl (+) initial by ((list |> List.sum) + initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (+) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldl (+) 0 is the same as using List.sum." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.sum) + initial)
"""
                        ]
        ]


listFoldlProductTests : Test
listFoldlProductTests =
    describe "product"
        [ test "should replace List.foldl (*) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (*) 1 list by List.product list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product list
"""
                        ]
        , test "should replace List.foldl (*) 1 <| list by List.product <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product <| list
"""
                        ]
        , test "should replace list |> List.foldl (*) 1 by list |> List.product" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.product
"""
                        ]
        , test "should replace List.foldl (\\x -> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> x * y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x * y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> y * x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y * x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (*) y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (*) y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (*) initial list by initial * (List.product list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product list)
"""
                        ]
        , test "should replace List.foldl (*) initial <| list by initial * (List.product <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product <| list)
"""
                        ]
        , test "should replace list |> List.foldl (*) initial by ((list |> List.product) * initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (*) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldl (*) 1 is the same as using List.product." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.product) * initial)
"""
                        ]
        ]


listFoldrTests : Test
listFoldrTests =
    describe "List.foldr"
        [ test "should not report List.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\el soFar -> soFar - el) 20 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.foldr ... << Set.toList ... used with wrong amount of arguments" <|
            \() ->
                """module A exposing (..)
a = List.foldr reduce init << Set.toList list
a = List.foldr reduce init compileTimeError << Set.toList
a = List.foldr reduce << Set.toList
a = List.foldr << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.foldr fn x [] by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr fn x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (\\_ -> identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\_ -> identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (\\_ a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\_ a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always <| \\a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always <| \\a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always identity) x by always x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should replace List.foldr f x (Set.toList set) by Set.foldl f x set" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x set
"""
                        ]
        , test "should replace List.foldr f x << Set.toList by Set.foldr f x" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x
"""
                        ]
        , test "should replace Set.toList >> List.foldr f x by Set.foldr f x" <|
            \() ->
                """module A exposing (..)
a = Set.toList >> List.foldr f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x
"""
                        ]
        , listFoldrSumTests
        , listFoldrProductTests
        , listFoldrAllTests
        , listFoldrAnyTests
        ]


listFoldrAnyTests : Test
listFoldrAnyTests =
    describe "any"
        [ test "should replace List.foldr (||) True by always True" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should replace List.foldr (||) True list by True" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.foldr (||) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (||) False list by List.any identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity list
"""
                        ]
        , test "should replace List.foldr (||) False <| list by List.any identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity <| list
"""
                        ]
        , test "should replace list |> List.foldr (||) False by list |> List.any identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x -> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x || y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x || y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y || x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y || x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (||) y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (||) y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.any identity instead"
                            , details =
                                [ "Using List.foldr (||) False is the same as using List.any identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        ]


listFoldrAllTests : Test
listFoldrAllTests =
    describe "all"
        [ test "should replace List.foldr (&&) False by always False" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.foldr (&&) False list by False" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.foldr will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.foldr (&&) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (&&) True list by List.all identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity list
"""
                        ]
        , test "should replace List.foldr (&&) True <| list by List.all identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity <| list
"""
                        ]
        , test "should replace list |> List.foldr (&&) True by list |> List.all identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x -> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x && y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x && y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y && x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y && x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (&&) y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (&&) y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.all identity instead"
                            , details =
                                [ "Using List.foldr (&&) True is the same as using List.all identity." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        ]


listFoldrSumTests : Test
listFoldrSumTests =
    describe "sum"
        [ test "should replace List.foldr (+) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (+) 0 list by List.sum list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum list
"""
                        ]
        , test "should replace List.foldr (+) 0 <| list by List.sum <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum <| list
"""
                        ]
        , test "should replace list |> List.foldr (+) 0 by list |> List.sum" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.sum
"""
                        ]
        , test "should replace List.foldr (\\x -> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> x + y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x + y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> y + x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y + x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (+) y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (+) y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (+) initial list by initial + (List.sum list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum list)
"""
                        ]
        , test "should replace List.foldr (+) initial <| list by initial + (List.sum <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum <| list)
"""
                        ]
        , test "should replace list |> List.foldr (+) initial by ((list |> List.sum) + initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (+) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.sum instead"
                            , details =
                                [ "Using List.foldr (+) 0 is the same as using List.sum." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.sum) + initial)
"""
                        ]
        ]


listFoldrProductTests : Test
listFoldrProductTests =
    describe "product"
        [ test "should replace List.foldr (*) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (*) 1 list by List.product list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product list
"""
                        ]
        , test "should replace List.foldr (*) 1 <| list by List.product <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product <| list
"""
                        ]
        , test "should replace list |> List.foldr (*) 1 by list |> List.product" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.product
"""
                        ]
        , test "should replace List.foldr (\\x -> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> x * y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x * y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> y * x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y * x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (*) y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (*) y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (*) initial list by initial * (List.product list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product list)
"""
                        ]
        , test "should replace List.foldr (*) initial <| list by initial * (List.product <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product <| list)
"""
                        ]
        , test "should replace list |> List.foldr (*) initial by ((list |> List.product) * initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (*) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use List.product instead"
                            , details =
                                [ "Using List.foldr (*) 1 is the same as using List.product." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.product) * initial)
"""
                        ]
        ]


listAllTests : Test
listAllTests =
    describe "List.all"
        [ test "should not report List.all used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.all f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.all f [] by True" <|
            \() ->
                """module A exposing (..)
a = List.all f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.all will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) x by True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.all will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) by always True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.all will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always True)
"""
                        ]
        ]


listAnyTests : Test
listAnyTests =
    describe "List.any"
        [ test "should not report List.any used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.any f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.any f [] by False" <|
            \() ->
                """module A exposing (..)
a = List.any f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.any will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) x by False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.any will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) by always False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.any will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always False)
"""
                        ]
        ]


listRangeTests : Test
listRangeTests =
    describe "List.range"
        [ test "should not report List.range used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.range
a = List.range 5
a = List.range 5 10
a = List.range 5 0xF
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.range 10 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 10 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.range will result in []"
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.range 0xF 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 0xF 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.range will result in []"
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace 5 |> List.range 10 by []" <|
            \() ->
                """module A exposing (..)
a = 5 |> List.range 10
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.range will result in []"
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listLengthTests : Test
listLengthTests =
    describe "List.length"
        [ test "should not report List.length used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.length
a = List.length b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.length [] by 0" <|
            \() ->
                """module A exposing (..)
a = List.length []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.length [b, c, d] by 3" <|
            \() ->
                """module A exposing (..)
a = List.length [b, c, d]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 3"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace [] |> List.length by 0" <|
            \() ->
                """module A exposing (..)
a = [] |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]


listRepeatTests : Test
listRepeatTests =
    describe "List.repeat"
        [ test "should not report List.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.repeat n list
b = List.repeat 5 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace List.repeat n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.repeat 0 list by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat will result in an empty list"
                            , details = [ "Using List.repeat with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.repeat 0 by (always [])" <|
            \() ->
                """module A exposing (..)
a = List.repeat 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat will result in an empty list"
                            , details = [ "Using List.repeat with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.repeat -5 list by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat -5 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat will result in an empty list"
                            , details = [ "Using List.repeat with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should not replace List.repeat 1" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1 x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


listSortTests : Test
listSortTests =
    describe "List.sort"
        [ test "should not report List.sort on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sort
b = List.sort list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sort on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sort (a :: bToZ)
b = List.sort [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sort [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sort []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sort on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sort [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.sort [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sorting a list with a single element will result in the list itself"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a ]
"""
                        ]
        ]


listSortByTests : Test
listSortByTests =
    describe "List.sortBy"
        [ test "should not report List.sortBy with a function variable and a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn
b = List.sortBy fn list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortBy with a function variable and a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn (a :: bToZ)
b = List.sortBy fn [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sortBy fn [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sortBy fn [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
b = List.sortBy fn [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sorting a list with a single element will result in the list itself"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
b = [ a ]
"""
                        ]
        , test "should replace List.sortBy (always a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (always b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortBy (always a) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (always b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortBy (\\_ -> a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\_ -> b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortBy (\\_ -> a) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\_ -> b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortBy identity by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy identity is the same as using List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\a -> a) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\b -> b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy identity is the same as using List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy identity list by List.sort list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy identity list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy identity is the same as using List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort list
"""
                        ]
        , test "should replace List.sortBy (\\a -> a) list by List.sort list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\b -> b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortBy identity is the same as using List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort list
"""
                        ]
        ]


listSortWithTests : Test
listSortWithTests =
    describe "List.sortWith"
        [ test "should not report List.sortWith with a function variable and a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn
b = List.sortWith fn list
b = List.sortWith (always fn) list
b = List.sortWith (always (always fn)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortWith with a function variable and a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn (a :: bToZ)
b = List.sortWith fn [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sortWith fn [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sortWith fn [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
b = List.sortWith fn [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Sorting a list with a single element will result in the list itself"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
b = [ a ]
"""
                        ]
        , test "should replace List.sortWith (always (always GT)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always GT)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always GT)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always GT) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> GT) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> GT)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> GT)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always EQ)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always EQ)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always EQ)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always EQ) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> EQ) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> EQ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> EQ)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse list
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) <| list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT)) <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse <| list
"""
                        ]
        , test "should replace list |> List.sortWith (always (always LT)) by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.sortWith (always (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.reverse
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always LT) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> LT) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> LT)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> LT)) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        ]


listReverseTests : Test
listReverseTests =
    describe "List.reverse"
        [ test "should not report List.reverse that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.reverse
b = List.reverse str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.reverse [] by []" <|
            \() ->
                """module A exposing (..)
a = List.reverse []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.reverse on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.reverse <| List.reverse <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.reverse <| List.reverse <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double reversal"
                            , details = [ "Composing `reverse` with `reverse` cancel each other out." ]
                            , under = "List.reverse <| List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]


listTakeTests : Test
listTakeTests =
    describe "List.take"
        [ test "should not report List.take that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.take 2 x
b = List.take y [ 1, 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.take n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.take n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.take on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.take 0 x by []" <|
            \() ->
                """module A exposing (..)
a = List.take 0 x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Taking 0 items from a list will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.take 0 by (always [])" <|
            \() ->
                """module A exposing (..)
a = List.take 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Taking 0 items from a list will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        ]


listDropTests : Test
listDropTests =
    describe "List.drop"
        [ test "should not report List.drop that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.drop 2 x
b = List.drop y [ 1, 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.drop n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.drop n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.drop on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.drop 0 x by x" <|
            \() ->
                """module A exposing (..)
a = List.drop 0 x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dropping 0 items from a list will result in the list itself"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.drop 0 by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.drop 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dropping 0 items from a list will result in the list itself"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.drop 0 by identity" <|
            \() ->
                """module A exposing (..)
a = List.drop 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dropping 0 items from a list will result in the list itself"
                            , details = [ "You can replace this function by identity." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


listPartitionTests : Test
listPartitionTests =
    describe "List.partition"
        [ test "should not report List.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.partition f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.partition f [] by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace List.partition f <| [] by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace [] |> List.partition f by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = [] |> List.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace List.partition (always True) x by ( x, [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first list"
                            , details = [ "Since the predicate function always returns True, the second list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( x, [] )
"""
                        ]
        , test "should not replace List.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
a = List.partition (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.partition (always False) x by ( [], x )" <|
            \() ->
                """module A exposing (..)
a = List.partition (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], x )
"""
                        ]
        , test "should replace List.partition (always False) by (Tuple.pair [])" <|
            \() ->
                """module A exposing (..)
a = List.partition (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        , test "should replace List.partition <| (always False) by (Tuple.pair [])" <|
            \() ->
                """module A exposing (..)
a = List.partition <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        , test "should replace always False |> List.partition by Tuple.pair []" <|
            \() ->
                """module A exposing (..)
a = always False |> List.partition
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        ]


listIntersperseTests : Test
listIntersperseTests =
    describe "List.intersperse"
        [ test "should not report List.intersperse that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.intersperse 2 x
b = List.intersperse y [ 1, 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.intersperse n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.intersperse x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.intersperse on an empty list will result in an empty list"
                            , details = [ "You can replace this call by an empty list." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listUnzipTests : Test
listUnzipTests =
    describe "List.unzip"
        [ test "should not report List.unzip on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.unzip
b = List.unzip list
c = List.unzip [ h ]
d = List.unzip (h :: t)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.unzip [] by []" <|
            \() ->
                """module A exposing (..)
a = List.unzip []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.unzip on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.unzip"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        ]


listMap2Tests : Test
listMap2Tests =
    describe "List.map2"
        [ test "should not report List.map2 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map2 f
a = List.map2 f list0
b = List.map2 f list0 list1
c = List.map2 f [ h ] list1
d = List.map2 f (h :: t) list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map2 f [] list1 by []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map2 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map2 f [] by always []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map2 with any list being [] will result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map2 f list0 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f list0 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map2 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap3Tests : Test
listMap3Tests =
    describe "List.map3"
        [ test "should not report List.map3 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map3 f
a = List.map3 f list0
b = List.map3 f list0 list1
b = List.map3 f list0 list1 list2
c = List.map3 f [ h ] list1 list2
d = List.map3 f (h :: t) list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map3 f [] list1 list2 by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map3 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map3 f [] list1 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map3 with any list being [] will result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map3 f [] list1 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map3 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map3 with any list being [] will result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map3 f list0 [] list2 by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f list0 [] list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map3 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map3 f list0 list1 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f list0 list1 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map3 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap4Tests : Test
listMap4Tests =
    describe "List.map4"
        [ test "should not report List.map4 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map4 f
a = List.map4 f list0
b = List.map4 f list0 list1
b = List.map4 f list0 list1 list2
b = List.map4 f list0 list1 list2 list3
c = List.map4 f [ h ] list1 list2 list3
d = List.map4 f (h :: t) list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map4 f [] list1 list2 list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map4 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f [] list1 list2 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map4 with any list being [] will result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map4 f [] list1 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map4 with any list being [] will result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map4 f [] by (\\_ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map4 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map4 with any list being [] will result in []"
                            , details = [ "You can replace this call by (\\_ _ _ -> [])." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ -> [])
"""
                        ]
        , test "should replace List.map4 f list0 [] list2 list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 [] list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map4 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f list0 list1 [] list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 list1 [] list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map4 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f list0 list1 list2 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 list1 list2 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map4 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap5Tests : Test
listMap5Tests =
    describe "List.map5"
        [ test "should not report List.map5 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map5 f
a = List.map5 f list0
b = List.map5 f list0 list1
b = List.map5 f list0 list1 list2
b = List.map5 f list0 list1 list2 list3
b = List.map5 f list0 list1 list2 list3 list4
c = List.map5 f [ h ] list1 list2 list3 list4
d = List.map5 f (h :: t) list1 list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map5 f [] list1 list2 list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f [] list1 list2 list3 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map5 f [] list1 list2 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map5 f [] list1 by (\\_ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by (\\_ _ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ -> [])
"""
                        ]
        , test "should replace List.map5 f [] by (\\_ _ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by (\\_ _ _ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ _ -> [])
"""
                        ]
        , test "should replace List.map5 f list0 [] list2 list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 [] list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 [] list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 [] list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 list2 [] list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 list2 [] list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 list2 list3 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 list2 list3 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map5 with any list being [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]



-- Maybe


maybeTests : Test
maybeTests =
    describe "Maybe"
        [ maybeMapTests
        , maybeAndThenTests
        , maybeWithDefaultTests
        ]


maybeMapTests : Test
maybeMapTests =
    describe "Maybe.map"
        [ test "should not report Maybe.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.map f Nothing by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.map f <| Nothing by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f <| Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Nothing |> Maybe.map f by Nothing" <|
            \() ->
                """module A exposing (..)
a = Nothing |> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map with an identity function is the same as not using Maybe.map"
                            , details = [ "You can remove this call and replace it by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map with an identity function is the same as not using Maybe.map"
                            , details = [ "You can remove this call and replace it by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Maybe.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Maybe.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map with an identity function is the same as not using Maybe.map"
                            , details = [ "You can remove this call and replace it by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = Maybe.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map with an identity function is the same as not using Maybe.map"
                            , details = [ "You can remove this call and replace it by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Maybe.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Maybe.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map with an identity function is the same as not using Maybe.map"
                            , details = [ "You can remove this call and replace it by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Maybe.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Maybe.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.map with an identity function is the same as not using Maybe.map"
                            , details = [ "You can remove this call and replace it by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Maybe.map f (Just x) by Just (f x)" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f (Just x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f (x))
"""
                        ]
        , test "should replace Maybe.map f <| Just x by Just (f x)" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f <| Just x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f <| x)
"""
                        ]
        , test "should replace Just x |> Maybe.map f by x |> f |> Just" <|
            \() ->
                """module A exposing (..)
a = Just x |> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f |> Just
"""
                        ]
        , test "should replace x |> Just |> Maybe.map f by x |> f |> Just" <|
            \() ->
                """module A exposing (..)
a = x |> Just |> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f |> Just
"""
                        ]
        , test "should replace Maybe.map f <| Just <| x by Just <| f <| x" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f <| Just <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f <| x)
"""
                        ]
        , test "should replace Maybe.map f << Just by Just << f" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just << f
"""
                        ]
        , test "should replace Just >> Maybe.map f by f >> Just" <|
            \() ->
                """module A exposing (..)
a = Just >> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Just
"""
                        ]
        , test "should replace Maybe.map f << Just << a by Just << f << a" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f << Just << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just << f << a
"""
                        ]
        , test "should replace g << Maybe.map f << Just by g << Just << f" <|
            \() ->
                """module A exposing (..)
a = g << Maybe.map f << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g << Just << f
"""
                        ]
        , test "should replace Just >> Maybe.map f >> g by f >> Just >> g" <|
            \() ->
                """module A exposing (..)
a = Just >> Maybe.map f >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.map on a value that is Just"
                            , details = [ "The function can be called without Maybe.map." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Just >> g
"""
                        ]
        ]


maybeAndThenTests : Test
maybeAndThenTests =
    describe "Maybe.andThen"
        [ test "should not report Maybe.andThen used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.andThen f Nothing by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen f Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.andThen on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.andThen Just x by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen Just x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.andThen with a function that will always return Just is the same as not using Maybe.andThen"
                            , details = [ "You can remove this call and replace it by the value itself." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.andThen (always Nothing) x by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (always Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.andThen with a function that will always return Nothing will result in Nothing"
                            , details = [ "You can remove this call and replace it by Nothing." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.andThen (\\b -> Just b) x by Maybe.map (\\b -> b) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\b -> Just b) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead"
                            , details = [ "Using Maybe.andThen with a function that always returns Just is the same thing as using Maybe.map." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (\\b -> b) x
"""
                        ]
        , test "should replace Maybe.andThen (\\b -> if cond then Just b else Just c) x by Maybe.map (\\b -> if cond then b else c) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\b -> if cond then Just b else Just c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead"
                            , details = [ "Using Maybe.andThen with a function that always returns Just is the same thing as using Maybe.map." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (\\b -> if cond then b else c) x
"""
                        ]
        , test "should not report Maybe.andThen (\\b -> if cond then Just b else Nothing) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\b -> if cond then Just b else Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.andThen (\\b -> case b of C -> Just b ; D -> Just c) x by Maybe.map (\\b -> case b of C -> b ; D -> c) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (
    \\b ->
        case b of
            C -> Just b
            D -> Just c
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead"
                            , details = [ "Using Maybe.andThen with a function that always returns Just is the same thing as using Maybe.map." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (
    \\b ->
        case b of
            C -> b
            D -> c
    ) x
"""
                        ]
        , test "should replace Maybe.andThen (\\b -> let y = 1 in Just y) x by Maybe.map (\\b -> let y = 1 in y) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (
    \\b ->
        let
            y = 1
        in
        Just y
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead"
                            , details = [ "Using Maybe.andThen with a function that always returns Just is the same thing as using Maybe.map." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (
    \\b ->
        let
            y = 1
        in
        y
    ) x
"""
                        ]
        , test "should not report Maybe.andThen (\\b -> case b of C -> Just b ; D -> Nothing) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (
    \\b ->
        case b of
            C -> Just b
            D -> Nothing
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.andThen f (Just x) by f (x)" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen f (Just x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.andThen on a value that is known to be Just"
                            , details = [ "You can remove the Just and just call the function directly." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f (x)
"""
                        ]
        , test "should replace Just x |> Maybe.andThen f by f (x)" <|
            \() ->
                """module A exposing (..)
a = Just x |> Maybe.andThen f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Maybe.andThen on a value that is known to be Just"
                            , details = [ "You can remove the Just and just call the function directly." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        ]


maybeWithDefaultTests : Test
maybeWithDefaultTests =
    describe "Maybe.withDefault"
        [ test "should not report Maybe.withDefault used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.withDefault x Nothing by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.withDefault on Nothing will result in the default value"
                            , details = [ "You can replace this call by the default value." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.withDefault x (Just y) by y" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x (Just y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.withDefault on a value that is Just will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Just." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (y)
"""
                        ]
        , test "should replace Maybe.withDefault x <| (Just y) by y" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x <| (Just y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.withDefault on a value that is Just will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Just." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (y)
"""
                        ]
        , test "should replace (Just y) |> Maybe.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = (Just y) |> Maybe.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.withDefault on a value that is Just will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Just." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (y)
"""
                        ]
        , test "should replace y |> Just |> Maybe.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = y |> Just |> Maybe.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Maybe.withDefault on a value that is Just will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Just." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        ]



-- Result


resultTests : Test
resultTests =
    describe "Result"
        [ resultMapTests
        , resultMapErrorTests
        , resultAndThenTests
        , resultWithDefaultTests
        , resultToMaybeTests
        ]


resultMapTests : Test
resultMapTests =
    describe "Result.map"
        [ test "should not report Result.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.map f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.map f (Err z) by (Err z)" <|
            \() ->
                """module A exposing (..)
a = Result.map f (Err z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map on an error will result in an error"
                            , details = [ "You can replace this call by an error." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Err z)
"""
                        ]
        , test "should replace Result.map f <| Err z by Err z" <|
            \() ->
                """module A exposing (..)
a = Result.map f <| Err z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map on an error will result in an error"
                            , details = [ "You can replace this call by an error." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err z
"""
                        ]
        , test "should replace Err z |> Result.map f by Err z" <|
            \() ->
                """module A exposing (..)
a = Err z |> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map on an error will result in an error"
                            , details = [ "You can replace this call by an error." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err z
"""
                        ]
        , test "should replace Result.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = Result.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map with an identity function is the same as not using Result.map"
                            , details = [ "You can remove this call and replace it by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Result.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map with an identity function is the same as not using Result.map"
                            , details = [ "You can remove this call and replace it by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Result.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Result.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map with an identity function is the same as not using Result.map"
                            , details = [ "You can remove this call and replace it by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map with an identity function is the same as not using Result.map"
                            , details = [ "You can remove this call and replace it by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map with an identity function is the same as not using Result.map"
                            , details = [ "You can remove this call and replace it by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Result.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Result.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.map with an identity function is the same as not using Result.map"
                            , details = [ "You can remove this call and replace it by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.map f (Ok x) by Ok (f x)" <|
            \() ->
                """module A exposing (..)
a = Result.map f (Ok x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok (f (x))
"""
                        ]
        , test "should replace Result.map f <| Ok x by Ok (f x)" <|
            \() ->
                """module A exposing (..)
a = Result.map f <| Ok x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok (f <| x)
"""
                        ]
        , test "should replace Ok x |> Result.map f by x |> f |> Ok" <|
            \() ->
                """module A exposing (..)
a = Ok x |> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f |> Ok
"""
                        ]
        , test "should replace x |> Ok |> Result.map f by x |> f |> Ok" <|
            \() ->
                """module A exposing (..)
a = x |> Ok |> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f |> Ok
"""
                        ]
        , test "should replace Result.map f <| Ok <| x by Ok <| f <| x" <|
            \() ->
                """module A exposing (..)
a = Result.map f <| Ok <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok (f <| x)
"""
                        ]
        , test "should replace Result.map f << Ok by Ok << f" <|
            \() ->
                """module A exposing (..)
a = Result.map f << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok << f
"""
                        ]
        , test "should replace Ok >> Result.map f by f >> Ok" <|
            \() ->
                """module A exposing (..)
a = Ok >> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Ok
"""
                        ]
        , test "should replace Result.map f << Ok << a by Ok << f << a" <|
            \() ->
                """module A exposing (..)
a = Result.map f << Ok << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok << f << a
"""
                        ]
        , test "should replace g << Result.map f << Ok by g << Ok << f" <|
            \() ->
                """module A exposing (..)
a = g << Result.map f << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g << Ok << f
"""
                        ]
        , test "should replace Ok >> Result.map f >> g by f >> Ok >> g" <|
            \() ->
                """module A exposing (..)
a = Ok >> Result.map f >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.map on a value that is Ok"
                            , details = [ "The function can be called without Result.map." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Ok >> g
"""
                        ]
        ]


resultMapErrorTests : Test
resultMapErrorTests =
    describe "Result.mapError"
        [ test "should not report Result.mapError used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.mapError
b = Result.mapError f
c = Result.mapError f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.mapError f (Ok z) by (Ok z)" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f (Ok z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.mapError on a value that is Ok will always return the Ok result value"
                            , details = [ "You can remove the Result.mapError call." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Ok z)
"""
                        ]
        , test "should replace Result.mapError f <| Ok z by Ok z" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f <| Ok z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.mapError on a value that is Ok will always return the Ok result value"
                            , details = [ "You can remove the Result.mapError call." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok z
"""
                        ]
        , test "should replace Ok z |> Result.mapError f by Ok z" <|
            \() ->
                """module A exposing (..)
a = Ok z |> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.mapError on a value that is Ok will always return the Ok result value"
                            , details = [ "You can remove the Result.mapError call." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok z
"""
                        ]
        , test "should replace Result.mapError identity x by x" <|
            \() ->
                """module A exposing (..)
a = Result.mapError identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError identity will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.mapError identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Result.mapError identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError identity will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Result.mapError identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Result.mapError identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError identity will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.mapError identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.mapError identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError identity will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.mapError <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.mapError <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError identity will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Result.mapError by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Result.mapError
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError identity will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.mapError f (Err x) by Err (f x)" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f (Err x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err (f x)
"""
                        ]
        , test "should replace Result.mapError f <| Err x by Err (f x)" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f <| Err x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err (f <| x)
"""
                        ]
        , test "should replace Err x |> Result.mapError f by x |> f |> Err" <|
            \() ->
                """module A exposing (..)
a = Err x |> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f |> Err
"""
                        ]
        , test "should replace x |> Err |> Result.mapError f by x |> f |> Err" <|
            \() ->
                """module A exposing (..)
a = x |> Err |> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f |> Err
"""
                        ]
        , test "should replace Result.mapError f <| Err <| x by Err <| f <| x" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f <| Err <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err (f <| x)
"""
                        ]
        , test "should replace Result.mapError f << Err by Err << f" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should replace Err >> Result.mapError f by f >> Err" <|
            \() ->
                """module A exposing (..)
a = Err >> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Err
"""
                        ]
        , test "should replace Result.mapError f << Err << a by Err << f << a" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << Err << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f << a
"""
                        ]
        , test "should replace g << Result.mapError f << Err by g << Err << f" <|
            \() ->
                """module A exposing (..)
a = g << Result.mapError f << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g << Err << f
"""
                        ]
        , test "should replace Err >> Result.mapError f >> g by f >> Err >> g" <|
            \() ->
                """module A exposing (..)
a = Err >> Result.mapError f >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.mapError on Err will result in Err with the function applied to the error"
                            , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Err >> g
"""
                        ]
        ]


resultAndThenTests : Test
resultAndThenTests =
    describe "Result.andThen"
        [ test "should not report Result.andThen used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.andThen f (Err z) by (Err z)" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f (Err z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.andThen on an error will result in the error"
                            , details = [ "You can replace this call by the error itself." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Err z)
"""
                        ]
        , test "should not report Result.andThen (always (Err z)) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (always (Err z)) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.andThen Ok x by Result.map identity x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen Ok x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.andThen with a function that will always return Just is the same as not using Result.andThen"
                            , details = [ "You can remove this call and replace it by the value itself." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.andThen (\\b -> Ok b) x by Result.map (\\b -> b) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> Ok b) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Result.map instead"
                            , details = [ "Using Result.andThen with a function that always returns Ok is the same thing as using Result.map." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Result.map (\\b -> b) x
"""
                        ]
        , test "should replace Result.andThen (\\b -> let y = 1 in Ok y) x by Result.map (\\b -> let y = 1 in y) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> let y = 1 in Ok y) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Result.map instead"
                            , details = [ "Using Result.andThen with a function that always returns Ok is the same thing as using Result.map." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Result.map (\\b -> let y = 1 in y) x
"""
                        ]
        , test "should replace Result.andThen (\\b -> if cond then Ok b else Ok c) x by Result.map (\\b -> if cond then b else c) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> if cond then Ok b else Ok c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Result.map instead"
                            , details = [ "Using Result.andThen with a function that always returns Ok is the same thing as using Result.map." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Result.map (\\b -> if cond then b else c) x
"""
                        ]
        , test "should not report Result.andThen (\\b -> if cond then Ok b else Err c) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> if cond then Ok b else Err c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.andThen f (Ok x) by f (x)" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f (Ok x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.andThen on a value that is known to be Ok"
                            , details = [ "You can remove the Ok and just call the function directly." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f (x)
"""
                        ]
        , test "should replace Ok x |> Result.andThen f by f (x)" <|
            \() ->
                """module A exposing (..)
a = Ok x |> Result.andThen f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calling Result.andThen on a value that is known to be Ok"
                            , details = [ "You can remove the Ok and just call the function directly." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        ]


resultWithDefaultTests : Test
resultWithDefaultTests =
    describe "Result.withDefault"
        [ test "should not report Result.withDefault used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.withDefault x (Err z) by x" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x (Err z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.withDefault on an error will result in the default value"
                            , details = [ "You can replace this call by the default value." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.withDefault x (Ok y) by y" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x (Ok y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.withDefault on a value that is Ok will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Ok." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (y)
"""
                        ]
        , test "should replace Result.withDefault x <| (Ok y) by y" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x <| (Ok y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.withDefault on a value that is Ok will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Ok." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (y)
"""
                        ]
        , test "should replace (Ok y) |> Result.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = (Ok y) |> Result.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.withDefault on a value that is Ok will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Ok." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (y)
"""
                        ]
        , test "should replace y |> Ok |> Result.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = y |> Ok |> Result.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.withDefault on a value that is Ok will result in that value"
                            , details = [ "You can replace this call by the value wrapped in Ok." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        ]


resultToMaybeTests : Test
resultToMaybeTests =
    describe "Result.toMaybe"
        [ test "should not report Result.toMaybe used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe
b = Result.toMaybe x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.toMaybe (Err a) by Nothing" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe (Err b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.toMaybe on an error will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Result.toMaybe (Ok a) by Just (a)" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe (Ok b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.toMaybe on a value that is Ok will result in Just that value itself"
                            , details = [ "You can replace this call by the value itself wrapped in Just." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (b)
"""
                        ]
        , test "should replace Result.toMaybe <| Ok a by Just <| a" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe <| Ok b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.toMaybe on a value that is Ok will result in Just that value itself"
                            , details = [ "You can replace this call by the value itself wrapped in Just." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| b
"""
                        ]
        , test "should replace Ok a |> Result.toMaybe by a |> Just" <|
            \() ->
                """module A exposing (..)
a = Ok b |> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.toMaybe on a value that is Ok will result in Just that value itself"
                            , details = [ "You can replace this call by the value itself wrapped in Just." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> Just
"""
                        ]
        , test "should replace a |> Ok |> Result.toMaybe by a |> Just" <|
            \() ->
                """module A exposing (..)
a = b |> Ok |> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.toMaybe on a value that is Ok will result in Just that value itself"
                            , details = [ "You can replace this call by the value itself wrapped in Just." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> Just
"""
                        ]
        , test "should replace Ok >> Result.toMaybe by Just" <|
            \() ->
                """module A exposing (..)
a = Ok >> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.toMaybe on a value that is Ok will result in Just that value itself"
                            , details = [ "You can replace this call by Just." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just
"""
                        ]
        , test "should replace Err >> Result.toMaybe by always Nothing" <|
            \() ->
                """module A exposing (..)
a = Err >> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Result.toMaybe on an error will result in Nothing"
                            , details = [ "You can replace this call by always Nothing." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always Nothing
"""
                        ]
        ]



-- Set


setSimplificationTests : Test
setSimplificationTests =
    describe "Set"
        [ setMapTests
        , setFilterTests
        , setIsEmptyTests
        , setSizeTests
        , setFromListTests
        , setToListTests
        , setPartitionTests
        , setRemoveTests
        , setMemberTests
        , setIntersectTests
        , setDiffTests
        , setUnionTests
        , setInsertTests
        ]


setMapTests : Test
setMapTests =
    describe "Set.map"
        [ test "should not report Set.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.map f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.map f Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.map f Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.map f <| Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.map f <| Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.empty |> Set.map f by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = Set.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Set.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Set.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Set.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = Set.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Set.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Set.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Set.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Set.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


setFilterTests : Test
setFilterTests =
    describe "Set.filter"
        [ test "should not report Set.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.filter f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.filter f Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter f Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter f <| Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter f <| Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.empty |> Set.filter f by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always True) x by x" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.filter (\\x -> True) x by x" <|
            \() ->
                """module A exposing (..)
a = Set.filter (\\x -> True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Set.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = Set.filter <| (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace always True |> Set.filter by identity" <|
            \() ->
                """module A exposing (..)
a = always True |> Set.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Set.filter (always False) x by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (\\x -> False) x by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (\\x -> False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always False) <| x by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always False) <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace x |> Set.filter (always False) by Set.empty" <|
            \() ->
                """module A exposing (..)
a = x |> Set.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always False) by always Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always Set.empty
"""
                        ]
        , test "should replace Set.filter <| (always False) by always Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always Set.empty
"""
                        ]
        , test "should replace always False |> Set.filter by always Set.empty" <|
            \() ->
                """module A exposing (..)
a = always False |> Set.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always Set.empty
"""
                        ]
        ]


setSizeTests : Test
setSizeTests =
    describe "Set.size"
        [ test "should not report Set.size used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.size
a = Set.size b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.size Set.empty by 0" <|
            \() ->
                """module A exposing (..)
a = Set.size Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 0"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should not replace Set.size (Set.fromList [b, c, d])" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [b, c, d])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.size (Set.fromList []) by 0" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.fromList will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.size (Set.empty)
"""
                        , Review.Test.error
                            { message = "The size of the set is 0"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace Set.size (Set.fromList [a]) by 1" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [a])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 1"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        , Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.size (Set.singleton a)
"""
                        ]
        , test "should replace Set.size (Set.fromList [1, 2, 3]) by 3" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [1, 2, 3])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 3"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace Set.size (Set.fromList [1, 2, 3, 3, 0x3]) by 3" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [1, 2, 3, 3, 0x3])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 3"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace Set.size (Set.fromList [2, -2, -(-2)]) by 2" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [2, -2, -2])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should replace Set.size (Set.fromList [1.3, -1.3, 2.1, 2.1]) by 3" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [1.3, -1.3, 2.1, 2.1])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 3"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace Set.size (Set.fromList [\"foo\", \"bar\", \"foo\"]) by 2" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList ["foo", "bar", "foo"])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should replace Set.size (Set.fromList ['a', 'b', ('a')]) by 2" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList ['a', 'b', ('a')])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should replace Set.size (Set.fromList [([1, 2], [3, 4]), ([1, 2], [3, 4]), ([], [1])]) by 2" <|
            \() ->
                """module A exposing (..)
a = Set.size (Set.fromList [([1, 2], [3, 4]), ([1, 2], [3, 4]), ([], [1])])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should replace Set.empty |> Set.size by 0" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 0"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace Set.singleton x |> Set.size by 1" <|
            \() ->
                """module A exposing (..)
a = Set.singleton x |> Set.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 1"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        ]


setIsEmptyTests : Test
setIsEmptyTests =
    describe "Set.isEmpty"
        [ test "should not report Set.isEmpty with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.isEmpty
b = Set.isEmpty list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.isEmpty Set.empty by True" <|
            \() ->
                """module A exposing (..)
a = Set.isEmpty Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace Set.isEmpty (Set.fromList [x]) by False" <|
            \() ->
                """module A exposing (..)
a = Set.isEmpty (Set.fromList [x])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        , Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.isEmpty (Set.singleton x)
"""
                        ]
        , test "should replace Set.isEmpty (Set.fromList []) by False" <|
            \() ->
                """module A exposing (..)
a = Set.isEmpty (Set.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        , Review.Test.error
                            { message = "The call to Set.fromList will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.isEmpty (Set.empty)
"""
                        ]
        , test "should replace Set.isEmpty (Set.singleton x) by False" <|
            \() ->
                """module A exposing (..)
a = Set.isEmpty (Set.singleton x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace x :: xs |> Set.isEmpty by False" <|
            \() ->
                """module A exposing (..)
a = Set.singleton x |> Set.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


setFromListTests : Test
setFromListTests =
    describe "Set.fromList"
        [ test "should not report Set.fromList with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.fromList
b = Set.fromList list
c = Set.fromList (x :: ys)
d = Set.fromList [x, y]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.fromList [] by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.fromList will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.fromList [ a ] by Set.singleton a" <|
            \() ->
                """module A exposing (..)
a = Set.fromList [ b ]
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton b
"""
                        ]
        , test "should replace Set.fromList [ f a ] by Set.singleton (f a)" <|
            \() ->
                """module A exposing (..)
a = Set.fromList [ f b ]
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton (f b)
"""
                        ]
        , test "should replace Set.fromList (List.singleton a) by Set.singleton a" <|
            \() ->
                """module A exposing (..)
a = Set.fromList (List.singleton b)
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton b
"""
                        ]
        , test "should replace Set.fromList <| List.singleton a by Set.singleton <| a" <|
            \() ->
                """module A exposing (..)
a = Set.fromList <| List.singleton b
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton <| b
"""
                        ]
        , test "should replace List.singleton a |> Set.fromList by a |> Set.singleton" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> Set.fromList
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> Set.singleton
"""
                        ]
        , test "should replace List.singleton >> Set.fromList by Set.singleton" <|
            \() ->
                """module A exposing (..)
a = List.singleton >> Set.fromList
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton
"""
                        ]
        , test "should replace Set.fromList << List.singleton by Set.singleton" <|
            \() ->
                """module A exposing (..)
a = Set.fromList << List.singleton
"""
                    |> Review.Test.run (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList with a single element can be replaced using Set.singleton"
                            , details = [ "You can replace this call by Set.singleton with the list element itself." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton
"""
                        ]
        ]


setToListTests : Test
setToListTests =
    describe "Set.toList"
        [ test "should not report Set.toList with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.toList
b = Set.toList list
c = Set.toList set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.toList Set.empty by []" <|
            \() ->
                """module A exposing (..)
a = Set.toList Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Set.toList will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "Set.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


setPartitionTests : Test
setPartitionTests =
    describe "Set.partition"
        [ test "should not report Set.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.partition f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.partition f Set.empty by ( Set.empty, Set.empty )" <|
            \() ->
                """module A exposing (..)
a = Set.partition f Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.partition on Set.empty will result in ( Set.empty, Set.empty )"
                            , details = [ "You can replace this call by ( Set.empty, Set.empty )." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Set.empty, Set.empty )
"""
                        ]
        , test "should replace Set.partition f <| Set.empty by ( Set.empty, Set.empty )" <|
            \() ->
                """module A exposing (..)
a = Set.partition f <| Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.partition on Set.empty will result in ( Set.empty, Set.empty )"
                            , details = [ "You can replace this call by ( Set.empty, Set.empty )." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Set.empty, Set.empty )
"""
                        ]
        , test "should replace Set.empty |> Set.partition f by ( Set.empty, Set.empty )" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.partition on Set.empty will result in ( Set.empty, Set.empty )"
                            , details = [ "You can replace this call by ( Set.empty, Set.empty )." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Set.empty, Set.empty )
"""
                        ]
        , test "should replace Set.partition (always True) x by ( x, Set.empty )" <|
            \() ->
                """module A exposing (..)
a = Set.partition (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first set"
                            , details = [ "Since the predicate function always returns True, the second set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( x, Set.empty )
"""
                        ]
        , test "should not replace Set.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
a = Set.partition (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.partition (always False) x by ( Set.empty, x )" <|
            \() ->
                """module A exposing (..)
a = Set.partition (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Set.empty, x )
"""
                        ]
        , test "should replace Set.partition (always False) by (Tuple.pair Set.empty)" <|
            \() ->
                """module A exposing (..)
a = Set.partition (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair Set.empty)
"""
                        ]
        , test "should replace Set.partition <| (always False) by (Tuple.pair Set.empty)" <|
            \() ->
                """module A exposing (..)
a = Set.partition <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair Set.empty)
"""
                        ]
        , test "should replace always False |> Set.partition by Tuple.pair Set.empty" <|
            \() ->
                """module A exposing (..)
a = always False |> Set.partition
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair Set.empty)
"""
                        ]
        ]


setRemoveTests : Test
setRemoveTests =
    describe "Set.remove"
        [ test "should not report Set.remove used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.remove x x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.remove x Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.remove x Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.remove on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.remove"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        ]


setMemberTests : Test
setMemberTests =
    describe "Set.member"
        [ test "should not report Set.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.member x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.member x Set.empty by False" <|
            \() ->
                """module A exposing (..)
a = Set.member x Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.member on Set.empty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


setIntersectTests : Test
setIntersectTests =
    describe "Set.intersect"
        [ test "should not report Set.intersect used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.intersect x x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.intersect Set.empty set by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.intersect Set.empty set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.intersect on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.intersect set Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.intersect set Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.intersect on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        ]


setDiffTests : Test
setDiffTests =
    describe "Set.diff"
        [ test "should not report Set.diff used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.diff x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.diff Set.empty set by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.diff Set.empty set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Diffing Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.diff set Set.empty by set" <|
            \() ->
                """module A exposing (..)
a = Set.diff set Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Diffing a set with Set.empty will result in the set itself"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = set
"""
                        ]
        , test "should replace Set.empty |> Set.diff set by set" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.diff set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Diffing a set with Set.empty will result in the set itself"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = set
"""
                        ]
        ]


setUnionTests : Test
setUnionTests =
    describe "Set.union"
        [ test "should not report Set.union used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.union x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.union Set.empty set by set" <|
            \() ->
                """module A exposing (..)
a = Set.union Set.empty set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary union with Set.empty"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = set
"""
                        ]
        , test "should replace Set.union set Set.empty by set" <|
            \() ->
                """module A exposing (..)
a = Set.union set Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary union with Set.empty"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = set
"""
                        ]
        , test "should replace Set.empty |> Set.union set by set" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.union set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary union with Set.empty"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = set
"""
                        ]
        , test "should replace set |> Set.union Set.empty by set" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.union set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary union with Set.empty"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = set
"""
                        ]
        ]


setInsertTests : Test
setInsertTests =
    describe "Set.insert"
        [ test "should not report Set.insert used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.insert x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.insert x Set.empty by Set.singleton x" <|
            \() ->
                """module A exposing (..)
a = Set.insert x Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Set.singleton instead of inserting in Set.empty"
                            , details = [ "You can replace this call by Set.singleton." ]
                            , under = "Set.insert"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton x
"""
                        ]
        , test "should replace Set.empty |> Set.insert x by Set.singleton x" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.insert x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Set.singleton instead of inserting in Set.empty"
                            , details = [ "You can replace this call by Set.singleton." ]
                            , under = "Set.insert"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.singleton x
"""
                        ]
        ]



-- Dict


dictSimplificationTests : Test
dictSimplificationTests =
    describe "Dict"
        [ dictIsEmptyTests
        , dictFromListTests
        , dictToListTests
        , dictSizeTests
        , dictMemberTests
        , dictPartitionTests
        ]


dictIsEmptyTests : Test
dictIsEmptyTests =
    describe "Dict.isEmpty"
        [ test "should not report Dict.isEmpty with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Dict.isEmpty
b = Dict.isEmpty list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.isEmpty Dict.empty by True" <|
            \() ->
                """module A exposing (..)
a = Dict.isEmpty Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Dict.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.fromList [x]) by False" <|
            \() ->
                """module A exposing (..)
a = Dict.isEmpty (Dict.fromList [x])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Dict.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.fromList []) by False" <|
            \() ->
                """module A exposing (..)
a = Dict.isEmpty (Dict.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Dict.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        , Review.Test.error
                            { message = "The call to Dict.fromList will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Dict.isEmpty (Dict.empty)
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.singleton x) by False" <|
            \() ->
                """module A exposing (..)
a = Dict.isEmpty (Dict.singleton x y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Dict.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace x :: xs |> Dict.isEmpty by False" <|
            \() ->
                """module A exposing (..)
a = Dict.singleton x y |> Dict.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Dict.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


dictFromListTests : Test
dictFromListTests =
    describe "Dict.fromList"
        [ test "should not report Dict.fromList with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Dict.fromList
b = Dict.fromList list
b = Dict.fromList [x]
b = Dict.fromList [x, y]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.fromList [] by Dict.empty" <|
            \() ->
                """module A exposing (..)
a = Dict.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Dict.fromList will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Dict.empty
"""
                        ]
        ]


dictToListTests : Test
dictToListTests =
    describe "Dict.toList"
        [ test "should not report Dict.toList with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Dict.toList
b = Dict.toList list
c = Dict.toList set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.toList Dict.empty by []" <|
            \() ->
                """module A exposing (..)
a = Dict.toList Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to Dict.toList will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "Dict.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


dictSizeTests : Test
dictSizeTests =
    describe "Dict.size"
        [ test "should not report Dict.size used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Dict.size
a = Dict.size b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.size Dict.empty by 0" <|
            \() ->
                """module A exposing (..)
a = Dict.size Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the Dict is 0"
                            , details = [ "The size of the Dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [b, c, d]) by 3" <|
            \() ->
                """module A exposing (..)
a = Dict.size (Dict.fromList [b, c, d])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the Dict is 3"
                            , details = [ "The size of the Dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace Dict.empty |> Dict.size by 0" <|
            \() ->
                """module A exposing (..)
a = Dict.empty |> Dict.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the Dict is 0"
                            , details = [ "The size of the Dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace Dict.singleton x y |> Dict.size by 1" <|
            \() ->
                """module A exposing (..)
a = Dict.singleton x y |> Dict.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the Dict is 1"
                            , details = [ "The size of the Dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        ]


dictMemberTests : Test
dictMemberTests =
    describe "Dict.member"
        [ test "should not report Dict.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Dict.member x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.member x Dict.empty by False" <|
            \() ->
                """module A exposing (..)
a = Dict.member x Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.member on Dict.empty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


dictPartitionTests : Test
dictPartitionTests =
    describe "Dict.partition"
        [ test "should not report Dict.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Dict.partition f
a = Dict.partition f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.partition f Dict.empty by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
a = Dict.partition f Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition f <| Dict.empty by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
a = Dict.partition f <| Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.empty |> Dict.partition f by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
a = Dict.empty |> Dict.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (always True) x by ( x, Dict.empty )" <|
            \() ->
                """module A exposing (..)
a = Dict.partition (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first Dict"
                            , details = [ "Since the predicate function always returns True, the second Dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( x, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (\\_ -> True) x by ( x, Dict.empty )" <|
            \() ->
                """module A exposing (..)
a = Dict.partition (\\_ -> True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first Dict"
                            , details = [ "Since the predicate function always returns True, the second Dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( x, Dict.empty )
"""
                        ]
        , test "should not replace Dict.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
a = Dict.partition (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.partition (always False) x by ( Dict.empty, x )" <|
            \() ->
                """module A exposing (..)
a = Dict.partition (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second Dict"
                            , details = [ "Since the predicate function always returns False, the first Dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( Dict.empty, x )
"""
                        ]
        , test "should replace Dict.partition (always False) by (Tuple.pair Dict.empty)" <|
            \() ->
                """module A exposing (..)
a = Dict.partition (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second Dict"
                            , details = [ "Since the predicate function always returns False, the first Dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair Dict.empty)
"""
                        ]
        , test "should replace Dict.partition <| (always False) by (Tuple.pair Dict.empty)" <|
            \() ->
                """module A exposing (..)
a = Dict.partition <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second Dict"
                            , details = [ "Since the predicate function always returns False, the first Dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair Dict.empty)
"""
                        ]
        , test "should replace always False |> Dict.partition by Tuple.pair Dict.empty" <|
            \() ->
                """module A exposing (..)
a = always False |> Dict.partition
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second Dict"
                            , details = [ "Since the predicate function always returns False, the first Dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair Dict.empty)
"""
                        ]
        ]



-- Cmd


cmdTests : Test
cmdTests =
    describe "Cmd.batch"
        [ test "should not report Cmd.batch used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch
a = Cmd.batch b
a = Cmd.batch [ b, x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Cmd.batch [] by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace by Cmd.batch"
                            , details = [ "Cmd.batch [] and Cmd.none are equivalent but the latter is more idiomatic in Elm code" ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        , test "should replace Cmd.batch [ a, Cmd.none, b ] by Cmd.batch [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ a, Cmd.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.none"
                            , details = [ "Cmd.none will be ignored by Cmd.batch." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ a, b ]
"""
                        ]
        , test "should replace Cmd.batch [ Cmd.none ] by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ Cmd.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.batch"
                            , details = [ "Cmd.batch with a single element is equal to that element." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        , test "should replace Cmd.batch [ b ] by b" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.batch"
                            , details = [ "Cmd.batch with a single element is equal to that element." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should replace Cmd.batch [ b, Cmd.none ] by Cmd.batch []" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ b, Cmd.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.none"
                            , details = [ "Cmd.none will be ignored by Cmd.batch." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ b ]
"""
                        ]
        , test "should replace Cmd.batch [ Cmd.none, b ] by Cmd.batch [ b ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ Cmd.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.none"
                            , details = [ "Cmd.none will be ignored by Cmd.batch." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ b ]
"""
                        ]
        , test "should replace Cmd.map identity cmd by cmd" <|
            \() ->
                """module A exposing (..)
a = Cmd.map identity cmd
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Cmd.map with an identity function is the same as not using Cmd.map"
                            , details = [ "You can remove this call and replace it by the command itself." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = cmd
"""
                        ]
        , test "should replace Cmd.map f Cmd.none by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.map f Cmd.none
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Cmd.map on Cmd.none will result in Cmd.none"
                            , details = [ "You can replace this call by Cmd.none." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        ]



-- Sub


subTests : Test
subTests =
    describe "Sub.batch"
        [ test "should not report Sub.batch used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Sub.batch
a = Sub.batch b
a = Sub.batch [ b, x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Sub.batch [] by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.batch []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace by Sub.batch"
                            , details = [ "Sub.batch [] and Sub.none are equivalent but the latter is more idiomatic in Elm code" ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        , test "should replace Sub.batch [ a, Sub.none, b ] by Sub.batch [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ a, Sub.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.none"
                            , details = [ "Sub.none will be ignored by Sub.batch." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ a, b ]
"""
                        ]
        , test "should replace Sub.batch [ Sub.none ] by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ Sub.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.batch"
                            , details = [ "Sub.batch with a single element is equal to that element." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        , test "should replace Sub.batch [ b ] by b" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.batch"
                            , details = [ "Sub.batch with a single element is equal to that element." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should replace Sub.batch [ f n ] by (f n)" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ f n ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.batch"
                            , details = [ "Sub.batch with a single element is equal to that element." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        , test "should replace Sub.batch [ b, Sub.none ] by Sub.batch []" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ b, Sub.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.none"
                            , details = [ "Sub.none will be ignored by Sub.batch." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ b ]
"""
                        ]
        , test "should replace Sub.batch [ Sub.none, b ] by Sub.batch [ b ]" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ Sub.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.none"
                            , details = [ "Sub.none will be ignored by Sub.batch." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ b ]
"""
                        ]
        , test "should replace Sub.map identity sub by sub" <|
            \() ->
                """module A exposing (..)
a = Sub.map identity sub
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Sub.map with an identity function is the same as not using Sub.map"
                            , details = [ "You can remove this call and replace it by the subscription itself." ]
                            , under = "Sub.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = sub
"""
                        ]
        , test "should replace Sub.map f Sub.none by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.map f Sub.none
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Sub.map on Sub.none will result in Sub.none"
                            , details = [ "You can replace this call by Sub.none." ]
                            , under = "Sub.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        ]



-- Parser


parserTests : Test
parserTests =
    describe "Parser.oneOf"
        [ test "should not report Parser.oneOf used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Parser
import Parser.Advanced
a = Parser.oneOf x
b = Parser.oneOf [ y, z ]
c = Parser.Advanced.oneOf x
d = Parser.Advanced.oneOf [ y, z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Parser.oneOf [ x ] by x" <|
            \() ->
                """module A exposing (..)
import Parser
a = Parser.oneOf [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary oneOf"
                            , details = [ "There is only a single element in the list of elements to try out." ]
                            , under = "Parser.oneOf"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Parser
a = x
"""
                        ]
        , test "should replace Parser.Advanced.oneOf [ x ] by x" <|
            \() ->
                """module A exposing (..)
import Parser.Advanced
a = Parser.Advanced.oneOf [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary oneOf"
                            , details = [ "There is only a single element in the list of elements to try out." ]
                            , under = "Parser.Advanced.oneOf"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Parser.Advanced
a = x
"""
                        ]
        ]



-- Json.Decode


jsonDecodeTests : Test
jsonDecodeTests =
    describe "Json.Decode.oneOf"
        [ test "should not report Json.Decode.oneOf used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.oneOf x
b = Json.Decode.oneOf [ y, z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Decode.oneOf [ x ] by x" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.oneOf [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary oneOf"
                            , details = [ "There is only a single element in the list of elements to try out." ]
                            , under = "Json.Decode.oneOf"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = x
"""
                        ]
        ]



-- Html.Attributes


htmlAttributesTests : Test
htmlAttributesTests =
    describe "Html.Attributes"
        [ htmlAttributesClassListTests ]


htmlAttributesClassListTests : Test
htmlAttributesClassListTests =
    describe "Html.Attributes.classList"
        [ test "should not report Html.Attributes.classList used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList
b = Html.Attributes.classList x
c = Html.Attributes.classList [ y, z ]
c = Html.Attributes.classList [ y, ( z, True ) ]
d = Html.Attributes.classList (x :: y :: z)
d = Html.Attributes.classList (( y, True ) :: z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Html.Attributes.classList [ ( x, True ) ] by Html.Attributes.class x" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ ( x, True ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class x
"""
                        ]
        , test "should replace Html.Attributes.classList [ ( f x, True ) ] by Html.Attributes.class (f x)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ ( f x, True ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class (f x)
"""
                        ]
        , test "should replace Html.Attributes.classList (List.singleton ( x, True )) by Html.Attributes.class x" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (List.singleton ( x, True ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class x
"""
                        ]
        , test "should replace Html.Attributes.classList (List.singleton ( f x, True )) by Html.Attributes.class (f x)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (List.singleton ( f x, True ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                            , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.class (f x)
"""
                        ]
        , test "should replace Html.Attributes.classList (List.singleton ( x, False )) by Html.Attributes.classList []" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (List.singleton ( x, False ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList []
"""
                        ]
        , test "should replace Html.Attributes.classList [ x, ( y, False ), z ] by Html.Attributes.classList [ x, y ]" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x, ( y, False ), z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x, z ]
"""
                        ]
        , test "should replace Html.Attributes.classList [ x, ( y, False ) ] by Html.Attributes.classList [ x ]" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x, ( y, False ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ x ]
"""
                        ]
        , test "should replace Html.Attributes.classList [ ( x, False ), y ] by Html.Attributes.classList [ y ]" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ ( x, False ), y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [ y ]
"""
                        ]
        , test "should replace Html.Attributes.classList [( x, False )] by Html.Attributes.classList []" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList [( x, False )]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList []
"""
                        ]
        , test "should replace Html.Attributes.classList (( x, False ) :: tail) by Html.Attributes.classList (tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (( x, False ) :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (( x, False ) :: f tail) by Html.Attributes.classList (f tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (( x, False ) :: f tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (f tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (x :: ( y, False ) :: tail) by Html.Attributes.classList (x :: tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: ( y, False ) :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (x :: ( y, False ) :: z :: tail) by Html.Attributes.classList (x :: tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: ( y, False ) :: z :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (x :: z :: tail)
"""
                        ]
        , test "should replace Html.Attributes.classList (( x, False ) :: y :: tail) by Html.Attributes.classList (x :: tail)" <|
            \() ->
                """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (( x, False ) :: y :: tail)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
                            , details = [ "You can remove the tuple list element where the second part is False." ]
                            , under = "Html.Attributes.classList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Html.Attributes
a = Html.Attributes.classList (y :: tail)
"""
                        ]
        ]



-- Record access


recordAccessTests : Test
recordAccessTests =
    let
        details : List String
        details =
            [ "Accessing the field of a record or record update can be simplified to just that field's value"
            ]
    in
    describe "Simplify.RecordAccess"
        [ test "should simplify record accesses for explicit records" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = details
                            , under = "{ b = 3 }.b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should simplify record accesses for explicit records and add parens when necessary" <|
            \() ->
                """module A exposing (..)
a = { b = f n }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = details
                            , under = "{ b = f n }.b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        , test "should simplify record accesses for explicit records in parentheses" <|
            \() ->
                """module A exposing (..)
a = (({ b = 3 })).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = details
                            , under = "(({ b = 3 })).b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "shouldn't simplify record accesses for explicit records if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify record accesses for record updates" <|
            \() ->
                """module A exposing (..)
a = foo { d | b = f x y }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = details
                            , under = "{ d | b = f x y }.b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates in parentheses" <|
            \() ->
                """module A exposing (..)
a = foo (({ d | b = f x y })).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = details
                            , under = "(({ d | b = f x y })).b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { d | b = 3 }.c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of an unrelated record update can be simplified to just the original field's value" ]
                            , under = "{ d | b = 3 }.c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = d.c
"""
                        ]
        , test "should simplify record accesses for let/in expressions" <|
            \() ->
                """module A exposing (..)
a = (let b = c in { e = 3 }).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in { e = 3 }.e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression" <|
            \() ->
                """module A exposing (..)
a = (let b = c in f x).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression, without adding unnecessary parentheses" <|
            \() ->
                """module A exposing (..)
a = (let b = c in x).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in x.e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions in parentheses" <|
            \() ->
                """module A exposing (..)
a = (((let b = c in {e = 2}))).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (((let b = c in {e = 2}.e)))
"""
                        ]
        , test "should simplify nested record accesses for let/in expressions (inner)" <|
            \() ->
                """module A exposing (..)
a = (let b = c in { e = { f = 2 } }).e.f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in { e = { f = 2 } }.e).f
"""
                        ]
        , test "should simplify nested record accesses for let/in expressions (outer)" <|
            \() ->
                """module A exposing (..)
a = (let b = c in (f x).e).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e.f)
"""
                        ]
        , test "should simplify record accesses for if/then/else expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else { z | f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside an if/then/else expression can be simplified to access the field inside it" ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should not simplify record accesses if some branches are not records" <|
            \() ->
                """module A exposing (..)
a = (if x then a else { f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify record accesses for nested if/then/else expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else if y then { z | f = 4 } else { z | f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside an if/then/else expression can be simplified to access the field inside it" ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else if y then { z | f = 4 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should simplify record accesses for mixed if/then/else and case expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else if y then {f = 2} else
            case b of Nothing -> { f = 4 }
                      Just _ -> { f = 5 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside an if/then/else expression can be simplified to access the field inside it" ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else if y then {f = 2}.f else
            case b of Nothing -> { f = 4 }.f
                      Just _ -> { f = 5 }.f)
"""
                        ]
        ]


letTests : Test
letTests =
    describe "Let declarations"
        [ test "should merge two adjacent let declarations" <|
            \() ->
                """module A exposing (..)
a =
    let
        b =
            1
    in
    let
        c =
            1
    in
    b + c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Let blocks can be joined together"
                            , details = [ "Let blocks can contain multiple declarations, and there is no advantage to having multiple chained let expressions rather than one longer let expression." ]
                            , under = "let"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let
        b =
            1

        c =
            1
    in
    b + c
"""
                        ]
        , test "should merge two adjacent let declarations with multiple declarations" <|
            \() ->
                """module A exposing (..)
a =
    let
        b =
            1

        c =
            1
    in
    let
        d =
            1

        e =
            1
    in
    b + c + d + e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Let blocks can be joined together"
                            , details = [ "Let blocks can contain multiple declarations, and there is no advantage to having multiple chained let expressions rather than one longer let expression." ]
                            , under = "let"
                            }
                            |> Review.Test.atExactly { start = { row = 10, column = 5 }, end = { row = 10, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let
        b =
            1

        c =
            1

        d =
            1

        e =
            1
    in
    b + c + d + e
"""
                        ]
        ]
