module NoImportingEverythingTest exposing (all)

import Dependencies.ElmCore
import NoImportingEverything exposing (rule)
import Review.Project as Project
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "Prefer listing what you wish to import and/or using qualified imports"


details : List String
details =
    [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]


all : Test
all =
    describe "NoImportingEverything"
        [ withoutModuleInformationTests
        , withModuleInformationTests
        ]


withoutModuleInformationTests : Test
withoutModuleInformationTests =
    describe "Without module information"
        [ test "should not report imports without exposing clause" <|
            \_ ->
                """module A exposing (thing)
import Html
import Html as B
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report imports that expose some elements" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (B, c)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report imports that expose all constructors of a type" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (B(..))
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report imports that expose everything" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (..)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "(..)"
                            }
                        ]
        , test "should not report imports that are in the exceptions list" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (..)
import Thing.Foo as Foo exposing (..)
"""
                    |> Review.Test.run (rule [ "Html", "Thing.Foo" ])
                    |> Review.Test.expectNoErrors
        ]


withModuleInformationTests : Test
withModuleInformationTests =
    describe "With module information"
        [ test "should fix imports that expose everything" <|
            \_ ->
                [ """module A exposing (thing)
import OtherModule exposing (..)
thing = a
""", """module OtherModule exposing (..)
a = 1
""" ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = message
                                , details = details
                                , under = "(..)"
                                }
                                |> Review.Test.whenFixed """module A exposing (thing)
import OtherModule exposing (a)
thing = a
"""
                            ]
                          )
                        ]
        , test "should fix imports that from dependency module" <|
            \_ ->
                """module A exposing (thing)
import Tuple exposing (..)
thing = pair
"""
                    |> Review.Test.runWithProjectData (Project.new |> Project.addDependency Dependencies.ElmCore.dependency) (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (thing)
import Tuple exposing (pair)
thing = pair
"""
                        ]
        , test "should only replace by imports used in an unqualified manner" <|
            \_ ->
                [ """module A exposing (thing)
import OtherModule exposing (..)
thing = OtherModule.c a
""", """module OtherModule exposing (..)
a = 1
c = 2
""" ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = message
                                , details = details
                                , under = "(..)"
                                }
                                |> Review.Test.whenFixed """module A exposing (thing)
import OtherModule exposing (a)
thing = OtherModule.c a
"""
                            ]
                          )
                        ]
        , test "should import a custom type's constructors when using one of the constructors" <|
            \_ ->
                [ """module A exposing (thing)
import OtherModule exposing (..)
thing = Variant
""", """module OtherModule exposing (..)
type Custom = Variant
""" ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = message
                                , details = details
                                , under = "(..)"
                                }
                                |> Review.Test.whenFixed """module A exposing (thing)
import OtherModule exposing (Custom(..))
thing = Variant
"""
                            ]
                          )
                        ]
        , test "should only import type name when using a custom type but not its constructors" <|
            \_ ->
                [ """module A exposing (thing)
import OtherModule exposing (..)
thing : Custom
thing = 1
""", """module OtherModule exposing (..)
type Custom = Variant
""" ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = message
                                , details = details
                                , under = "(..)"
                                }
                                |> Review.Test.whenFixed """module A exposing (thing)
import OtherModule exposing (Custom)
thing : Custom
thing = 1
"""
                            ]
                          )
                        ]
        , test "should have the replacements sorted in alphabetical order" <|
            \_ ->
                [ """module A exposing (thing)
import OtherModule exposing (..)
thing = c foo Thing a C
""", """module OtherModule exposing (..)
type alias C = {}
type alias Thing = {}
foo = {}
a = 1
c = 2
""" ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = message
                                , details = details
                                , under = "(..)"
                                }
                                |> Review.Test.whenFixed """module A exposing (thing)
import OtherModule exposing (C, Thing, a, c, foo)
thing = c foo Thing a C
"""
                            ]
                          )
                        ]
        , test "should not touch fix unused exposing (..) if nothing was used" <|
            \_ ->
                [ """module A exposing (thing)
import OtherModule exposing (..)
thing = c
""", """module OtherModule exposing (..)
a = 1
""" ]
                    |> Review.Test.runOnModules (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = message
                                , details = details
                                , under = "(..)"
                                }
                            ]
                          )
                        ]
        ]
