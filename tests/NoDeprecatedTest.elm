module NoDeprecatedTest exposing (all)

import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode as Decode
import NoDeprecated exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoDeprecated"
        [ valueTests
        , typeTests
        , parametersTests
        , letTests
        , caseExpressionTests
        , fieldsTests
        , portsTests
        , fromOtherModulesTests
        , dependencyElementsTests
        , dependencyTests
        ]


valueTests : Test
valueTests =
    describe "Values"
        [ test "should not report an error when using a non-deprecated element" <|
            \() ->
                [ """module A exposing (..)
import Other
a = Other.normalFunction
""", """module Other exposing (..)
normalFunction = 1
""" ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectNoErrors
        , test "should report an error when referencing a local function whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
somethingDeprecated = 1

a = somethingDeprecated
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "somethingDeprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 24 } }
                        ]
        , test "should report an error when referencing a local function whose documentation contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
a = something

{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
something = 1
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "something"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (qualified import)" <|
            \() ->
                [ """module A exposing (..)
import Some.DeprecatedModule
a = Some.DeprecatedModule.something
""", moduleWithDeprecatedInItsName ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "Some.DeprecatedModule.something"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (unqualifed import)" <|
            \() ->
                [ """module A exposing (..)
import Some.DeprecatedModule as S
a = S.something
""", moduleWithDeprecatedInItsName ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "S.something"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (record update)" <|
            \() ->
                [ """module A exposing (..)
import Some.DeprecatedModule exposing (something)
a = { something | b = 1 }
""", moduleWithDeprecatedInItsName ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "something"
                                }
                                |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                            ]
                          )
                        ]
        , test "should not report an error when referencing a function whose name contains deprecated but is marked as an exception (local reference)" <|
            \() ->
                """module A exposing (..)
type Status = Deprecated | NotDeprecated
a = [ Deprecated, NotDeprecated ]
"""
                    |> Review.Test.run
                        (NoDeprecated.defaults
                            |> NoDeprecated.withExceptionsForElements [ "A.Deprecated", "A.NotDeprecated" ]
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "should not report an error when referencing a function whose name contains deprecated but is marked as an exception (other module reference)" <|
            \() ->
                [ """module A exposing (..)
import Status
a = [ Status.Deprecated, Status.NotDeprecated ]
""", """module Status exposing (Status)
type Status = Deprecated | NotDeprecated
""" ]
                    |> Review.Test.runOnModules
                        (NoDeprecated.defaults
                            |> NoDeprecated.withExceptionsForElements [ "Status.Deprecated", "Status.NotDeprecated" ]
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "should report a configuration error when giving an invalid exception " <|
            \() ->
                NoDeprecated.defaults
                    |> NoDeprecated.withExceptionsForElements [ "malformedStuff" ]
                    |> rule
                    |> Review.Test.expectConfigurationError
                        { message = "Invalid exceptions provided in the configuration"
                        , details =
                            [ "The names provided to the withExceptionsForElements function should look like 'Some.Module.value' or 'MyModule.Type', which wasn't the case for the following types:"
                            , " - malformedStuff"
                            ]
                        }
        ]


moduleWithDeprecatedInItsName : String
moduleWithDeprecatedInItsName =
    """module Some.DeprecatedModule exposing (..)
a = 1
"""


typeTests : Test
typeTests =
    describe "Types"
        [ test "should report an error when referencing a custom type constructor whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated
a = Deprecated
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type alias constructor whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
type alias Deprecated = {}
a = Deprecated
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should not report an error when referencing a non-deprecated type alias or type alias constructor" <|
            \() ->
                """module A exposing (..)
type alias TypeAlias = {}
a : TypeAlias
a = TypeAlias
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectNoErrors
        , test "should report an error when referencing a type whose name contains 'deprecated' (top-level declaration annotation)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Int
a : Deprecated
a = 1
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a (Deprecated value) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 4 }, end = { row = 3, column = 14 } }
                        ]
        , test "should report an error when referencing a custom type whose documentation contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a : Something
a = 1
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
type Something = Foo Int
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Something"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                        ]
        , test "should report an error when referencing a custom type constructor whose documentation contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a (A value) = 1
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
type Something = A Int
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "A"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 4 }, end = { row = 2, column = 5 } }
                        ]
        , test "should report an error when referencing a type alias whose documentation contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a : Something
a = 1
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
type alias Something = Int
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Something"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                        ]
        , test "should report an error when referencing a type alias constructor whose documentation contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a = Something 1
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
type alias Something = { b : Int }
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Something"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (custom type declaration)" <|
            \() ->
                """module A exposing (..)
type alias Deprecated = String
type A = Thing ( A, { b : Deprecated } )
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 27 }, end = { row = 3, column = 37 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (type alias declaration)" <|
            \() ->
                """module A exposing (..)
type alias Deprecated = String
type alias A = Thing { b : Deprecated }
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 28 }, end = { row = 3, column = 38 } }
                        ]
        ]


parametersTests : Test
parametersTests =
    describe "Parameters"
        [ test "should report an error when having a parameter whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a thingDeprecated = 1
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                ]
                            , under = "thingDeprecated"
                            }
                        ]
        , test "should report an error when destructuring a field whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a ({deprecated}) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "deprecated"
                            }
                        ]
        , test "should report an error when using a parameter alias whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a (( x, y ) as deprecated) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                ]
                            , under = "deprecated"
                            }
                        ]
        ]


letTests : Test
letTests =
    describe "Let expressions"
        [ test "should report an error when referencing a type whose name contains 'deprecated' (let declaration annotation)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Int
a =
    let
        b : Deprecated
        b = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 13 }, end = { row = 5, column = 23 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let declaration)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a =
    let
        b (Deprecated value) = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 12 }, end = { row = 5, column = 22 } }
                        ]
        , test "should report an error when having a parameter whose name contains 'deprecated' (let declaration)" <|
            \() ->
                """module A exposing (..)
a =
    let
        b thingDeprecated = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                ]
                            , under = "thingDeprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let destructuring)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a =
    let
        (Deprecated b) = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 10 }, end = { row = 5, column = 20 } }
                        ]
        ]


caseExpressionTests : Test
caseExpressionTests =
    describe "Case expressions"
        [ test "should report an error when referencing a type whose name contains 'deprecated' (case expression)" <|
            \() ->
                """module A exposing (..)
type A = ThingDeprecated Int | Thing
a =
    case x of
        ThingDeprecated b -> 1
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ThingDeprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 9 }, end = { row = 5, column = 24 } }
                        ]
        ]


fieldsTests : Test
fieldsTests =
    describe "Fields"
        [ test "should report an error when referencing a field whose name contains 'deprecated' (record access)" <|
            \() ->
                """module A exposing (..)
a = some.thingDeprecated
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "thingDeprecated"
                            }
                        ]
        , test "should report an error when referencing a field whose name contains 'deprecated' (record access function)" <|
            \() ->
                """module A exposing (..)
a = .thingDeprecated
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = ".thingDeprecated"
                            }
                        ]
        ]


portsTests : Test
portsTests =
    describe "Ports"
        [ test "should report an error when referencing a type whose name contains 'deprecated' (Sub port)" <|
            \() ->
                """module A exposing (..)
type alias DeprecatedString = String
port input : (DeprecatedString -> msg) -> Sub msg
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "DeprecatedString"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 15 }, end = { row = 3, column = 31 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (Cmd port)" <|
            \() ->
                """module A exposing (..)
type alias DeprecatedString = String
port output : DeprecatedString -> Cmd msg
"""
                    |> Review.Test.run (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "DeprecatedString"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 15 }, end = { row = 3, column = 31 } }
                        ]
        ]


fromOtherModulesTests : Test
fromOtherModulesTests =
    describe "From other modules"
        [ test "should report an error when referencing a value from a deprecated module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a = OtherModule.something
""", deprecatedModule ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.something"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a custom type from a deprecated module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a : OtherModule.CustomType
a = 1
""", deprecatedModule ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.CustomType"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a custom type constructor from a deprecated module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a = OtherModule.Constructor
""", deprecatedModule ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.Constructor"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a type alias from a deprecated module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a : OtherModule.Alias
a = 1
""", modulesWithDeprecatedThings ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "This element was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.Alias"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a deprecated value from a different module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a = OtherModule.value
""", modulesWithDeprecatedThings ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "This element was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.value"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a deprecated custom type from a different module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a : OtherModule.CustomType
a = 1
""", modulesWithDeprecatedThings ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "This element was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.CustomType"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a constructor of a deprecated custom type from a different module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a = OtherModule.Constructor
""", modulesWithDeprecatedThings ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "This element was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.Constructor"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a deprecated type alias from a different module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a : OtherModule.Alias
a = 1
""", modulesWithDeprecatedThings ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "This element was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.Alias"
                                }
                            ]
                          )
                        ]
        , test "should report an error when referencing a constructor of a deprecated record alias from a different module" <|
            \() ->
                [ """module A exposing (..)
import OtherModule
a = OtherModule.RecordAlias
""", modulesWithDeprecatedThings ]
                    |> Review.Test.runOnModules (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Found new usage of deprecated element"
                                , details =
                                    [ "This element was marked as deprecated and should not be used anymore."
                                    , "Please check its documentation to know the alternative solutions."
                                    ]
                                , under = "OtherModule.RecordAlias"
                                }
                            ]
                          )
                        ]
        ]


dependencyElementsTests : Test
dependencyElementsTests =
    describe "Elements from dependencies"
        [ test "should report an error when referencing a value from a deprecated dependency module" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_1
a = ModuleFromDependency_1.something
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_1.something"
                            }
                        ]
        , test "should report an error when referencing a custom type from a deprecated dependency module" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_1
a : ModuleFromDependency_1.CustomType
a = 1
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_1.CustomType"
                            }
                        ]
        , test "should report an error when referencing a custom type constructor from a deprecated dependency module" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_1
a = ModuleFromDependency_1.Constructor
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_1.Constructor"
                            }
                        ]
        , test "should report an error when referencing a type alias from a deprecated dependency module" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_1
a : ModuleFromDependency_1.Alias
a = 1
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_1.Alias"
                            }
                        ]
        , test "should report an error when referencing a deprecated value from a dependency" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_2
a = ModuleFromDependency_2.value
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_2.value"
                            }
                        ]
        , test "should report an error when referencing a deprecated custom type from a dependency" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_2
a : ModuleFromDependency_2.CustomType
a = 1
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_2.CustomType"
                            }
                        ]
        , test "should report an error when referencing a constructor of a deprecated custom type from a dependency" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_2
a = ModuleFromDependency_2.Constructor
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_2.Constructor"
                            }
                        ]
        , test "should report an error when referencing a deprecated type alias from a dependency" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_2
a : ModuleFromDependency_2.Alias
a = 1
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_2.Alias"
                            }
                        ]
        , test "should report an error when referencing a constructor of a deprecated record alias from a dependency" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_2
a = ModuleFromDependency_2.RecordAlias
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "This element was marked as deprecated and should not be used anymore."
                                , "Please check its documentation to know the alternative solutions."
                                ]
                            , under = "ModuleFromDependency_2.RecordAlias"
                            }
                        ]
        ]


dependencyTests : Test
dependencyTests =
    describe "Deprecating dependencies"
        [ test "should report an error when referencing a value from a deprecated dependency module" <|
            \() ->
                """module A exposing (..)
import OkModule
a = OkModule.something
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations
                        (NoDeprecated.defaults
                            |> NoDeprecated.dependencies [ "author/package" ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details =
                                [ "The dependency where this element is defined was marked as deprecated and should not be used anymore."
                                , "Please check its documentation or your review configuration to know the alternative solutions."
                                ]
                            , under = "OkModule.something"
                            }
                        ]
        , test "should report a global error when deprecating a package not in the dependencies" <|
            \() ->
                """module A exposing (..)
import OkModule
a = OkModule.something
"""
                    |> Review.Test.run
                        (NoDeprecated.defaults
                            |> NoDeprecated.dependencies [ "author/package" ]
                            |> rule
                        )
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find package `author/package`"
                          , details =
                                [ "You marked this package as deprecated, but I can't find it in your dependencies."
                                , "It could be a typo, or maybe you've successfully removed it from your project?"
                                ]
                          }
                        ]
        ]


deprecatedModule : String
deprecatedModule =
    """module OtherModule exposing (..)
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
import Basics
a = 1
"""


modulesWithDeprecatedThings : String
modulesWithDeprecatedThings =
    """module OtherModule exposing (..)
import Basics
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
type CustomType = Constructor
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
type alias Alias = Int
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
type alias RecordAlias = {}
{-| Does X.

@deprecated This is deprecated, use Y instead.
-}
value = 1
"""


projectWithDeprecations : Project
projectWithDeprecations =
    Project.addDependency dependency Project.new


dependency : Dependency
dependency =
    Dependency.create
        "author/package"
        (createElmJsonProject dependencyElmJson)
        dependencyModules


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

        Err error ->
            Debug.todo ("[elm.json]: " ++ Debug.toString error)


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "ModuleFromDependency_1"
      , comment = """{-| Does X.

@deprecated This is deprecated, use Y instead. -}
"""
      , unions =
            [ { name = "CustomType"
              , comment = ""
              , args = []
              , tags = [ ( "Constructor", [] ) ]
              }
            ]
      , aliases =
            [ { name = "Alias"
              , comment = ""
              , args = []
              , tipe = Elm.Type.Tuple []
              }
            ]
      , values =
            [ { name = "value"
              , comment = ""
              , tipe = Elm.Type.Tuple []
              }
            ]
      , binops = []
      }
    , { name = "ModuleFromDependency_2"
      , comment = ""
      , unions =
            [ { name = "CustomType"
              , comment = """{-| Does X.

@deprecated This is deprecated, use Y instead. -}
"""
              , args = []
              , tags = [ ( "Constructor", [] ) ]
              }
            ]
      , aliases =
            [ { name = "Alias"
              , comment = """{-| Does X.

@deprecated This is deprecated, use Y instead. -}
"""
              , args = []
              , tipe = Elm.Type.Tuple []
              }
            , { name = "RecordAlias"
              , comment = """{-| Does X.

@deprecated This is deprecated, use Y instead. -}
"""
              , args = []
              , tipe = Elm.Type.Record [] Nothing
              }
            ]
      , values =
            [ { name = "value"
              , comment = """{-| Does X.

@deprecated This is deprecated, use Y instead. -}
"""
              , tipe = Elm.Type.Tuple []
              }
            ]
      , binops = []
      }
    , { name = "OkModule"
      , comment = ""
      , unions = []
      , aliases = []
      , values =
            [ { name = "value"
              , comment = ""
              , tipe = Elm.Type.Tuple []
              }
            ]
      , binops = []
      }
    ]


dependencyElmJson : String
dependencyElmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Moods for tests",
    "license": "MIT",
    "version": "1.0.0",
    "exposed-modules": [
        "ModuleFromDependency_1",
        "ModuleFromDependency_2"
    ],
    "elm-version": "0.19.0 <= v <= 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}"""
