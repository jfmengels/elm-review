module NoUnused.ExportsTest exposing (all)

import NoUnused.Exports exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)
import TestProject exposing (application, lamderaApplication, package)


unusedExposedElementDetails : List String
unusedExposedElementDetails =
    [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
    ]


unusedModuleDetails : List String
unusedModuleDetails =
    [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
    ]


all : Test
all =
    describe "NoUnusedExports"
        [ functionsAndValuesTests
        , typesTests
        , typeAliasesTests
        , duplicateModuleNameTests
        , importsTests
        , lamderaTests
        , unusedModuleTests

        -- TODO Add tests that report exposing the type's variants if they are never used.
        ]


functionsAndValuesTests : Test
functionsAndValuesTests =
    describe "Functions and values"
        [ test "should report an exposed function when it is not used in other modules" <|
            \() ->
                [ """
module A exposing (a)
a = 1
""", """module Exposed exposing (..)
import A
a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed function or value `a` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "a"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 21 } }
                            ]
                          )
                        ]
        , test "should not report an exposed function when it is used in other modules (qualified import)" <|
            \() ->
                [ """
module A exposing (a)
a = 1
""", """
module B exposing (main)
import A
main = A.a
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed function when it is used in other modules (using an alias)" <|
            \() ->
                [ """
module A exposing (a)
a = 1
""", """
module B exposing (main)
import A as SomeA
main = SomeA.a
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed function when it is used in other modules (using `exposing` to import)" <|
            \() ->
                [ """
module A exposing (a)
a = 1
""", """
module B exposing (main)
import A exposing (a)
main = a
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed function when it is used in other modules (using `exposing(..)` to import)" <|
            \() ->
                [ """
module A exposing (a)
a = 1
""", """
module B exposing (main)
import A exposing (..)
main = a
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed value when it is used in other modules (using record update syntax, importing explicitly)" <|
            \() ->
                [ """
module Exposed exposing (..)
import B exposing (person)
otherPerson =
    { person | age = 30 }
""", """
module B exposing (person)
person =
    { age = 29 }
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed value when it is used in other modules (using record update syntax, importing all)" <|
            \() ->
                [ """module Exposed exposing (..)
import B exposing (..)
otherPerson =
    { person | age = 30 }
""", """module B exposing (person)
person =
    { age = 29 }
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should report an exposed function when it is not used in other modules, even if it is used in the module" <|
            \() ->
                [ """
module A exposing (exposed)
exposed = 1
main = exposed
""", """module Exposed exposing (..)
import A
a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed function or value `exposed` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "exposed"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 27 } }
                            ]
                          )
                        ]
        , test "should propose a fix for unused exports if there are others exposed elements" <|
            \() ->
                [ """
module A exposing (exposed1, exposed2)
exposed1 = 1
exposed2 = 2
""", """module Exposed exposing (..)
import A
a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed function or value `exposed1` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "exposed1"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 28 } }
                                |> Review.Test.whenFixed """
module A exposing (exposed2)
exposed1 = 1
exposed2 = 2
"""
                            , Review.Test.error
                                { message = "Exposed function or value `exposed2` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "exposed2"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 30 }, end = { row = 2, column = 38 } }
                                |> Review.Test.whenFixed """
module A exposing (exposed1)
exposed1 = 1
exposed2 = 2
"""
                            ]
                          )
                        ]
        , test "should propose to remove the @docs entry in the module's documentation along with the removed export" <|
            \() ->
                [ """module A exposing (exposed1, exposed2, exposed3)
{-|

@docs exposed1, exposed2
@docs exposed3
-}

exposed1 = 1
exposed2 = 2
exposed3 = 3
""", """module Exposed exposing (..)
import A
a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed function or value `exposed1` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "exposed1"
                                }
                                |> Review.Test.atExactly { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } }
                                |> Review.Test.whenFixed """module A exposing (exposed2, exposed3)
{-|

@docs exposed2
@docs exposed3
-}

exposed1 = 1
exposed2 = 2
exposed3 = 3
"""
                            , Review.Test.error
                                { message = "Exposed function or value `exposed2` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "exposed2"
                                }
                                |> Review.Test.atExactly { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } }
                                |> Review.Test.whenFixed """module A exposing (exposed1, exposed3)
{-|

@docs exposed1
@docs exposed3
-}

exposed1 = 1
exposed2 = 2
exposed3 = 3
"""
                            , Review.Test.error
                                { message = "Exposed function or value `exposed3` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "exposed3"
                                }
                                |> Review.Test.atExactly { start = { row = 1, column = 40 }, end = { row = 1, column = 48 } }
                                |> Review.Test.whenFixed """module A exposing (exposed1, exposed2)
{-|

@docs exposed1, exposed2
-}

exposed1 = 1
exposed2 = 2
exposed3 = 3
"""
                            ]
                          )
                        ]
        , test "should not report anything for modules that expose everything" <|
            \() ->
                [ """
module A exposing (..)
a = 1
""", """module Exposed exposing (..)
import A
a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report the `main` function for an application even if it is unused" <|
            \() ->
                """
module Main exposing (main)
main = text ""
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should report the `main` function for a package when it is never used outside the module" <|
            \() ->
                [ """module A exposing (main)
main = text ""
""", """module Exposed exposing (..)
import A
a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed function or value `main` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "main"
                                }
                                |> Review.Test.atExactly { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                            ]
                          )
                        ]
        , test "should not report a function that does not refer to anything" <|
            \() ->
                [ """
module A exposing (b)
a = 1
""", """module Exposed exposing (..)
import A
a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report exposed tests" <|
            \() ->
                """
module ThingTest exposing (a)
import Test exposing (Test)
a : Test
a = Test.describe "thing" []
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report exposed tests (with other declarations available)" <|
            \() ->
                """module A exposing (all)
import Test exposing (Test, describe, fuzz, test)

all : Test
all =
   test "NameVisitor" testFn

a = 1
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report ReviewConfig.config" <|
            \() ->
                """
module ReviewConfig exposing (config)
config = []
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report a function in a 'shadowed' module" <|
            \() ->
                [ """module Foo exposing (foo)
foo = 1
""", """module Bar exposing (bar)
bar = 2
""", """module Main exposing (main)
import Bar as Foo
import Foo
main = [ Foo.foo, Foo.bar ]
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        ]


typesTests : Test
typesTests =
    describe "Types"
        [ test "should report an unused exposed custom type" <|
            \() ->
                [ """
module A exposing (Exposed)
type Exposed = VariantA | VariantB
""", """module Main exposing (main)
import A
main = text ""
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `Exposed` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "Exposed"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 27 } }
                            ]
                          )
                        ]
        , test "should not report a used exposed custom type (type signature)" <|
            \() ->
                [ """
module A exposing (Exposed, variantA)
type Exposed = VariantA | VariantB
variantA = VariantA
""", """
module B exposing (main)
import A
main : A.Exposed
main = A.variantA
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a used exposed custom type (value usage)" <|
            \() ->
                [ """
module A exposing (Exposed(..))
type Exposed = VariantA | VariantB
""", """
module B exposing (main)
import A
main = A.VariantA
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a used exposed custom type (case expression usage)" <|
            \() ->
                [ """
module A exposing (main)
import Variant
import Html

main =
    case config of
        Variant.A -> Html.text "a"
""", """
module Variant exposing (Variant(..))

type Variant = A
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a used exposed custom type (argument destructuring usage)" <|
            \() ->
                [ """
module A exposing (main)
import Variant
import Html

main a =
    let
        fn (Variant.A str) = value
    in
    fn a
""", """
module Variant exposing (Variant(..))

type Variant = A String
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a used exposed custom type (function declaration destructuring)" <|
            \() ->
                [ """
module Main exposing (main)
import Variant
import Html

main (Variant.A str) = value
""", """
module Variant exposing (Variant(..))

type Variant = A String
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a used exposed custom type (let destructuring usage)" <|
            \() ->
                [ """
module A exposing (main)
import Variant
import Html

main =
    let (Variant.A str) = value
    in str
""", """
module Variant exposing (Variant(..))

type Variant = A String
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a used exposed custom type (used in type alias)" <|
            \() ->
                [ """
module A exposing (ExposedB, ExposedC)
type ExposedB = B
type ExposedC = C
""", """
module Exposed exposing (B, C)
import A
type alias B = A.ExposedB
type alias C = A.ExposedC
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's part of the package's exposed API" <|
            \() ->
                """
module Exposed exposing (MyType)
type MyType = VariantA | VariantB
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's present in the signature of an exposed function" <|
            \() ->
                """
module A exposing (main, MyType)
type MyType = VariantA | VariantB
main : () -> MyType
main = 1
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's aliased by an exposed type alias" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type MyType = VariantA | VariantB
type alias OtherType = MyType
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's present in an exposed type alias" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type MyType = VariantA | VariantB
type alias OtherType = { thing : MyType }
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's present in an exposed type alias (nested)" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type MyType = VariantA | VariantB
type alias OtherType = { other : { thing : ((), MyType) } }
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's present in an exposed custom type constructor's arguments" <|
            \() ->
                [ """
module A exposing (MyType, OtherType(..))
type MyType = VariantA | VariantB
type OtherType = Thing MyType
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should report an unused exposed custom type if it's present in an exposed custom type constructor's arguments but the constructors are not exposed" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type MyType = VariantA | VariantB
type OtherType = Thing MyType
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "MyType"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 26 } }
                                |> Review.Test.whenFixed """
module A exposing (OtherType)
type MyType = VariantA | VariantB
type OtherType = Thing MyType
"""
                            ]
                          )
                        ]
        , test "should not report an unused exposed custom type if it's present in an exposed custom type constructor's arguments (nested)" <|
            \() ->
                [ """
module A exposing (MyType, OtherType(..))
type MyType = VariantA | VariantB
type OtherType = OtherThing | SomeThing ((), List MyType)
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's present in an exposed custom type constructor's arguments (nested) but the constructors are not exposed" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type MyType = VariantA | VariantB
type OtherType = OtherThing | SomeThing ((), List MyType)
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "MyType"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 26 } }
                                |> Review.Test.whenFixed """
module A exposing (OtherType)
type MyType = VariantA | VariantB
type OtherType = OtherThing | SomeThing ((), List MyType)
"""
                            ]
                          )
                        ]
        , test "should not report an exposed type if it is used in a let block type annotation" <|
            \() ->
                [ """module Main exposing (main)
import B
main =
  let
    type1 : B.Type1
    type1 =
      B.type1
  in
  type1
""", """module B exposing (Type1, type1)
type Type1 = Type1
type1 = Type1
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        ]


typeAliasesTests : Test
typeAliasesTests =
    describe "Type aliases"
        [ test "should report an unused exposed type alias" <|
            \() ->
                [ """
module A exposing (Exposed)
type alias Exposed = {}
""", """module Main exposing (main)
import A
main = text ""
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `Exposed` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "Exposed"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 27 } }
                            ]
                          )
                        ]
        , test "should not report a used exposed type alias (type signature)" <|
            \() ->
                [ """
module A exposing (Exposed)
type alias Exposed = {}
""", """
module B exposing (main)
import A
main : A.Exposed
main = {}
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed type alias if it is used in a let block type annotation" <|
            \() ->
                [ """module Main exposing (main)
import B
main =
  let
    type1 : B.Type1
    type1 =
      1
  in
  type1
""", """module B exposing (Type1)
type alias Type1 = Int
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed type if it is used as value in a let block" <|
            \() ->
                [ """module Main exposing (main)
import B
main =
  let type1 = B.Type1
  in type1
""", """module B exposing (Type1)
type alias Type1 = {}
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a used exposed type alias (used in type alias)" <|
            \() ->
                [ """
module A exposing (ExposedB)
type alias ExposedB = {}
""", """
module Exposed exposing (B)
import A
type alias B = A.ExposedB
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's part of the package's exposed API" <|
            \() ->
                """
module Exposed exposing (MyType)
type alias MyType = {}
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's present in the signature of an exposed function" <|
            \() ->
                """
module A exposing (main, MyType)
type alias MyType = {}
main : () -> MyType
main = 1
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's aliased by an exposed type alias" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type alias MyType = {}
type alias OtherType = MyType
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's present in an exposed type alias" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type alias MyType = {}
type alias OtherType = { thing : MyType }
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's present in an exposed type alias (nested)" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type alias MyType = {}
type alias OtherType = { other : { thing : ((), MyType) } }
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's present in an exposed custom type constructor's arguments" <|
            \() ->
                [ """
module A exposing (MyType, OtherType(..))
type alias MyType = {}
type OtherType = OtherType MyType
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's present in an exposed custom type constructor's arguments but the constructors are not exposed" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type alias MyType = {}
type OtherType = OtherType MyType
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "MyType"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 26 } }
                                |> Review.Test.whenFixed """
module A exposing (OtherType)
type alias MyType = {}
type OtherType = OtherType MyType
"""
                            ]
                          )
                        ]
        , test "should not report an unused exposed type alias if it's present in an exposed custom type constructor's arguments (nested)" <|
            \() ->
                [ """
module A exposing (MyType, OtherType(..))
type alias MyType = {}
type OtherType = OtherThing | SomeThing ((), List MyType)
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should report an unused exposed type alias if it's present in an exposed custom type constructor's arguments (nested) but the constructors are not exposed" <|
            \() ->
                [ """
module A exposing (MyType, OtherType)
type alias MyType = {}
type OtherType = OtherThing | SomeThing ((), List MyType)
""", """
module Exposed exposing (..)
import A
type alias B = A.OtherType
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "MyType"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 26 } }
                                |> Review.Test.whenFixed """
module A exposing (OtherType)
type alias MyType = {}
type OtherType = OtherThing | SomeThing ((), List MyType)
"""
                            ]
                          )
                        ]
        , test "should report the correct range when exports are on multiple lines" <|
            \() ->
                [ """module A
             exposing ( Card
    , Link
    , init
    , toElement
    )
type Card = Card
type Link = Link
init = 1
""", """
module Exposed exposing (..)
import A
a = A.Card A.init A.toElement
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `Link` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "Link"
                                }
                                |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                                |> Review.Test.whenFixed """module A
             exposing ( Card
    , init
    , toElement
    )
type Card = Card
type Link = Link
init = 1
"""
                            ]
                          )
                        ]
        , test "should not report an exposed type if it is used in a port (input)" <|
            \() ->
                [ """module Main exposing (main)
import B
main = somePort
port somePort : (B.Type1 -> msg) -> Sub msg
""", """module B exposing (Type1)
type alias Type1 = { user : String }
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed type if it is used in a port (output)" <|
            \() ->
                [ """module Main exposing (main)
import B
main = somePort
port somePort : B.Type1 -> Cmd msg
""", """module B exposing (Type1)
type alias Type1 = { user : String }
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        ]


duplicateModuleNameTests : Test
duplicateModuleNameTests =
    describe "Duplicate module names"
        [ test "should not report a used export even if it is imported through a module whose name appears twice" <|
            \() ->
                [ """module Main exposing (main)
import A as X
import B as X
main = X.a X.b
""", """module A exposing (a)
a = 1
""", """module B exposing (b)
b = 2
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        ]


importsTests : Test
importsTests =
    describe "Imports"
        [ test "should not report an export if it is imported by name" <|
            \() ->
                [ """module Main exposing (main)
import B exposing (Type1, Type2(..), TypeAlias, b)
main = 1
""", """module B exposing (Type1, Type2(..), TypeAlias, b)
type Type1 = Type1
type Type2 = Type2
type alias TypeAlias = {}
b = 2
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        ]


lamderaTests : Test
lamderaTests =
    describe "Lamdera support"
        [ test "should report an exposed `app` function in packages" <|
            \() ->
                [ """module NotExposed exposing (app)
app = foo
""", """module Exposed exposing (a)
import NotExposed
a = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData package rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "NotExposed"
                          , [ Review.Test.error
                                { message = "Exposed function or value `app` is never used outside this module."
                                , details = unusedExposedElementDetails
                                , under = "app"
                                }
                                |> Review.Test.atExactly { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } }
                            ]
                          )
                        ]
        , test "should report an exposed `app` function in applications" <|
            \() ->
                """module Main exposing (app)
main = text ""
app = foo
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed function or value `app` is never used outside this module."
                            , details = unusedExposedElementDetails
                            , under = "app"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } }
                        ]
        , test "should not report an exposed `app` function in Lamdera applications" <|
            \() ->
                """module Main exposing (app)
app = foo
"""
                    |> Review.Test.runWithProjectData lamderaApplication rule
                    |> Review.Test.expectNoErrors
        ]


unusedModuleTests : Test
unusedModuleTests =
    describe "When module is never imported"
        [ test "should not report a module when all modules are used" <|
            \() ->
                [ """
module NotReported exposing (..)
import OtherModule
main = text ""
"""
                , """
module OtherModule exposing (..)
a = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should report a module when it is never used" <|
            \() ->
                [ """
module NotReported exposing (..)
main = text ""
"""
                , """
module Reported exposing (..)
a = 1
"""
                , """
module Other.Reported exposing (..)
a = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Module `Reported` is never used."
                                , details = unusedModuleDetails
                                , under = "Reported"
                                }
                            ]
                          )
                        , ( "Other.Reported"
                          , [ Review.Test.error
                                { message = "Module `Other.Reported` is never used."
                                , details = unusedModuleDetails
                                , under = "Other.Reported"
                                }
                            ]
                          )
                        ]
        , test "should report a module even if it is the only module in the project" <|
            \() ->
                """
module Reported exposing (..)
import Something
a = 1
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                        ]
        , test "should not report an application module if it exposes a main function" <|
            \() ->
                """
module NotReported exposing (..)
main = text ""
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an application module if it contains a main function even if it is not exposed (but should report the unused exposed things)" <|
            \() ->
                """
module NotReported exposing (a)
main = text ""
a = 1
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed function or value `a` is never used outside this module."
                            , details = unusedExposedElementDetails
                            , under = "a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 30 }, end = { row = 2, column = 31 } }
                        ]
        , test "should not report a module with main function if we don't know the project type" <|
            \() ->
                """
module NotReported exposing (main)
main = text ""
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a module if it imports `Test`" <|
            \() ->
                """
module NotReported exposing (..)
import Test
a = 1
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report the `ReviewConfig` module" <|
            \() ->
                """
module ReviewConfig exposing (config)
config = []
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report modules exposed in a package" <|
            \() ->
                """
module Exposed exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectNoErrors
        , test "should report non-exposed and non-used modules from a package" <|
            \() ->
                """
module NotExposed exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module `NotExposed` is never used."
                            , details = unusedModuleDetails
                            , under = "NotExposed"
                            }
                        ]
        , test "should report non-exposed and non-used package modules that expose a `main` function" <|
            \() ->
                """
module Reported exposing (main)
main = text ""
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                        ]
        , test "should report non-exposed and non-used package modules that define a `main` function" <|
            \() ->
                """
module Reported exposing (a)
main = text ""
a = 1
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                        ]
        , test "should report modules that contain a top-level `app` function in packages" <|
            \() ->
                """
module Reported exposing (app)
app = text ""
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                        ]
        , test "should report modules that contain a top-level `app` function in Elm applications" <|
            \() ->
                """
module Reported exposing (app)
app = text ""
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                        ]
        , test "should not report modules that contain a top-level `app` function in Lamdera applications" <|
            \() ->
                """
module Reported exposing (app)
app = text ""
"""
                    |> Review.Test.runWithProjectData lamderaApplication rule
                    |> Review.Test.expectNoErrors
        , test "should not report modules that contain a top-level `main` function in Lamdera applications" <|
            \() ->
                """
module Reported exposing (main)
main = text ""
"""
                    |> Review.Test.runWithProjectData lamderaApplication rule
                    |> Review.Test.expectNoErrors
        ]
