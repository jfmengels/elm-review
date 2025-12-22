module NoUnused.ExportsTest exposing (all)

import NoUnused.Exports exposing (annotatedBy, defaults, definedInModule, prefixedBy, reportUnusedProductionExports, rule, suffixedBy, toRule)
import Review.Test
import Test exposing (Test, describe, test)
import TestProject exposing (application, lamderaApplication, package)


unusedExposedElementDetails : List String
unusedExposedElementDetails =
    [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
    ]


unusedExposedElementWhenExposingAllDetails : List String
unusedExposedElementWhenExposingAllDetails =
    [ "This exposed element is never used, neither inside its module nor outside. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
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
        , reportUnusedProductionExportsTest
        , exposingTypeVariantsTests
        , exposingAllTests
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
                                { message = "Exposed function or value `a` is never used outside this module"
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
                                { message = "Exposed function or value `exposed` is never used outside this module"
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
                                { message = "Exposed function or value `exposed1` is never used outside this module"
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
                                { message = "Exposed function or value `exposed2` is never used outside this module"
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
                                { message = "Exposed function or value `exposed1` is never used outside this module"
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
                                { message = "Exposed function or value `exposed2` is never used outside this module"
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
                                { message = "Exposed function or value `exposed3` is never used outside this module"
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
                                { message = "Exposed function or value `main` is never used outside this module"
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
                                { message = "Exposed type or type alias `Exposed` is never used outside this module"
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
        , test "should report an unused custom type even if one of its variants is named the same as another type (top-level declaration)" <|
            \() ->
                [ """
module Main exposing (main)
import M
main = x
x : M.T
x = M.t
""", """
module M exposing (Unused(..), T, t)
type alias T = ()
t = ()
type Unused = T
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "M"
                          , [ Review.Test.error
                                { message = "Exposed type `Unused` is never used outside this module"
                                , details = unusedExposedElementDetails
                                , under = "Unused(..)"
                                }
                            ]
                          )
                        ]
        , test "should report an unused custom type even if one of its variants is named the same as another type (let function usage)" <|
            \() ->
                [ """
module Main exposing (main)
import M
main =
    let x : M.T
        x = M.t
    in x
""", """
module M exposing (Unused(..), T, t)
type alias T = ()
t = ()
type Unused = T
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "M"
                          , [ Review.Test.error
                                { message = "Exposed type `Unused` is never used outside this module"
                                , details = unusedExposedElementDetails
                                , under = "Unused(..)"
                                }
                            ]
                          )
                        ]
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
b = A.Thing A.VariantB
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
                                { message = "Exposed type or type alias `MyType` is never used outside this module"
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
b = A.OtherThing
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
                                { message = "Exposed type or type alias `MyType` is never used outside this module"
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
                                { message = "Exposed type or type alias `Exposed` is never used outside this module"
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
b = A.OtherType {}
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
                                { message = "Exposed type or type alias `MyType` is never used outside this module"
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
b = A.OtherThing
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
                                { message = "Exposed type or type alias `MyType` is never used outside this module"
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
             exposing ( Card(..)
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
                                { message = "Exposed type or type alias `Link` is never used outside this module"
                                , details = unusedExposedElementDetails
                                , under = "Link"
                                }
                                |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                                |> Review.Test.whenFixed """module A
             exposing ( Card(..)
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
                                { message = "Exposed function or value `app` is never used outside this module"
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
                            { message = "Exposed function or value `app` is never used outside this module"
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
        , test "should not special types from module Types" <|
            \() ->
                [ """
module Types exposing (..)
type alias FrontendModel = {}
type alias BackendModel = {}
type FrontendMsg = NoOpFrontendMsg
type ToBackend = NoOpToBackend
type BackendMsg = NoOpBackendMsg
type ToFrontend = NoOpToFrontend
"""
                , """
module Backend exposing (..)
import Types
app = Debug.todo "app"
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData lamderaApplication rule
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
main = OtherModule.a
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
                                { message = "Module `Reported` is never used"
                                , details = unusedModuleDetails
                                , under = "Reported"
                                }
                                |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Reported", Review.Test.removed ) ]
                            ]
                          )
                        , ( "Other.Reported"
                          , [ Review.Test.error
                                { message = "Module `Other.Reported` is never used"
                                , details = unusedModuleDetails
                                , under = "Other.Reported"
                                }
                                |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Other.Reported", Review.Test.removed ) ]
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
                            { message = "Module `Reported` is never used"
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Reported", Review.Test.removed ) ]
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
                            { message = "Exposed function or value `a` is never used outside this module"
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
a : Test.Test
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
                            { message = "Module `NotExposed` is never used"
                            , details = unusedModuleDetails
                            , under = "NotExposed"
                            }
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "NotExposed", Review.Test.removed ) ]
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
                            { message = "Module `Reported` is never used"
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Reported", Review.Test.removed ) ]
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
                            { message = "Module `Reported` is never used"
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Reported", Review.Test.removed ) ]
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
                            { message = "Module `Reported` is never used"
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Reported", Review.Test.removed ) ]
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
                            { message = "Module `Reported` is never used"
                            , details = unusedModuleDetails
                            , under = "Reported"
                            }
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Reported", Review.Test.removed ) ]
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


reportUnusedProductionExportsTest : Test
reportUnusedProductionExportsTest =
    describe "reportUnusedProductionExports"
        [ test "should report functions that are only used in ignored files (no helpers defined)" <|
            \() ->
                [ """
module Main exposing (main)
import A
main = A.used
""", """
module A exposing (used, unusedInProductionCode)
used = 1
unusedInProductionCode = 2
""", """
module ATest exposing (..)
import A
import Test
a : Test.Test
a = A.unusedInProductionCode
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = []
                                }
                            |> toRule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed function or value `unusedInProductionCode` is never used in production code"
                                , details =
                                    [ "This exposed element is only used in files you have marked as non-production code (e.g. the tests folder), and should therefore be removed along with the places it's used in. This will help reduce the amount of code you will need to maintain."
                                    , "It is possible that this element is meant to enable work in your ignored folder (test helpers for instance), in which case you should keep it. To avoid this problem being reported again, please read the documentation on how to configure the rule."
                                    ]
                                , under = "unusedInProductionCode"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 26 }, end = { row = 2, column = 48 } }
                            ]
                          )
                        ]
        , test "should report functions that are only used in ignored files (helpers defined)" <|
            \() ->
                [ """
module Main exposing (main)
import A
main = A.used
""", """
module A exposing (used, unusedInProductionCode)
used = 1
unusedInProductionCode = 2
""", """
module ATest exposing (..)
import A
import Test
a : Test.Test
a = A.unusedInProductionCode
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre =
                                    [ annotatedBy "@helper"
                                    , annotatedBy "@test-helper"
                                    , suffixedBy "_FOR_TESTS"
                                    , prefixedBy "test_"
                                    ]
                                }
                            |> toRule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed function or value `unusedInProductionCode` is never used in production code"
                                , details =
                                    [ "This exposed element is only used in files you have marked as non-production code (e.g. the tests folder), and should therefore be removed along with the places it's used in. This will help reduce the amount of code you will need to maintain."
                                    , "It is possible that this element is meant to enable work in your ignored folder (test helpers for instance), in which case you should keep it. To avoid this problem being reported again, you can:"
                                    , """- Include @helper in the documentation of the element
- Include @test-helper in the documentation of the element
- Rename the element to end with _FOR_TESTS
- Rename the element to start with test_"""
                                    ]
                                , under = "unusedInProductionCode"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 26 }, end = { row = 2, column = 48 } }
                            ]
                          )
                        ]
        , test "should not report exposed tests even if they're in an ignored module" <|
            \() ->
                [ """
module Main exposing (main)
main = 1
""", """
module ATest exposing (tests)
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" []
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = []
                                }
                            |> toRule
                        )
                    |> Review.Test.expectNoErrors
        , test "should not report elements from ignored modules used in other ignored modules exposed tests even if they're in an ignored module" <|
            \() ->
                [ """
module ATest exposing (tests)
import BTest
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" BTest.helper
""", """
module BTest exposing (helper)
helper = 1
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = []
                                }
                            |> toRule
                        )
                    |> Review.Test.expectNoErrors
        , test "should not report elements only used in ignored modules if they're annotated with a tag" <|
            \() ->
                [ """
module ATest exposing (tests)
import B
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" B.helper
""", """
module B exposing (helper)
import Basics
{-| @ignore-helper -}
helper = Basics.min
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = [ annotatedBy "@ignore-helper" ]
                                }
                            |> toRule
                        )
                    |> Review.Test.expectNoErrors
        , test "should not report elements from ignored modules if they're imported only in tests but also used locally in the module" <|
            \() ->
                [ """
module Main exposing (main)
import B
main = B.exposed
""", """
module ATest exposing (tests)
import B
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" B.usedLocally
""", """
module B exposing (exposed, usedLocally)
exposed = usedLocally + 1
usedLocally = 1
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = []
                                }
                            |> toRule
                        )
                    |> Review.Test.expectNoErrors
        , test "should report elements never used anywhere even if they're annotated with a tag" <|
            \() ->
                [ """
module ATest exposing (tests)
import Test exposing (Test)
import B
tests : Test
tests = Test.describe "thing" []
""", """
module B exposing (helper)
{-| Module docs -}
{-| @ignore-helper -}
helper = 1
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = []
                                }
                            |> toRule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ Review.Test.error
                                { message = "Exposed function or value `helper` is never used outside this module"
                                , details = unusedExposedElementDetails
                                , under = "helper"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 26 } }
                            ]
                          )
                        ]
        , test "should report elements never used anywhere even if their name ends with the configured suffix" <|
            \() ->
                [ """
module Main exposing (main)
import B
main = B.b
""", """
module ATest exposing (tests)
import B
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" B.helperTEST
""", """
module B exposing (b, helperTEST)
b = 1
helperTEST = 1
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = [ suffixedBy "TEST" ]
                                }
                            |> toRule
                        )
                    |> Review.Test.expectNoErrors
        , test "should report elements never used anywhere even if their name starts with the configured suffix" <|
            \() ->
                [ """
module Main exposing (main)
import B
main = B.b
""", """
module ATest exposing (tests)
import B
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" B.test_helper
""", """
module B exposing (b, test_helper)
b = 1
test_helper = 1
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = [ prefixedBy "test_" ]
                                }
                            |> toRule
                        )
                    |> Review.Test.expectNoErrors
        , test "should report elements never used anywhere even if they're defined in a module marked as an exception" <|
            \() ->
                [ """
module Main exposing (main)
import Project.Utils.B as B
main = B.b
""", """
module ATest exposing (tests)
import Project.Utils.B as B
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" B.helper
""", """
module Project.Utils.B exposing (b, helper)
b = 1
helper = 1
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = [ definedInModule (\{ moduleName } -> List.member "Utils" moduleName) ]
                                }
                            |> toRule
                        )
                    |> Review.Test.expectNoErrors
        , test "should report unused exports in ignored files as regular errors" <|
            \() ->
                [ """
module Main exposing (main)
import B
main = B.b
""", """
module ATest exposing (tests, unused)
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" []
unused = 1
""" ]
                    |> Review.Test.runOnModules
                        (defaults
                            |> reportUnusedProductionExports
                                { isProductionFile = \{ moduleName } -> String.join "." moduleName |> String.endsWith "Test" |> not
                                , exceptionsAre = [ prefixedBy "test_" ]
                                }
                            |> toRule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "ATest"
                          , [ Review.Test.error
                                { message = "Exposed function or value `unused` is never used outside this module"
                                , details = unusedExposedElementDetails
                                , under = "unused"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 31 }, end = { row = 2, column = 37 } }
                                |> Review.Test.whenFixed """
module ATest exposing (tests)
import Test exposing (Test)
tests : Test
tests = Test.describe "thing" []
unused = 1
"""
                            ]
                          )
                        ]
        ]


exposingTypeVariantsTests : Test
exposingTypeVariantsTests =
    describe "Reporting unused exposing of type variants"
        [ test "should report and remove the exposing of variants if the variants are only used inside the module" <|
            \() ->
                [ """
module A exposing (Exposed(..), value)
type Exposed = VariantA | VariantB
value = VariantA
"""
                , """
module Main exposing (main)
import A
main : A.Exposed
main = A.value
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "The constructors for type `Exposed` are never used outside this module"
                                , details = [ "You should stop exposing the variants by removing the (..) at this location. You can re-expose them if necessary later." ]
                                , under = "Exposed(..)"
                                }
                                |> Review.Test.whenFixed """
module A exposing (Exposed, value)
type Exposed = VariantA | VariantB
value = VariantA
"""
                            ]
                          )
                        ]
        , test "should not report the exposing of the variants of a custom type exposed as part of a package's public API" <|
            \() ->
                """
module Exposed exposing (Exposed(..), value)
type Exposed = VariantA | VariantB
value = VariantA
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectNoErrors
        ]


exposingAllTests : Test
exposingAllTests =
    describe "When exposing all"
        [ test "reports an unused function" <|
            \() ->
                [ """
module Main exposing (main)
import Reported
main = Reported.used
"""
                , """
module Reported exposing (..)
unused = ()
used = 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Exposed function or value `unused` is never used in the project"
                                , details = unusedExposedElementWhenExposingAllDetails
                                , under = "unused"
                                }
                                |> Review.Test.whenFixed """
module Reported exposing (..)
used = 1
"""
                            ]
                          )
                        ]
        , test "reports an unused function (no fix because single declaration)" <|
            \() ->
                [ """
module Main exposing (main)
import Reported
main = ()
"""
                , """
module Reported exposing (..)
unused = ()
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Exposed function or value `unused` is never used in the project"
                                , details = unusedExposedElementWhenExposingAllDetails
                                , under = "unused"
                                }
                            ]
                          )
                        ]
        , test "reports an unused function (followed by a type declaration)" <|
            \() ->
                [ """
module Main exposing (main)
import Reported
main : Reported.Type
main = Reported.Constructor
"""
                , """
module Reported exposing (..)
unused = ()
type Type = Constructor
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Exposed function or value `unused` is never used in the project"
                                , details = unusedExposedElementWhenExposingAllDetails
                                , under = "unused"
                                }
                                |> Review.Test.whenFixed """
module Reported exposing (..)
type Type = Constructor
"""
                            ]
                          )
                        ]
        , test "reports an unused recursive function" <|
            \() ->
                [ """
module Main exposing (main)
import Reported
main = Reported.used
"""
                , """
module Reported exposing (..)
fib n = fib (n - 1) + fib (n - 2)
used = ()
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Exposed function or value `fib` is never used in the project"
                                , details = unusedExposedElementWhenExposingAllDetails
                                , under = "fib"
                                }
                                |> Review.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } }
                                |> Review.Test.whenFixed """
module Reported exposing (..)
used = ()
"""
                            ]
                          )
                        ]
        , test "reports an unused type alias" <|
            \() ->
                [ """
module Main exposing (main)
import Reported
main = ()
"""
                , """
module Reported exposing (..)
type alias Unused = ()
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `Unused` is never used in the project"
                                , details = unusedExposedElementWhenExposingAllDetails
                                , under = "Unused"
                                }
                            ]
                          )
                        ]
        , test "reports an unused custom type" <|
            \() ->
                [ """
module Main exposing (main)
import Reported
main = ()
"""
                , """
module Reported exposing (..)
type UnusedT = UnusedC
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Exposed type `UnusedT` is never used in the project"
                                , details = unusedExposedElementWhenExposingAllDetails
                                , under = "UnusedT"
                                }
                            ]
                          )
                        ]
        , test "reports an unused port" <|
            \() ->
                [ """
module Main exposing (main)
import Reported
main = ()
"""
                , """
port module Reported exposing (..)
port unused : ()
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Reported"
                          , [ Review.Test.error
                                { message = "Exposed function or value `unused` is never used in the project"
                                , details = unusedExposedElementWhenExposingAllDetails
                                , under = "unused"
                                }
                            ]
                          )
                        ]
        , test "should not report a function that's used internally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main = NotReported.external
"""
                , """
module NotReported exposing (..)
internal = ()
external = internal
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a type alias that's used internally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main : NotReported.External
main = ()
"""
                , """
module NotReported exposing (..)
type alias Internal = ()
type alias External = Internal
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type that's used internally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main = NotReported.external
"""
                , """
module NotReported exposing (..)
type InternalT = InternalC
external : InternalT
external = InternalC
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type whose constructor is used" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main = NotReported.Constructor
"""
                , """
module NotReported exposing (..)
type Type = Constructor
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type whose constructor is used internally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main = NotReported.external
"""
                , """
module NotReported exposing (..)
external = Constructor
type Type = Constructor
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "does not report a port that's used internally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main = NotReported.external
"""
                , """
port module NotReported exposing (..)
port internal : ()
external = internal
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a function that's used externally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main = NotReported.used
"""
                , """
module NotReported exposing (..)
used = ()
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a type alias that's used externally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main : NotReported.Used
main = ()
"""
                , """
module NotReported exposing (..)
type alias Used = ()
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type that's used externally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main : NotReported.UsedT
main = NotReported.UsedC
"""
                , """
module NotReported exposing (..)
type UsedT = UsedC
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report a port that's used externally" <|
            \() ->
                [ """
module Main exposing (main)
import NotReported
main = NotReported.used
"""
                , """
port module NotReported exposing (..)
port used : ()
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not remove a type alias used in a local let binding type annotation" <|
            \() ->
                [ """
module Main exposing (main)
import Tertiary
main = Tertiary.func 1 2
"""
                , """
module Tertiary exposing (..)

type alias Tertiary = {}

func : Int -> Int -> Int
func a b =
    let
        perhapsTertiary : Int -> Maybe Tertiary
        perhapsTertiary a =
            if a > 5 then
                Just {}
            else
                Nothing
    in
    case perhapsTertiary 4 of
        Just _ ->
            a + b

        Nothing ->
            a - 4
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not remove a type used in a local let destructuring" <|
            \() ->
                [ """
module Main exposing (main)
import Tertiary
main = Tertiary.func 1
"""
                , """
module Tertiary exposing (..)

type Wrapper = WrapperConstructor Int

func foo =
    let
        (WrapperConstructor int) = foo
    in
    int
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not custom type that is being pattern matched on locally" <|
            \() ->
                [ """
module Main exposing (main)
import Tertiary
main = Tertiary.func foo
"""
                , """
module Tertiary exposing (..)

type Used = X

func foo =
    case foo of
        X -> 1
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        ]
