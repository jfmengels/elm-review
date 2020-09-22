module NoUnused.ExportsTest exposing (all)

import Elm.Project
import Elm.Version
import Json.Decode as Decode
import NoUnused.Exports exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


application : Project
application =
    Project.new
        |> Project.addElmJson applicationElmJson


applicationElmJson : { path : String, raw : String, project : Elm.Project.Project }
applicationElmJson =
    { path = "elm.json"
    , raw = """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.2"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""
    , project =
        Elm.Project.Application
            { elm = Elm.Version.one
            , dirs = []
            , depsDirect = []
            , depsIndirect = []
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }


package_ : Project
package_ =
    Project.new
        |> Project.addElmJson (createPackageElmJson ())


createPackageElmJson : () -> { path : String, raw : String, project : Elm.Project.Project }
createPackageElmJson _ =
    case Decode.decodeString Elm.Project.decoder rawPackageElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawPackageElmJson
            , project = elmJson
            }

        Err _ ->
            createPackageElmJson ()


rawPackageElmJson : String
rawPackageElmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}"""


details : List String
details =
    [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
    ]


all : Test
all =
    describe "NoUnusedExports"
        [ functionsAndValuesTests
        , typesTests
        , typeAliasesTests
        , duplicateModuleNameTests
        , importsTests

        -- TODO Add tests that report exposing the type's variants if they are never used.
        ]


functionsAndValuesTests : Test
functionsAndValuesTests =
    describe "Functions and values"
        [ test "should report an exposed function when it is not used in other modules" <|
            \() ->
                """
module A exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed function or value `a` is never used outside this module."
                            , details = details
                            , under = "a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 21 } }
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
module A exposing (..)
import B exposing (person)
otherPerson =
    { person | age = 30 }
""", """
module B exposing (person)
person =
    { age = 29 }
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should not report an exposed value when it is used in other modules (using record update syntax, importing all)" <|
            \() ->
                [ """
module A exposing (..)
import B exposing (..)
otherPerson =
    { person | age = 30 }
""", """
module B exposing (person)
person =
    { age = 29 }
""" ]
                    |> Review.Test.runOnModulesWithProjectData application rule
                    |> Review.Test.expectNoErrors
        , test "should report an exposed function when it is not used in other modules, even if it is used in the module" <|
            \() ->
                """
module A exposing (exposed)
exposed = 1
main = exposed
"""
                    |> Review.Test.runWithProjectData package_ rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed function or value `exposed` is never used outside this module."
                            , details = details
                            , under = "exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 27 } }
                        ]
        , test "should propose a fix for unused exports if there are others exposed elements" <|
            \() ->
                """
module A exposing (exposed1, exposed2)
exposed1 = 1
exposed2 = 2
"""
                    |> Review.Test.runWithProjectData package_ rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed function or value `exposed1` is never used outside this module."
                            , details = details
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
                            , details = details
                            , under = "exposed2"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 30 }, end = { row = 2, column = 38 } }
                            |> Review.Test.whenFixed """
module A exposing (exposed1)
exposed1 = 1
exposed2 = 2
"""
                        ]
        , test "should not report anything for modules that expose everything`" <|
            \() ->
                """
module A exposing (..)
a = 1
"""
                    |> Review.Test.runWithProjectData package_ rule
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
                """
module Main exposing (main)
main = text ""
"""
                    |> Review.Test.runWithProjectData package_ rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed function or value `main` is never used outside this module."
                            , details = details
                            , under = "main"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 27 } }
                        ]
        , test "should not report a function that does not refer to anything" <|
            \() ->
                """
module A exposing (b)
a = 1
"""
                    |> Review.Test.runWithProjectData package_ rule
                    |> Review.Test.expectNoErrors
        , test "should not report exposed tests" <|
            \() ->
                """
module ThingTest exposing (a)
import Test exposing (Test)
a : Test
a = Test.describe "thing" []
"""
                    |> Review.Test.runWithProjectData package_ rule
                    |> Review.Test.expectNoErrors
        , test "should not ReviewConfig.config" <|
            \() ->
                """
module ReviewConfig exposing (config)
config = []
"""
                    |> Review.Test.runWithProjectData package_ rule
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
                """
module A exposing (Exposed)
type Exposed = VariantA | VariantB
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed type or type alias `Exposed` is never used outside this module."
                            , details = details
                            , under = "Exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 27 } }
                        ]
        , test "should not report a used exposed custom type (type signature)" <|
            \() ->
                [ """
module A exposing (Exposed)
type Exposed = VariantA | VariantB
""", """
module B exposing (main)
import A
main : A.Exposed
main = VariantA
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's part of the package's exposed API" <|
            \() ->
                """
module Exposed exposing (MyType)
type MyType = VariantA | VariantB
"""
                    |> Review.Test.runWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed custom type if it's present in an exposed custom type constructor's arguments but the constructors are not exposed" <|
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = details
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = details
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
        ]


typeAliasesTests : Test
typeAliasesTests =
    describe "Type aliases"
        [ test "should report an unused exposed type alias" <|
            \() ->
                """
module A exposing (Exposed)
type alias Exposed = {}
"""
                    |> Review.Test.runWithProjectData application rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Exposed type or type alias `Exposed` is never used outside this module."
                            , details = details
                            , under = "Exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 27 } }
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectNoErrors
        , test "should not report an unused exposed type alias if it's part of the package's exposed API" <|
            \() ->
                """
module Exposed exposing (MyType)
type alias MyType = {}
"""
                    |> Review.Test.runWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = details
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `MyType` is never used outside this module."
                                , details = details
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
                    |> Review.Test.runOnModulesWithProjectData package_ rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Exposed type or type alias `Link` is never used outside this module."
                                , details = details
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
