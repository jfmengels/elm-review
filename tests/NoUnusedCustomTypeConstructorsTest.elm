module NoUnusedCustomTypeConstructorsTest exposing (all)

import Elm.Project
import Json.Decode as Decode
import NoUnused.CustomTypeConstructors2 exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


packageProject : Project
packageProject =
    Project.new
        |> Project.withElmJson (createElmJson packageElmJson)


applicationProject : Project
applicationProject =
    Project.new
        |> Project.withElmJson (createElmJson applicationElmJson)


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = elmJson
            }

        Err _ ->
            Debug.todo "Invalid elm.json supplied to test"


applicationElmJson : String
applicationElmJson =
    """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "author/package-with-foo": "1.0.0",
            "author/package-with-bar": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""


packageElmJson : String
packageElmJson =
    """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "author/package-with-foo": "1.0.0 <= v < 2.0.0",
        "author/package-with-bar": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


details : List String
details =
    [ "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."
    , "You should either use this value somewhere, or remove it at the location I pointed at."
    , "If you remove it, you may find that other pieces of code are never used, and can themselves be removed too. This could end up simplifying your code a lot."
    ]


all : Test
all =
    describe "NoUnusedCustomTypeConstructors"
        [ unusedTests "package project" packageProject
        , unusedTests "application project" applicationProject
        , phantomTypeTests "package project" packageProject
        , phantomTypeTests "application project" applicationProject
        , crossModuleTests
        , usingConstructorsFromOtherModules "package project" packageProject
        ]


unusedTests : String -> Project -> Test
unusedTests typeOfProject project =
    describe ("Unused variables for " ++ typeOfProject)
        [ test "should not report non-exposed variables" <|
            \() ->
                """
module MyModule exposing (b)
a = 1"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report used type constructors" <|
            \() ->
                """
module MyModule exposing (b)
type Foo = Bar | Baz
a = Bar
b = Baz"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors" <|
            \() ->
                """
module MyModule exposing (b)
type Foo = Bar | Baz"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = details
                            , under = "Bar"
                            }
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = details
                            , under = "Baz"
                            }
                        ]
        , test "should report unused type constructors, even if the type is exposed" <|
            \() ->
                """
module MyModule exposing (Foo)
type Foo = Bar | Baz"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = details
                            , under = "Bar"
                            }
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = details
                            , under = "Baz"
                            }
                        ]
        ]


phantomTypeTests : String -> Project -> Test
phantomTypeTests typeOfProject project =
    describe ("Phantom type for " ++ typeOfProject)
        [ test "should not report a custom type with one constructor, when it is used in the stead of a phantom variable" <|
            \() ->
                """
module MyModule exposing (id)
type User = User
type Id a = Id

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type with one constructor, when it is used in the stead of a phantom variable in a let variable" <|
            \() ->
                """
module MyModule exposing (id)
type User = User
type Id a = Id


id =
  let
    a : Id User
    a = Id
  in
  a
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report a custom type with multiple constructors, when it is used in the stead of a phantom variable" <|
            \() ->
                """
module MyModule exposing (id)
type Something = A | B
type Id a = Id

id : Id Something
id = Id
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `A` is not used."
                            , details = details
                            , under = "A"
                            }
                        , Review.Test.error
                            { message = "Type constructor `B` is not used."
                            , details = details
                            , under = "B"
                            }
                        ]
        , test "should report a custom type with one constructor, when there is a phantom type available but it isn't used" <|
            \() ->
                """
module MyModule exposing (id)
type User = User
type Id a = Id
id = Id
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 17 } }
                        ]
        , test "should report a custom type with one constructor when the constructor is named differently than the type, even when it is used in the stead of a phantom variable" <|
            \() ->
                """
module MyModule exposing (id)
type User = UserConstructor
type Id a = Id

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `UserConstructor` is not used."
                            , details = details
                            , under = "UserConstructor"
                            }
                        ]
        , test "should report a custom type with one constructor, when it is used in the stead of a non-phantom variable" <|
            \() ->
                """
module MyModule exposing (id)
type User = User
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 17 } }
                        ]
        , test "should report a custom type with a type variable, when it is used in the stead of a phantom variable" <|
            \() ->
                """
module MyModule exposing (id)
type User something = User
type Id a = Id a

id : Id (User otherThing)
id = Id
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 23 }, end = { row = 3, column = 27 } }
                        ]
        , test "should report a custom type with one constructor that has arguments, when it is used in the stead of a phantom variable" <|
            \() ->
                """
module MyModule exposing (id)
type User = User Something
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 17 } }
                        ]
        ]


crossModuleTests : Test
crossModuleTests =
    describe "Exposed constructors"
        [ test "should not report unused type constructors when a package module is exposing all and module is exposed" <|
            \() ->
                """
module Exposed exposing (..)
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors when a package module is exposing all and module is not exposed" <|
            \() ->
                """
module MyModule exposing (..)
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = details
                            , under = "Bar"
                            }
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = details
                            , under = "Baz"
                            }
                        ]
        , test "should report unused type constructors when an application module is exposing all" <|
            \() ->
                """
module MyModule exposing (..)
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData applicationProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = details
                            , under = "Bar"
                            }
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = details
                            , under = "Baz"
                            }
                        ]
        , test "should not report unused type constructors when package module is exposing the constructors of that type and module is exposed" <|
            \() ->
                """
module Exposed exposing (Foo(..))
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors when package module is exposing the constructors of that type and module is not exposed" <|
            \() ->
                """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = details
                            , under = "Bar"
                            }
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = details
                            , under = "Baz"
                            }
                        ]
        , test "should report unused type constructors when application module is exposing the constructors" <|
            \() ->
                """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData applicationProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = details
                            , under = "Bar"
                            }
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = details
                            , under = "Baz"
                            }
                        ]
        ]


usingConstructorsFromOtherModules : String -> Project -> Test
usingConstructorsFromOtherModules typeOfProject project =
    Test.skip <|
        describe ("Using constructors from others modules (" ++ typeOfProject ++ ")")
            [ test "should not report type constructors used in other files when module is exposing the constructors of that type (qualifed import)" <|
                \() ->
                    [ """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
""", """
module OtherModule exposing (a)
import MyModule
a = [ MyModule.Bar, MyModule.Baz ]
""" ]
                        |> Review.Test.runOnModulesWithProjectData project rule
                        |> Review.Test.expectNoErrors
            ]
