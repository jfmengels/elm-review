module NoUnused.CustomTypeConstructorsTest exposing (all)

import Elm.Project
import Json.Decode as Decode
import NoUnused.CustomTypeConstructors exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


packageProject : Project
packageProject =
    Review.Test.Dependencies.projectWithElmCore
        |> Project.addElmJson (createElmJson packageElmJson)


applicationProject : Project
applicationProject =
    Review.Test.Dependencies.projectWithElmCore
        |> Project.addElmJson (createElmJson applicationElmJson)


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = elmJson
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


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


defaultDetails : String
defaultDetails =
    "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."


conditionDetails : String
conditionDetails =
    "I found it used in comparisons, but since it is never created anywhere, all of those can be evaluated to False (for (==), True for (/=))."


recursiveNeedDetails : String
recursiveNeedDetails =
    "The only locations where I found it being created require already having one."


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
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report used type constructors" <|
            \() ->
                """
module MyModule exposing (b)
type Foo = Bar | Baz
a = Bar
b = Baz"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors" <|
            \() ->
                """
module MyModule exposing (b)
type Foo = Bar | Baz"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = [ defaultDetails ]
                            , under = "Bar"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (b)
type Foo = Baz"""
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = [ defaultDetails ]
                            , under = "Baz"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (b)
type Foo = Bar"""
                        ]
        , test "should report unused type constructors, even if the type is exposed" <|
            \() ->
                """
module MyModule exposing (Foo)
type Foo = Bar | Baz"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = [ defaultDetails ]
                            , under = "Bar"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (Foo)
type Foo = Baz"""
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = [ defaultDetails ]
                            , under = "Baz"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (Foo)
type Foo = Bar"""
                        ]
        , test "should report type constructors that are only used inside pattern matches that require themselves" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Used | Unused
a = case () of
        Unused -> Unused + Used
        Used -> Used
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, recursiveNeedDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 19 }, end = { row = 3, column = 25 } }
                            |> Review.Test.whenFixed
                                ("""
module MyModule exposing (a)
type Foo = Used
a = case () of
       $
        Used -> Used
""" |> String.replace "$" " ")
                        ]
        , test "should report type constructors that are only used inside pattern matches that require themselves (reversed order of patterns)" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Used | Unused
a = case () of
        Used -> Used
        Unused -> Unused + Used
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, recursiveNeedDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 19 }, end = { row = 3, column = 25 } }
                            |> Review.Test.whenFixed
                                ("""
module MyModule exposing (a)
type Foo = Used
a = case () of
        Used -> Used
""" |> String.replace "$" " ")
                        ]
        , test "should report type constructors that are only used inside deep pattern matches that require themselves" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Used | Unused
a = case () of
        Foo (a :: [ ( Unused as b, bar ) ]) -> Unused + Used
        Used -> Used
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, recursiveNeedDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 19 }, end = { row = 3, column = 25 } }
                            |> Review.Test.whenFixed
                                ("""
module MyModule exposing (a)
type Foo = Used
a = case () of
       $
        Used -> Used
""" |> String.replace "$" " ")
                        ]
        , test "should properly remove the ignored constructors once the pattern has been left" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = A | B | C
a = case () of
        A -> B
        B -> A
        C -> 1
b = C
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not ignore type constructors that are used after other case expressions" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = A | B
a = case () of
        A -> case () of
                B -> 1
        B -> A
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not count type constructors used in an equality expression (==)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = Unused == value
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, conditionDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 18 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (a, b)
type Foo = B
a = False
b = B
"""
                        ]
        , test "should not count type constructors used in an equality expression (/=)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = value /= Unused
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, conditionDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 18 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (a, b)
type Foo = B
a = True
b = B
"""
                        ]
        , test "should count type constructors used in a function call inside an equality expression" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = foo Unused == value
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not count type constructors used in an deep static expression" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = value /= Just ([value, ({foo = Unused}, {bar | foo = Unused})])
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, conditionDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 18 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (a, b)
type Foo = B
a = True
b = B
"""
                        ]
        , test "should not count type constructors used as arguments to a prefix (==) operator (with 2 arguments)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = (==) Unused value
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, conditionDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 18 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (a, b)
type Foo = B
a = False
b = B
"""
                        ]
        , test "should not count type constructors used as arguments to a prefix (/=) operator (with 2 arguments)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = (/=) value Unused
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, conditionDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 18 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (a, b)
type Foo = B
a = True
b = B
"""
                        ]
        , test "should not count type constructors used as arguments to a prefix (==) operator (with 1 argument)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = (==) Unused
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, conditionDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 18 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (a, b)
type Foo = B
a = always False
b = B
"""
                        ]
        , test "should not count type constructors used as arguments to a prefix (/=) operator (with 1 argument)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused | B
a = (/=) Unused
b = B
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Unused` is not used."
                            , details = [ defaultDetails, conditionDetails ]
                            , under = "Unused"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 18 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (a, b)
type Foo = B
a = always True
b = B
"""
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
                    |> Review.Test.runWithProjectData project (rule [])
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
                    |> Review.Test.runWithProjectData project (rule [])
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
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `A` is not used."
                            , details = [ defaultDetails ]
                            , under = "A"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (id)
type Something = B
type Id a = Id

id : Id Something
id = Id
"""
                        , Review.Test.error
                            { message = "Type constructor `B` is not used."
                            , details = [ defaultDetails ]
                            , under = "B"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (id)
type Something = A
type Id a = Id

id : Id Something
id = Id
"""
                        ]
        , test "should report a custom type with one constructor, when there is a phantom type available but it isn't used" <|
            \() ->
                """
module MyModule exposing (id)
type User = User
type Id a = Id
id = Id
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = [ defaultDetails ]
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
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `UserConstructor` is not used."
                            , details = [ defaultDetails ]
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
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = [ defaultDetails ]
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
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = [ defaultDetails ]
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
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = [ defaultDetails ]
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 17 } }
                        ]
        , test "should not report a custom type with one constructor that takes Never (constructor named like the type)" <|
            \() ->
                """
module MyModule exposing (id)
type User = User Never
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type with one constructor that takes Never (constructor named differently from the type)" <|
            \() ->
                """
module MyModule exposing (id)
type User = SomeConstructor Never
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type with one constructor that takes itself" <|
            \() ->
                """
module MyModule exposing (id)
type User = SomeConstructor User
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report a custom type with multiple constructors, even when some take Never" <|
            \() ->
                """
module MyModule exposing (id)
type User = User Never | Other
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = [ defaultDetails ]
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 17 } }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (id)
type User = Other
type Id a = Id a

id : Id User
id = Id
"""
                        , Review.Test.error
                            { message = "Type constructor `Other` is not used."
                            , details = [ defaultDetails ]
                            , under = "Other"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (id)
type User = User Never
type Id a = Id a

id : Id User
id = Id
"""
                        ]
        , test "should not report a phantom type if it is used in another module (directly imported)" <|
            \() ->
                [ """
module MyModule exposing (User(..))
type User = User Something
""", """
module OtherModule exposing (id)
import MyModule exposing (User)
type Id a = Id

id : Id User
id = Id
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report a phantom type if it is used in another module (qualified imported)" <|
            \() ->
                [ """
module MyModule exposing (User(..))
type User = User Something
""", """
module OtherModule exposing (id)
import MyModule
type Id a = Id

id : Id MyModule.User
id = Id
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report a phantom type if both container and phantom type are defined in another module" <|
            \() ->
                [ """
module Id exposing (Id(..))
import MyModule
type Id a = Id
""", """
module MyModule exposing (User(..))
type User = User Something
""", """
module OtherModule exposing (id)
import Id

id : Id.Id MyModule.User
id = Id.Id
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report a phantom type if it is used in another module even when constructors are not exposed" <|
            \() ->
                [ """
module MyModule exposing (User)
type User = User Something
""", """
module OtherModule exposing (id)
import MyModule exposing (User)
type Id a = Id

id : Id User
id = Id
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type, when it is used in the stead of a type that the user said is a phantom variable" <|
            \() ->
                """
module MyModule exposing (id)

import IdModule exposing (Id)

type User = User Something

id : Id User
id = Id
"""
                    |> Review.Test.runWithProjectData project
                        (rule
                            [ { moduleName = "IdModule"
                              , typeName = "Id"
                              , index = 0
                              }
                            ]
                        )
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type, when it is used aliased by a type alias" <|
            \() ->
                """
module MyModule exposing (Thing)
import IdModule exposing (Id)

type ThingType = ThingType
type alias ThingId = Id ThingType
type alias Thing = { id : Id ThingType }
"""
                    |> Review.Test.runWithProjectData project
                        (rule
                            [ { moduleName = "IdModule"
                              , typeName = "Id"
                              , index = 0
                              }
                            ]
                        )
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.runWithProjectData packageProject (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors when a package module is exposing all and module is not exposed" <|
            \() ->
                """
module MyModule exposing (..)
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData packageProject (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = [ defaultDetails ]
                            , under = "Bar"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (..)
type Foo = Baz
"""
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = [ defaultDetails ]
                            , under = "Baz"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (..)
type Foo = Bar
"""
                        ]
        , test "should report unused type constructors when a package module is exposing all and module is exposed but types are not" <|
            \() ->
                """
module Exposed exposing (Opaque)
type Opaque = Opaque
type NotExposed = NotExposed
a = 1
"""
                    |> Review.Test.runWithProjectData packageProject (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Opaque` is not used."
                            , details = [ defaultDetails ]
                            , under = "Opaque"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 15 }, end = { row = 3, column = 21 } }
                        , Review.Test.error
                            { message = "Type constructor `NotExposed` is not used."
                            , details = [ defaultDetails ]
                            , under = "NotExposed"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 19 }, end = { row = 4, column = 29 } }
                        ]
        , test "should not report unused type constructors when a package module is exposing all and module and module exposes everything" <|
            \() ->
                """
module Exposed exposing (..)
type Foo = Unused
a = 1
"""
                    |> Review.Test.runWithProjectData packageProject (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors when an application module is exposing all" <|
            \() ->
                """
module MyModule exposing (..)
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData applicationProject (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = [ defaultDetails ]
                            , under = "Bar"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (..)
type Foo = Baz
"""
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = [ defaultDetails ]
                            , under = "Baz"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (..)
type Foo = Bar
"""
                        ]
        , test "should not report unused type constructors when package module is exposing the constructors of that type and module is exposed" <|
            \() ->
                """
module Exposed exposing (Foo(..))
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData packageProject (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors when package module is exposing the constructors of that type and module is not exposed" <|
            \() ->
                """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData packageProject (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = [ defaultDetails ]
                            , under = "Bar"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (Foo(..))
type Foo = Baz
"""
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = [ defaultDetails ]
                            , under = "Baz"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (Foo(..))
type Foo = Bar
"""
                        ]
        , test "should report unused type constructors when application module is exposing the constructors" <|
            \() ->
                """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
"""
                    |> Review.Test.runWithProjectData applicationProject (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `Bar` is not used."
                            , details = [ defaultDetails ]
                            , under = "Bar"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (Foo(..))
type Foo = Baz
"""
                        , Review.Test.error
                            { message = "Type constructor `Baz` is not used."
                            , details = [ defaultDetails ]
                            , under = "Baz"
                            }
                            |> Review.Test.whenFixed
                                """
module MyModule exposing (Foo(..))
type Foo = Bar
"""
                        ]
        ]


usingConstructorsFromOtherModules : String -> Project -> Test
usingConstructorsFromOtherModules typeOfProject project =
    describe ("Using constructors from others modules (" ++ typeOfProject ++ ")")
        [ test "should not report type constructors used in other files when module is exposing the constructors of that type (qualified import)" <|
            \() ->
                [ """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
""", """
module OtherModule exposing (a)
import MyModule
a = [ MyModule.Bar, MyModule.Baz ]
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report type constructors used in other files when module is exposing the constructors of that type (qualified import, aliasing the module name)" <|
            \() ->
                [ """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
""", """
module OtherModule exposing (a)
import MyModule as M
a = [ M.Bar, M.Baz ]
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report type constructors used in other files when module is exposing the constructors of that type (exposing the constructors)" <|
            \() ->
                [ """
module MyModule exposing (Foo(..))
type Foo = Bar | Baz
""", """
module OtherModule exposing (a)
import MyModule exposing (Foo(..))
a = [ Bar, Baz ]
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report type constructors used in other files when module is exposing the constructors of that type (nested module name)" <|
            \() ->
                [ """
module My.Module exposing (Foo(..))
type Foo = Bar | Baz
""", """
module OtherModule exposing (a)
import My.Module
a = [ My.Module.Bar, My.Module.Baz ]
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should correctly find the custom type constructor that gets shadowed" <|
            \() ->
                [ """module A exposing (main)
import Other exposing (Msg(..))
type Msg = NoOp

a = NoOp
""", """module Other exposing (Msg(..))
type Msg = NoOp
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "Other"
                          , [ Review.Test.error
                                { message = "Type constructor `NoOp` is not used."
                                , details = [ defaultDetails ]
                                , under = "NoOp"
                                }
                            ]
                          )
                        ]
        , test "should report but not fix if constructor is handled in a pattern" <|
            \() ->
                [ """module A exposing (main)
import Other exposing (Msg(..))
a = Used
main = case foo of
  Unused -> 1
""", """module Other exposing (Msg(..))
type Msg = Unused | Used
""" ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "Other"
                          , [ Review.Test.error
                                { message = "Type constructor `Unused` is not used."
                                , details = [ defaultDetails ]
                                , under = "Unused"
                                }
                            ]
                          )
                        ]
        , test "should not report type constructors in confusing situations" <|
            \() ->
                [ """module A exposing (A(..))
type A = Foo | Bar
"""
                , """module B exposing (B(..))
type B = A | B
"""
                , """module C exposing (foo)
import A exposing (A(..))
import B exposing (B(..))
foo = [ (Foo, A), (Bar, B) ]
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report imported type constructors when they are shadowed by a local type alias that does not create a function (#5)" <|
            \() ->
                [ """module A exposing (x)

import B exposing (Foo(..))

type alias Foo = B.Foo -- this "shadowing" is causing the bug, removing the alias removes the false positive

x = Foo 1 -- usage of the custom type constructor!
"""
                , """module B exposing (Foo(..))
type Foo = Foo Int
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report imported type constructors even when they are shadowed by a local type alias that creates a function (#5)" <|
            \() ->
                [ """module A exposing (x)

import B exposing (Foo(..))

type alias Foo = { n : Int }

x = Foo 1 -- Not a usage of B.Foo in this case!
"""
                , """module B exposing (Foo(..))
type Foo = Foo Int
"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project (rule [])
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ Review.Test.error
                                { message = "Type constructor `Foo` is not used."
                                , details = [ defaultDetails ]
                                , under = "Foo"
                                }
                                |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } }
                            ]
                          )
                        ]
        ]
