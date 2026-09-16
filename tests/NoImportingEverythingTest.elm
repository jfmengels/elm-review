module NoImportingEverythingTest exposing (all)

import Elm.Project
import Json.Decode as Decode
import NoImportingEverything exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoImportingEverything"
        [ describe "for package" (testsForProject packageProject)
        , describe "for application" (testsForProject applicationProject)
        ]


testsForProject : Project -> List Test
testsForProject project =
    [ test "should not report imports without exposing clause" <|
        \() ->
            """module A exposing (thing)
import Html
import Html as B
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectNoErrors
    , test "should not report imports that expose some elements" <|
        \() ->
            """module A exposing (thing)
import Html exposing (B, c)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectNoErrors
    , test "should not report imports that expose all constructors of a type" <|
        \() ->
            """module A exposing (thing)
import Html exposing (B(..))
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectNoErrors
    , test "should report imports that expose everything" <|
        \() ->
            """module A exposing (thing)
import Html exposing (..)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (thing)
import Html
"""
                    ]
    , test "should report aliased imports that expose everything" <|
        \() ->
            """module A exposing (thing)
import Json.Decode as JD exposing (..)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (thing)
import Json.Decode as JD
"""
                    ]
    , test "should only include used values in the fixed exposing list" <|
        \() ->
            """module A exposing (view)
import Html exposing (..)
view = p [] [ strong [] [ text "Thing" ] ]
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (view)
import Html exposing (p, strong, text)
view = p [] [ strong [] [ text "Thing" ] ]
"""
                    ]
    , test "should not include qualified imports values in the fixed exposing list" <|
        \() ->
            """module A exposing (view)
import Html exposing (..)
view = Html.p [] [ strong [] [ text "Thing" ] ]
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (view)
import Html exposing (strong, text)
view = Html.p [] [ strong [] [ text "Thing" ] ]
"""
                    ]
    , test "should import the constructors of the type when used value is a custom type constructor" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value = C1
"""
            , """module B exposing (SomeType(..))
type SomeType
  = C1
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (SomeType(..))
value = C1
"""
                        ]
                    ]
    , test "should import the constructors of the type found in a case pattern" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value x = case x of
    C1 -> True
"""
            , """module B exposing (SomeType(..))
type SomeType
  = C1
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (SomeType(..))
value x = case x of
    C1 -> True
"""
                        ]
                    ]
    , test "should import the constructors of the type found in a top-level function patterns" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value (Constructor x) = x
"""
            , """module B exposing (SomeType(..))
type SomeType
  = Constructor String
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (SomeType(..))
value (Constructor x) = x
"""
                        ]
                    ]
    , test "should import the constructors of the type found in a lambda's argument's patterns" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value = \\(Constructor x) -> 1
"""
            , """module B exposing (SomeType(..))
type SomeType
  = Constructor String
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (SomeType(..))
value = \\(Constructor x) -> 1
"""
                        ]
                    ]
    , test "should import the constructors of a type from a dependency" <|
        \() ->
            """module A exposing (x)
import Parser.Advanced exposing (..)
x = Mandatory
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (x)
import Parser.Advanced exposing (Trailing(..))
x = Mandatory
"""
                    ]
    , test "should import the constructors of the type found in a let function patterns" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value = let fn (Constructor x) = x
        in fn (Debug.todo "x")
"""
            , """module B exposing (SomeType(..))
type SomeType
  = Constructor String
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (SomeType(..))
value = let fn (Constructor x) = x
        in fn (Debug.todo "x")
"""
                        ]
                    ]
    , test "should import the constructors of the type found in a let destructuring patterns" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value = let (Constructor x) = Debug.todo "x"
        in x
"""
            , """module B exposing (SomeType(..))
type SomeType
  = Constructor String
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (SomeType(..))
value = let (Constructor x) = Debug.todo "x"
        in x
"""
                        ]
                    ]
    , test "should import a type only once, even if both constructors and types are referenced" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value : SomeType
value = C1
"""
            , """module B exposing (SomeType(..))
type SomeType
  = C1
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (SomeType(..))
value : SomeType
value = C1
"""
                        ]
                    ]
    , test "should not list qualified import in what gets imported" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value = let (B.Constructor x) = Debug.todo "x"
        in x
"""
            , """module B exposing (SomeType(..))
type SomeType
  = Constructor String
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B
value = let (B.Constructor x) = Debug.todo "x"
        in x
"""
                        ]
                    ]
    , test "should import the type found in a type annotation" <|
        \() ->
            """module A exposing (view)
import Html exposing (..)
view : Html msg
view = text ""
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (view)
import Html exposing (Html, text)
view : Html msg
view = text ""
"""
                    ]
    , test "should import the type found in a let..in type annotation" <|
        \() ->
            """module A exposing (view)
import Html exposing (..)
view =
    let
        x : Html msg
        x = text ""
    in x
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (view)
import Html exposing (Html, text)
view =
    let
        x : Html msg
        x = text ""
    in x
"""
                    ]
    , test "should import the type found in a custom type declaration" <|
        \() ->
            """module A exposing (X)
import Html exposing (..)
type X msg = H (Html msg)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (X)
import Html exposing (Html)
type X msg = H (Html msg)
"""
                    ]
    , test "should import the type found in a type alias declaration" <|
        \() ->
            """module A exposing (X)
import Html exposing (..)
type alias X msg = Html msg
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (X)
import Html exposing (Html)
type alias X msg = Html msg
"""
                    ]
    , test "should import a value from a record expression" <|
        \() ->
            [ """module A exposing (value)
import B exposing (..)
value = { x | a = 1 }
"""
            , """module B exposing (x)
x = { a = 1, b = 1 }
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project (rule [])
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "A"
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (value)
import B exposing (x)
value = { x | a = 1 }
"""
                        ]
                    ]
    , test "should import an operator from an operator application" <|
        \() ->
            """module A exposing (x)
import Parser exposing (..)
x a b = a |= b
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (x)
import Parser exposing ((|=))
x a b = a |= b
"""
                    ]
    , test "should import an operator from a prefix operator" <|
        \() ->
            """module A exposing (x)
import Parser exposing (..)
x = (|=)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (x)
import Parser exposing ((|=))
x = (|=)
"""
                    ]
    , test "should not report imports that are in the exceptions list" <|
        \() ->
            """module A exposing (thing)
import Html exposing (..)
import Thing.Foo as Foo exposing (..)
"""
                |> Review.Test.runWithProjectData project (rule [ "Html", "Thing.Foo" ])
                |> Review.Test.expectNoErrors
    ]


applicationProject : Project
applicationProject =
    Project.new
        |> Project.addElmJson (createElmJson applicationElmJson)
        |> Project.addDependency Review.Test.Dependencies.elmCore
        |> Project.addDependency Review.Test.Dependencies.elmHtml
        |> Project.addDependency Review.Test.Dependencies.elmParser


packageProject : Project
packageProject =
    Project.new
        |> Project.addElmJson (createElmJson packageElmJson)
        |> Project.addDependency Review.Test.Dependencies.elmCore
        |> Project.addDependency Review.Test.Dependencies.elmHtml
        |> Project.addDependency Review.Test.Dependencies.elmParser


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
            "elm/html": "1.0.0",
            "elm/parser": "1.1.0"
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
        "elm/html": "1.0.0 <= v < 2.0.0",
        "elm/parser": "1.1.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


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
