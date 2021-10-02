module Docs.NoMissingTest exposing (all)

import Docs.NoMissing exposing (rule)
import Elm.Project
import Json.Decode as Decode
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


missingModuleDetails : List String
missingModuleDetails =
    [ "A module documentation summarizes what a module is for, the responsibilities it has and how to use it. Providing a good module documentation will be useful for your users or colleagues."
    ]


missingElementDetails : List String
missingElementDetails =
    [ "Documentation can help developers use this API." ]


all : Test
all =
    describe "Docs.NoMissing"
        [ everythingEverywhereTests
        , everythingFromExposedModulesTests
        , onlyExposedFromExposedModulesTests
        ]


everythingEverywhereTests : Test
everythingEverywhereTests =
    let
        config : { document : Docs.NoMissing.What, from : Docs.NoMissing.From }
        config =
            { document = Docs.NoMissing.everything
            , from = Docs.NoMissing.allModules
            }
    in
    describe "document everything - from everywhere"
        [ test "should report an error when a function does not have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing

function = 1
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing documentation for `function`"
                            , details = missingElementDetails
                            , under = "function"
                            }
                        ]
        , test "should not report an error when a function does have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing

{-| documentation -}
function = 1
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function's documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing

{-| -}
function = 1
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation for `function` is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "{-| -}"
                            }
                        ]
        , test "should report an error when a custom type does not have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
type CustomType = A
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing documentation for `CustomType`"
                            , details = missingElementDetails
                            , under = "CustomType"
                            }
                        ]
        , test "should not report an error when a custom type does have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| documentation -}
type CustomType = A
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectNoErrors
        , test "should report an error when a custom type's documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| -}
type CustomType = A
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation for `CustomType` is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "{-| -}"
                            }
                        ]
        , test "should report an error when a type alias does not have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
type alias Alias = A
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing documentation for `Alias`"
                            , details = missingElementDetails
                            , under = "Alias"
                            }
                        ]
        , test "should not report an error when a type alias does have documentation" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| documentation -}
type alias Alias = A
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectNoErrors
        , test "should report an error when a type alias' documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| module documentation -}
import Thing
{-| -}
type alias Alias = A
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation for `Alias` is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "{-| -}"
                            }
                        ]
        , test "should report an error when a module does not have documentation" <|
            \() ->
                """module A exposing (..)
import Thing
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing documentation for module `A`"
                            , details = missingModuleDetails
                            , under = "A"
                            }
                        ]
        , test "should not report an error when a module does have documentation" <|
            \() ->
                """module A exposing (..)
{-| documentation -}
import Thing
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectNoErrors
        , test "should report an error when the module's documentation is empty" <|
            \() ->
                """module A exposing (..)
{-| -}
import Thing
"""
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The documentation for module `A` is empty"
                            , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                            , under = "{-| -}"
                            }
                        ]
        ]


everythingFromExposedModulesTests : Test
everythingFromExposedModulesTests =
    let
        config : { document : Docs.NoMissing.What, from : Docs.NoMissing.From }
        config =
            { document = Docs.NoMissing.everything
            , from = Docs.NoMissing.exposedModules
            }
    in
    describe "document everything - from exposed modules"
        [ test "should not report things from non-exposed modules for a package" <|
            \() ->
                """module NotExposed exposing (..)
import Thing
"""
                    |> Review.Test.runWithProjectData packageProject (rule config)
                    |> Review.Test.expectNoErrors
        , test "should report things from exposed modules for a package" <|
            \() ->
                """module Exposed exposing (..)
import Thing
"""
                    |> Review.Test.runWithProjectData packageProject (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing documentation for module `Exposed`"
                            , details = missingModuleDetails
                            , under = "Exposed"
                            }
                        ]
        ]


onlyExposedFromExposedModulesTests : Test
onlyExposedFromExposedModulesTests =
    let
        config : { document : Docs.NoMissing.What, from : Docs.NoMissing.From }
        config =
            { document = Docs.NoMissing.onlyExposed
            , from = Docs.NoMissing.exposedModules
            }
    in
    describe "document only exposed - from exposed modules"
        [ test "should not report non-exposed elements from exposed modules" <|
            \() ->
                """module Exposed exposing (a)

{-| module
-}
import Thing

{-| a
-}
a : ()
a = ()

b = ()
"""
                    |> Review.Test.runWithProjectData packageProject (rule config)
                    |> Review.Test.expectNoErrors
        , test "should report exposed elements from exposed modules, using exposing everything" <|
            \() ->
                """module Exposed exposing (..)
import Thing
function = 1
type CustomType = Variant
type alias Alias = A
"""
                    |> Review.Test.runWithProjectData packageProject (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing documentation for module `Exposed`"
                            , details = missingModuleDetails
                            , under = "Exposed"
                            }
                        , Review.Test.error
                            { message = "Missing documentation for `function`"
                            , details = missingElementDetails
                            , under = "function"
                            }
                        , Review.Test.error
                            { message = "Missing documentation for `CustomType`"
                            , details = missingElementDetails
                            , under = "CustomType"
                            }
                        , Review.Test.error
                            { message = "Missing documentation for `Alias`"
                            , details = missingElementDetails
                            , under = "Alias"
                            }
                        ]
        , test "should report exposed elements from exposed modules, using explicit exposing" <|
            \() ->
                """module Exposed exposing (function, CustomType, Alias)
import Thing
function = 1
type CustomType = Variant
type alias Alias = A
"""
                    |> Review.Test.runWithProjectData packageProject (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing documentation for module `Exposed`"
                            , details = missingModuleDetails
                            , under = "Exposed"
                            }
                        , Review.Test.error
                            { message = "Missing documentation for `function`"
                            , details = missingElementDetails
                            , under = "function"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } }
                        , Review.Test.error
                            { message = "Missing documentation for `CustomType`"
                            , details = missingElementDetails
                            , under = "CustomType"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 16 } }
                        , Review.Test.error
                            { message = "Missing documentation for `Alias`"
                            , details = missingElementDetails
                            , under = "Alias"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 12 }, end = { row = 5, column = 17 } }
                        ]
        ]


packageProject : Project
packageProject =
    Project.new
        |> Project.addElmJson (createElmJson packageElmJson)


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
        "elm/core": "1.0.0 <= v < 2.0.0"
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

        Err _ ->
            Debug.todo "Invalid elm.json supplied to test"
