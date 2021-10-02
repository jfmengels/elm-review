module Docs.ReviewAtDocsTest exposing (all)

import Docs.ReviewAtDocs exposing (rule)
import Elm.Project
import Json.Decode
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Docs.ReviewAtDocs"
        [ test "should not report an error when all @docs are correct" <|
            \() ->
                """module A exposing (D, T, a, b, c)

{-| Bla bla

@docs T, a, b
@docs c, D
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when an element has a @docs reference but function is not exposed" <|
            \() ->
                """module A exposing (a, b)

{-| Bla bla
@docs a, b, notExposed
-}
import B
a = 1
b = 2
notExposed = 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs reference for non-exposed `notExposed`"
                            , details =
                                [ "I couldn't find this element among the module's exposed elements. Maybe you removed or renamed it recently."
                                , "Please remove the @docs reference or update the reference to the new name."
                                ]
                            , under = "notExposed"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 13 }, end = { row = 4, column = 23 } }
                        ]
        , test "should report an error when an element has a @docs reference but type is not exposed" <|
            \() ->
                """module A exposing (a, b)

{-| Bla bla
@docs a, b, NotExposed
-}
import B
a = 1
b = 2
type NotExposed = NotExposed
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs reference for non-exposed `NotExposed`"
                            , details =
                                [ "I couldn't find this element among the module's exposed elements. Maybe you removed or renamed it recently."
                                , "Please remove the @docs reference or update the reference to the new name."
                                ]
                            , under = "NotExposed"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 13 }, end = { row = 4, column = 23 } }
                        ]
        , test "should not report an error when an element has a @docs reference and is exposed with exposing (..)" <|
            \() ->
                """module A exposing (..)

{-| Bla bla
@docs a, b, Exposed
-}
import B
a = 1
b = 2
type Exposed = Exposed
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when an unknown element has a @docs reference, with exposing (..)" <|
            \() ->
                """module A exposing (..)

{-| Bla bla
@docs a, b, Exposed
-}
import B
a = 1
b = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs reference for non-exposed `Exposed`"
                            , details =
                                [ "I couldn't find this element among the module's exposed elements. Maybe you removed or renamed it recently."
                                , "Please remove the @docs reference or update the reference to the new name."
                                ]
                            , under = "Exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 13 }, end = { row = 4, column = 20 } }
                        ]
        , test "should report an error when encountering @docs on the first line of the module documentation (without space)" <|
            \() ->
                """module A exposing (a)

{-|@docs a
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs on the first line"
                            , details = [ "Using @docs on the first line will make for a broken documentation once published. Please move it to the beginning of the next line." ]
                            , under = "@docs"
                            }
                        ]
        , test "should report an error when encountering @docs on the first line of the module documentation (with space)" <|
            \() ->
                """module A exposing (a)

{-| @docs a
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found @docs on the first line"
                            , details = [ "Using @docs on the first line will make for a broken documentation once published. Please move it to the beginning of the next line." ]
                            , under = "@docs"
                            }
                        ]
        , test "should report an error when encountering indented @docs" <|
            \() ->
                """module A exposing (a)

{-|
    @docs a
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found indented @docs"
                            , details = [ "@docs need to be at the beginning of a line, otherwise they can lead to broken documentation once published. on the first line will make for a broken documentation once published. Please remove the leading spaces" ]
                            , under = "@docs"
                            }
                        ]
        , test "should report an error when an element is exposed but has no @docs reference" <|
            \() ->
                """module A exposing (a, b, exposed)

{-| Bla bla
@docs a, b
-}
import B
a = 1
b = 2
exposed = 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing @docs reference for exposed `exposed`"
                            , details =
                                [ "There is no @docs reference for this element. Maybe you exposed or renamed it recently."
                                , "Please add a @docs reference to it the module documentation (the one at the top of the module) like this:"
                                , """{-|
@docs exposed
-}"""
                                ]
                            , under = "exposed"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 26 }, end = { row = 1, column = 33 } }
                        ]
        , test "should not report errors when there is no @docs at all" <|
            \() ->
                """module A exposing (a)

{-| Bla bla
-}
import B
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report errors for exposed modules of a package even if there are no @docs at all" <|
            \() ->
                """module Exposed exposing (element)

{-| Bla bla
-}
import B
element = 1
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing @docs reference for exposed `element`"
                            , details =
                                [ "There is no @docs reference for this element. Maybe you exposed or renamed it recently."
                                , "Please add a @docs reference to it the module documentation (the one at the top of the module) like this:"
                                , """{-|
@docs element
-}"""
                                ]
                            , under = "element"
                            }
                            |> Review.Test.atExactly { start = { row = 1, column = 26 }, end = { row = 1, column = 33 } }
                        ]
        , test "should report errors for duplicate docs" <|
            \() ->
                """module Exposed exposing (something, element)

{-|
@docs element
@docs something, element
-}
import B
element = 1
something = 2
"""
                    |> Review.Test.runWithProjectData package rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found duplicate @docs reference for `element`"
                            , details = [ "An element should only be referenced once, but I found a previous reference to it on line 4. Please remove one of them." ]
                            , under = "element"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 18 }, end = { row = 5, column = 25 } }
                        ]
        , test "should report errors for usage of @docs in function documentation" <|
            \() ->
                """module A exposing (something, element)
{-|
@docs something, element
-}
import B
{-|
@docs something
-}
element = 1
something = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found usage of @docs in a function documentation"
                            , details = [ "@docs can only be used in the module's documentation. You should remove this @docs and move it there." ]
                            , under = "@docs"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 6 } }
                        ]
        , test "should report errors for usage of @docs in function documentation even if there are no @docs in the module documentation" <|
            \() ->
                """module A exposing (something, element)
import B
{-|
@docs something
-}
element = 1
something = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found usage of @docs in a function documentation"
                            , details = [ "@docs can only be used in the module's documentation. You should remove this @docs and move it there." ]
                            , under = "@docs"
                            }
                        ]
        , test "should report errors for usage of @docs in type alias documentation" <|
            \() ->
                """module A exposing (something, Element)
{-|
@docs something, Element
-}
import B
{-|
@docs something
-}
type alias Element = {}
something = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found usage of @docs in a type documentation"
                            , details = [ "@docs can only be used in the module's documentation. You should remove this @docs and move it there." ]
                            , under = "@docs"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 1 }, end = { row = 7, column = 6 } }
                        ]
        , test "should report errors for usage of @docs in custom type documentation" <|
            \() ->
                """module A exposing (something, Element)
{-|
@docs something, Element
-}
import B
{-|
Bla bla bla

@docs something
-}
type Element = Element
something = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found usage of @docs in a type documentation"
                            , details = [ "@docs can only be used in the module's documentation. You should remove this @docs and move it there." ]
                            , under = "@docs"
                            }
                            |> Review.Test.atExactly { start = { row = 9, column = 1 }, end = { row = 9, column = 6 } }
                        ]
        , test "should not report mention of @docs after words" <|
            \() ->
                """module A exposing (a)
{-|
@docs a
-}
import B

{-| Reports problems with the usages of `@docs`.
-}
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


package : Project
package =
    case Json.Decode.decodeString Elm.Project.decoder elmJson of
        Ok project ->
            Project.new
                |> Project.addElmJson
                    { path = "elm.json"
                    , raw = elmJson
                    , project = project
                    }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


elmJson : String
elmJson =
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
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""
