module Tests.NoForbiddenWords exposing (all)

import Elm.Project
import Json.Decode as Decode
import Json.Encode as Encode
import NoForbiddenWords exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Rule
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoForbiddenWords"
        [ test "with no words reports nothing" <|
            \() ->
                """
module A exposing (..)
-- TODO: Write main
main = Debug.todo ""
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "reports any found words" <|
            \() ->
                """
module A exposing (..)
-- TODO: Write main
main = Debug.todo ""
"""
                    |> Review.Test.run (rule [ "TODO" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "TODO"
                        ]
        , test "reports any found words in multi-line comments" <|
            \() ->
                """
module A exposing (..)
{- The entry point for our project.

TODO: Write main

-}
main = Debug.todo ""
"""
                    |> Review.Test.run (rule [ "TODO" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "TODO"
                        ]
        , test "reports `-- [ ]` as `- [ ]`" <|
            \() ->
                """
module A exposing (..)
-- [ ] Documentation
main = Debug.todo ""
"""
                    |> Review.Test.run (rule [ "- [ ]" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "- [ ]"
                        ]
        , test "reports forbidden words in module documentation" <|
            \() ->
                """
module A exposing (..)
{-| Module A

TODO: Write the documentation
-}
import Foo

main = Debug.todo ""
"""
                    |> Review.Test.run (rule [ "TODO" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "TODO"
                        ]
        , test "reports forbidden words in function documentation" <|
            \() ->
                """
module A exposing (..)
{-| Module A
-}
import Foo

{-| Main

TODO: Write the documentation
-}
main = Debug.todo ""
"""
                    |> Review.Test.run (rule [ "TODO" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "TODO"
                        ]
        , test "reports forbidden words in type documentation" <|
            \() ->
                """
module A exposing (..)
import Foo

{-| Page

TODO: Add more pages
-}
type Page
    = Page

main = Debug.todo ""
"""
                    |> Review.Test.run (rule [ "TODO" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "TODO"
                        ]
        , test "reports forbidden words in type alias documentation" <|
            \() ->
                """
module A exposing (..)
import Foo

{-| Page

TODO: Add footer
-}
type alias Page =
    { title : String, body : List (Html msg) }

main = Debug.todo ""
"""
                    |> Review.Test.run (rule [ "TODO" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "TODO"
                        ]
        , test "reports forbidden words in port documentation" <|
            \() ->
                """
port module Ports exposing (..)
import Foo

{-| Save

TODO: Use Json.Encode.Value here.
-}
port save : String -> Cmd Msg
"""
                    |> Review.Test.run (rule [ "TODO" ])
                    |> Review.Test.expectErrors
                        [ forbiddenWordError "TODO"
                        ]
        , test "checks forbidden words in README.md" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addReadme
                                { path = "README.md"
                                , content = """
# My Awesome Project

TODO: Write the readme
"""
                                }
                in
                """
module A exposing (..)
a = 1"""
                    |> Review.Test.runWithProjectData project (rule [ "TODO" ])
                    |> Review.Test.expectErrorsForReadme
                        [ forbiddenWordErrorForReadme "TODO"
                        ]
        , test "forbidden words in README.md can be ignored" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addReadme
                                { path = "README.md"
                                , content = """
# My Awesome Project

TODO: Write the readme
"""
                                }
                in
                """
module A exposing (..)
a = 1"""
                    |> Review.Test.runWithProjectData project
                        (rule [ "TODO" ]
                            |> Review.Rule.ignoreErrorsForFiles [ "README.md" ]
                        )
                    |> Review.Test.expectNoErrors
        , test "checks elm.json summary for forbidden words" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addElmJson
                                (packageElmJson
                                    { summary = "REPLACEME"
                                    , formatted = True
                                    }
                                )
                in
                """
module A exposing (..)
a = 1"""
                    |> Review.Test.runWithProjectData project (rule [ "REPLACEME" ])
                    |> Review.Test.expectErrorsForElmJson
                        [ forbiddenWordErrorForElmJson "REPLACEME"
                        ]
        , test "checks minified elm.json summary for forbidden words" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addElmJson
                                (packageElmJson
                                    { summary = "REPLACEME"
                                    , formatted = False
                                    }
                                )
                in
                """
module A exposing (..)
a = 1"""
                    |> Review.Test.runWithProjectData project (rule [ "REPLACEME" ])
                    |> Review.Test.expectErrorsForElmJson
                        [ forbiddenWordErrorForElmJson "REPLACEME"
                        ]
        ]


packageElmJson : { summary : String, formatted : Bool } -> { path : String, raw : String, project : Elm.Project.Project }
packageElmJson { summary, formatted } =
    let
        spaces : number
        spaces =
            if formatted then
                4

            else
                0
    in
    Encode.encode spaces
        (Encode.object
            [ ( "type", Encode.string "package" )
            , ( "name", Encode.string "sparksp/elm-review-new-package" )
            , ( "summary", Encode.string summary )
            , ( "license", Encode.string "MIT" )
            , ( "version", Encode.string "1.0.0" )
            , ( "exposed-modules", Encode.list Encode.string [ "NoNewPackage" ] )
            , ( "elm-version", Encode.string "0.19.0 <= v < 0.20.0" )
            , ( "dependencies"
              , Encode.object
                    [ ( "elm/code", Encode.string "1.0.5 <= v <= 2.0.0" )
                    ]
              )
            , ( "test-dependencies", Encode.object [] )
            ]
        )
        |> createElmJson


forbiddenWordError : String -> Review.Test.ExpectedError
forbiddenWordError word =
    Review.Test.error
        { message = "`" ++ word ++ "` is not allowed in comments."
        , details =
            [ "You should review this comment and make sure the forbidden word has been removed before publishing your code."
            ]
        , under = word
        }


forbiddenWordErrorForReadme : String -> Review.Test.ExpectedError
forbiddenWordErrorForReadme word =
    Review.Test.error
        { message = "`" ++ word ++ "` is not allowed in README file."
        , details =
            [ "You should review this section and make sure the forbidden word has been removed before publishing your code."
            ]
        , under = word
        }


forbiddenWordErrorForElmJson : String -> Review.Test.ExpectedError
forbiddenWordErrorForElmJson word =
    Review.Test.error
        { message = "`" ++ word ++ "` is not allowed in elm.json summary."
        , details =
            [ "You should review your elm.json and make sure the forbidden word has been removed before publishing your code."
            ]
        , under = word
        }



--- HELPERS


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    { path = "elm.json"
    , raw = rawElmJson
    , project = createElmJsonProject rawElmJson
    }


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

        Err error ->
            Debug.todo ("[elm.json]: " ++ Debug.toString error)
