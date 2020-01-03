module NoUnusedModulesTest exposing (all)

import Dependencies
import Elm.Project
import Elm.Type as Type
import Elm.Version
import NoUnusedModules exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


application : Project
application =
    Project.new
        |> Project.withElmJson applicationElmJson


applicationElmJson : Elm.Project.Project
applicationElmJson =
    Elm.Project.Application
        { elm = Elm.Version.one
        , dirs = []
        , depsDirect = []
        , depsIndirect = []
        , testDepsDirect = []
        , testDepsIndirect = []
        }


details : List String
details =
    [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
    ]


tests : List Test
tests =
    [ test "should not report a module when all modules are used" <|
        \() ->
            [ """
module A exposing (..)
import B
main = text ""
"""
            , """
module B exposing (..)
"""
            ]
                |> Review.Test.runMultiWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should report a module when it is never used" <|
        \() ->
            [ """
module A exposing (..)
main = text ""
"""
            , """
module B exposing (..)
"""
            ]
                |> Review.Test.runMultiWithProjectData application rule
                |> Review.Test.expectErrorsForFiles
                    [ []
                    , [ Review.Test.error
                            { message = "Module `B` is never used."
                            , details = details
                            , under = "B"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 8 }, end = { row = 2, column = 9 } }
                      ]
                    ]
    ]


all : Test
all =
    describe "NoUnusedModules" tests
