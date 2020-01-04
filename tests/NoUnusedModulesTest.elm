module NoUnusedModulesTest exposing (all)

import Dependencies
import Elm.Constraint
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type as Type
import Elm.Version
import Expect
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


package_ : Project
package_ =
    Project.new
        |> Project.withElmJson (createPackageElmJson ())


createPackageElmJson : () -> Elm.Project.Project
createPackageElmJson _ =
    case ( Elm.Package.fromString "author/package", Elm.Constraint.fromString "1.0.0 <= v < 2.0.0", Elm.Module.fromString "Exposed" ) of
        ( Just name, Just elm, Just moduleName ) ->
            Elm.Project.Package
                { name = name
                , summary = "Summary"
                , license = Elm.License.bsd3
                , version = Elm.Version.one
                , exposed = Elm.Project.ExposedList [ moduleName ]
                , deps = []
                , testDeps = []
                , elm = elm
                }

        _ ->
            createPackageElmJson ()


details : List String
details =
    [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
    ]


tests : List Test
tests =
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
                |> Review.Test.runMultiWithProjectData application rule
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
            ]
                |> Review.Test.runMultiWithProjectData application rule
                |> Review.Test.expectErrorsForFiles
                    [ []
                    , [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = details
                            , under = "Reported"
                            }
                      ]
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
                        , details = details
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
    , test "should not report an application module if it contains a main function even if it is not exposed" <|
        \() ->
            """
module NotReported exposing (a)
main = text ""
a = 1
"""
                |> Review.Test.runWithProjectData application rule
                |> Review.Test.expectNoErrors
    , test "should not report a module with main function if we don't know the project type" <|
        \() ->
            """
module NotReported exposing (a)
main = text ""
a = 1
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
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectNoErrors
    , test "should report non-exposed and non-used modules from a package" <|
        \() ->
            """
module NotExposed exposing (..)
a = 1
"""
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `NotExposed` is never used."
                        , details = details
                        , under = "NotExposed"
                        }
                    ]
    , test "should report non-exposed and non-used package modules that expose a `main` function" <|
        \() ->
            """
module Reported exposing (main)
main = text ""
"""
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `Reported` is never used."
                        , details = details
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
                |> Review.Test.runWithProjectData package_ rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `Reported` is never used."
                        , details = details
                        , under = "Reported"
                        }
                    ]
    ]


all : Test
all =
    describe "NoUnusedModules" tests
