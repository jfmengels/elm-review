module NoExposingEverythingTest exposing (all)

import NoExposingEverything exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoExposingEverything"
        [ test "should not report anything when a module exposes a limited set of things" <|
            \() ->
                """
module A exposing (B(..), C, d)
type B = B
d = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should offer a fix listing all variables" <|
            \() ->
                """
module A exposing (..)
foo = 1
bar = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module exposes everything implicitly \"(..)\""
                            , details =
                                [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                                ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed
                                """
module A exposing (foo, bar)
foo = 1
bar = 2
"""
                        ]
        , test "should offer a fix listing all types" <|
            \() ->
                """
module A exposing (..)
type Foo = Foo
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module exposes everything implicitly \"(..)\""
                            , details =
                                [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                                ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed
                                """
module A exposing (Foo(..))
type Foo = Foo
"""
                        ]
        , test "should offer a fix listing all type aliases" <|
            \() ->
                """
module A exposing (..)
type alias Foo = String
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module exposes everything implicitly \"(..)\""
                            , details =
                                [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                                ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed
                                """
module A exposing (Foo)
type alias Foo = String
"""
                        ]
        , test "should offer a fix listing all ports" <|
            \() ->
                """
port module A exposing (..)
port foo : String
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module exposes everything implicitly \"(..)\""
                            , details =
                                [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                                ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed
                                """
port module A exposing (foo)
port foo : String
"""
                        ]
        , test "should offer a fix listing all infixes" <|
            \() ->
                """
module List exposing (..)
import Elm.Kernel.List
infix right 5 (::) = cons
cons : a -> List a -> List a
cons =
  Elm.Kernel.List.cons
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module exposes everything implicitly \"(..)\""
                            , details =
                                [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                                ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed
                                """
module List exposing ((::), cons)
import Elm.Kernel.List
infix right 5 (::) = cons
cons : a -> List a -> List a
cons =
  Elm.Kernel.List.cons
"""
                        ]
        ]
