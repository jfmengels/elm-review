module Simplify.JsonEncodeTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    Test.describe "Json.Encode"
        [ jsonEncodeArrayTests
        , jsonEncodeListTests
        , jsonEncodeSetTests
        ]


jsonEncodeArrayTests : Test
jsonEncodeArrayTests =
    describe "Json.Encode.array"
        [ test "should not report Json.Encode.array used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a0 = Json.Encode.array
a1 = Json.Encode.array f
a2 = Json.Encode.array f array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Encode.array f (Array.fromList list) by Json.Encode.list f list" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.array f (Array.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a list, you don't need to convert to an array"
                            , details = [ "Using Json.Encode.list directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.list f list
"""
                        ]
        , test "should replace Json.Encode.array f << Array.fromList by Json.Encode.list f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.array f << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a list, you don't need to convert to an array"
                            , details = [ "Using Json.Encode.list directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.list f
"""
                        ]
        , test "should replace Array.fromList >> Json.Encode.array f by Json.Encode.list f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Array.fromList >> Json.Encode.array f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a list, you don't need to convert to an array"
                            , details = [ "Using Json.Encode.list directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.list f
"""
                        ]
        , test "should replace Json.Encode.array identity (Array.map f array) by Json.Encode.array f array" <|
            \() ->
                """module A exposing (..)
import Array
import Json.Encode
a = Json.Encode.array identity (Array.map f array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map, then Json.Encode.array with an identity function can be combined into Json.Encode.array"
                            , details = [ "You can replace this call by Json.Encode.array with the same arguments given to Array.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
import Json.Encode
a = (Json.Encode.array f array)
"""
                        ]
        , test "should replace Json.Encode.array identity << Array.map f by Json.Encode.array f" <|
            \() ->
                """module A exposing (..)
import Array
import Json.Encode
a = Json.Encode.array identity << Array.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map, then Json.Encode.array with an identity function can be combined into Json.Encode.array"
                            , details = [ "You can replace this composition by Json.Encode.array with the same argument given to Array.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
import Json.Encode
a = Json.Encode.array f
"""
                        ]
        , test "should replace Array.map f >> Json.Encode.array identity by Json.Encode.array f" <|
            \() ->
                """module A exposing (..)
import Array
import Json.Encode
a = Array.map f >> Json.Encode.array identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map, then Json.Encode.array with an identity function can be combined into Json.Encode.array"
                            , details = [ "You can replace this composition by Json.Encode.array with the same argument given to Array.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
import Json.Encode
a = Json.Encode.array f
"""
                        ]
        ]


jsonEncodeListTests : Test
jsonEncodeListTests =
    describe "Json.Encode.list"
        [ test "should not report Json.Encode.list used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a0 = Json.Encode.list
a1 = Json.Encode.list f
a2 = Json.Encode.list f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Encode.list f (Array.toList array) by Json.Encode.array f list" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f (Array.toList array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode an array, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.array directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.array f array
"""
                        ]
        , test "should replace Json.Encode.list f << Array.toList by Json.Encode.array f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode an array, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.array directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.array f
"""
                        ]
        , test "should replace Array.toList >> Json.Encode.list f by Json.Encode.array f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Array.toList >> Json.Encode.list f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode an array, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.array directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.array f
"""
                        ]
        , test "should replace Json.Encode.list f (Set.toList set) by Json.Encode.set f set" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a set, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.set directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.set f set
"""
                        ]
        , test "should replace Json.Encode.list f << Set.toList by Json.Encode.set f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a set, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.set directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.set f
"""
                        ]
        , test "should replace Set.toList >> Json.Encode.list f by Json.Encode.set f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Set.toList >> Json.Encode.list f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a set, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.set directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.set f
"""
                        ]
        , test "should replace Json.Encode.list identity (List.map f list) by Json.Encode.list f list" <|
            \() ->
                """module A exposing (..)
import List
import Json.Encode
a = Json.Encode.list identity (List.map f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then Json.Encode.list with an identity function can be combined into Json.Encode.list"
                            , details = [ "You can replace this call by Json.Encode.list with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import List
import Json.Encode
a = (Json.Encode.list f list)
"""
                        ]
        , test "should replace Json.Encode.list identity << List.map f by Json.Encode.list f" <|
            \() ->
                """module A exposing (..)
import List
import Json.Encode
a = Json.Encode.list identity << List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then Json.Encode.list with an identity function can be combined into Json.Encode.list"
                            , details = [ "You can replace this composition by Json.Encode.list with the same argument given to List.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import List
import Json.Encode
a = Json.Encode.list f
"""
                        ]
        , test "should replace List.map f >> Json.Encode.list identity by Json.Encode.list f" <|
            \() ->
                """module A exposing (..)
import List
import Json.Encode
a = List.map f >> Json.Encode.list identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then Json.Encode.list with an identity function can be combined into Json.Encode.list"
                            , details = [ "You can replace this composition by Json.Encode.list with the same argument given to List.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import List
import Json.Encode
a = Json.Encode.list f
"""
                        ]
        ]


jsonEncodeSetTests : Test
jsonEncodeSetTests =
    describe "Json.Encode.set"
        [ test "should not report Json.Encode.set used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a0 = Json.Encode.set
a1 = Json.Encode.set f
a2 = Json.Encode.set f set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Encode.set identity (Set.map f set) by Json.Encode.set f set" <|
            \() ->
                """module A exposing (..)
import Set
import Json.Encode
a = Json.Encode.set identity (Set.map f set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map, then Json.Encode.set with an identity function can be combined into Json.Encode.set"
                            , details = [ "You can replace this call by Json.Encode.set with the same arguments given to Set.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
import Json.Encode
a = (Json.Encode.set f set)
"""
                        ]
        , test "should replace Json.Encode.set identity << Set.map f by Json.Encode.set f" <|
            \() ->
                """module A exposing (..)
import Set
import Json.Encode
a = Json.Encode.set identity << Set.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map, then Json.Encode.set with an identity function can be combined into Json.Encode.set"
                            , details = [ "You can replace this composition by Json.Encode.set with the same argument given to Set.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
import Json.Encode
a = Json.Encode.set f
"""
                        ]
        , test "should replace Set.map f >> Json.Encode.set identity by Json.Encode.set f" <|
            \() ->
                """module A exposing (..)
import Set
import Json.Encode
a = Set.map f >> Json.Encode.set identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map, then Json.Encode.set with an identity function can be combined into Json.Encode.set"
                            , details = [ "You can replace this composition by Json.Encode.set with the same argument given to Set.map which is meant for this exact purpose." ]
                            , under = "Json.Encode.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
import Json.Encode
a = Json.Encode.set f
"""
                        ]
        ]
