module TypeInference.TypeTest exposing (suite)

import Elm.Type
import Expect
import Test exposing (Test, test)
import TypeInference.Type as Type


suite : Test
suite =
    Test.describe "TypeInference.Type"
        [ fromMetadataType
        , toMetadataType
        ]


fromMetadataType : Test
fromMetadataType =
    Test.describe "TypeInference.Type.fromMetadataType"
        [ Test.test "generic variable" <|
            \() ->
                Elm.Type.Var "a"
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Generic "a")
        , Test.test "Function" <|
            \() ->
                Elm.Type.Lambda (Elm.Type.Var "a") (Elm.Type.Var "b")
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Function (Type.Generic "a") (Type.Generic "b"))
        , Test.test "Function with multiple arguments" <|
            \() ->
                Elm.Type.Lambda
                    (Elm.Type.Var "a")
                    (Elm.Type.Lambda (Elm.Type.Var "b") (Elm.Type.Var "c"))
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Function
                            (Type.Generic "a")
                            (Type.Function (Type.Generic "b") (Type.Generic "c"))
                        )
        , Test.test "Unit" <|
            \() ->
                Elm.Type.Tuple []
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Tuple [])
        , Test.test "Tuple" <|
            \() ->
                Elm.Type.Tuple [ Elm.Type.Var "b", Elm.Type.Var "c" ]
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Tuple [ Type.Generic "b", Type.Generic "c" ])
        , Test.test "Tuple with 3 elements" <|
            \() ->
                Elm.Type.Tuple [ Elm.Type.Var "b", Elm.Type.Var "c", Elm.Type.Var "d" ]
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Tuple [ Type.Generic "b", Type.Generic "c", Type.Generic "d" ])
        , Test.test "Type with an empty module name" <|
            \() ->
                Elm.Type.Type "Int" []
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Type [] "Int" [])
        , Test.test "Type with a module name" <|
            \() ->
                Elm.Type.Type "Basics.Some.Thing.Int" []
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Type [ "Basics", "Some", "Thing" ] "Int" [])
        , Test.test "Type with an argument" <|
            \() ->
                Elm.Type.Type "Basics.List" [ Elm.Type.Type "Basics.Int" [] ]
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Type [ "Basics" ] "List" [ Type.Type [ "Basics" ] "Int" [] ])
        , Test.test "Empty record" <|
            \() ->
                Elm.Type.Record [] Nothing
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Record
                            { generic = Nothing
                            , fields = []
                            , canHaveMoreFields = False
                            }
                        )
        , Test.test "Record with a field" <|
            \() ->
                Elm.Type.Record [ ( "name", Elm.Type.Type "Basics.String" [] ) ] Nothing
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Record
                            { generic = Nothing
                            , fields = [ ( "name", Type.Type [ "Basics" ] "String" [] ) ]
                            , canHaveMoreFields = False
                            }
                        )
        , Test.test "Record with a generic field" <|
            \() ->
                Elm.Type.Record [ ( "name", Elm.Type.Type "Basics.String" [] ) ] (Just "a")
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Record
                            { generic = Just "a"
                            , fields = [ ( "name", Type.Type [ "Basics" ] "String" [] ) ]
                            , canHaveMoreFields = False
                            }
                        )
        ]


toMetadataType : Test
toMetadataType =
    Test.describe "fromMetadataType"
        [ Test.test "Unknown" <|
            \() ->
                Type.Unknown
                    |> Type.toMetadataType
                    |> Expect.equal Nothing
        , Test.test "generic variable" <|
            \() ->
                Type.Generic "a"
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Var "a"))
        , Test.test "Unit" <|
            \() ->
                Type.Tuple []
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Tuple []))
        , Test.test "Tuple with only known elements" <|
            \() ->
                Type.Tuple [ Type.Generic "a", Type.Generic "b" ]
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Tuple [ Elm.Type.Var "a", Elm.Type.Var "b" ]))
        , Test.test "Tuple with unknown elements" <|
            \() ->
                Type.Tuple [ Type.Generic "a", Type.Unknown, Type.Generic "b" ]
                    |> Type.toMetadataType
                    |> Expect.equal Nothing
        , Test.test "Function with only known elements" <|
            \() ->
                Type.Function (Type.Generic "a") (Type.Generic "b")
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Lambda (Elm.Type.Var "a") (Elm.Type.Var "b")))
        , Test.test "Function with multiple arguments" <|
            \() ->
                Type.Function (Type.Generic "a") (Type.Function (Type.Generic "b") (Type.Generic "c"))
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Lambda (Elm.Type.Var "a") (Elm.Type.Lambda (Elm.Type.Var "b") (Elm.Type.Var "c"))))
        , Test.test "Function with unknown elements" <|
            \() ->
                Type.Function (Type.Generic "a") (Type.Function Type.Unknown (Type.Generic "c"))
                    |> Type.toMetadataType
                    |> Expect.equal Nothing
        , Test.test "Type with an empty module name" <|
            \() ->
                Type.Type [] "functionName" []
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Type "functionName" []))
        , Test.test "Type with a module name" <|
            \() ->
                Type.Type [ "Some", "Thing" ] "functionName" []
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Type "Some.Thing.functionName" []))
        , Test.test "Type with an argument" <|
            \() ->
                Type.Type [ "Some", "Thing" ] "functionName" [ Type.Generic "a" ]
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Type "Some.Thing.functionName" [ Elm.Type.Var "a" ]))
        , Test.test "Type with an unknown argument" <|
            \() ->
                Type.Type [ "Some", "Thing" ] "functionName" [ Type.Unknown ]
                    |> Type.toMetadataType
                    |> Expect.equal Nothing
        , Test.test "Empty record" <|
            \() ->
                Type.Record
                    { fields = []
                    , generic = Nothing
                    , canHaveMoreFields = False
                    }
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Record [] Nothing))
        , Test.test "Record with some fields" <|
            \() ->
                Type.Record
                    { fields = [ ( "name", Type.Type [] "String" [] ) ]
                    , generic = Nothing
                    , canHaveMoreFields = False
                    }
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Record [ ( "name", Elm.Type.Type "String" [] ) ] Nothing))
        , Test.test "Record with unknown fields" <|
            \() ->
                Type.Record
                    { fields = [ ( "name", Type.Unknown ) ]
                    , generic = Nothing
                    , canHaveMoreFields = False
                    }
                    |> Type.toMetadataType
                    |> Expect.equal Nothing
        , Test.test "Record with generic" <|
            \() ->
                Type.Record
                    { fields = [ ( "name", Type.Type [] "String" [] ) ]
                    , generic = Just "a"
                    , canHaveMoreFields = False
                    }
                    |> Type.toMetadataType
                    |> Expect.equal (Just (Elm.Type.Record [ ( "name", Elm.Type.Type "String" [] ) ] (Just "a")))
        , Test.test "Record with potentially more fields" <|
            \() ->
                Type.Record
                    { fields = [ ( "name", Type.Type [] "String" [] ) ]
                    , generic = Just "a"
                    , canHaveMoreFields = True
                    }
                    |> Type.toMetadataType
                    |> Expect.equal Nothing
        ]
