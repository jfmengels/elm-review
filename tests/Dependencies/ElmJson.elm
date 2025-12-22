module Dependencies.ElmJson exposing (dependency)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type exposing (Type(..))
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create "elm/json"
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ unsafeModuleName "Json.Decode", unsafeModuleName "Json.Encode" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName "elm/json"
        , summary = "Encode and decode JSON values"
        , deps = [ ( unsafePackageName "elm/core", unsafeConstraint "1.0.0 <= v < 2.0.0" ) ]
        , testDeps = []
        , version = Elm.Version.fromString "1.1.4" |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Json.Decode"
      , comment = ""
      , aliases =
            [ { name = "Value"
              , args = []
              , comment = ""
              , tipe = Type "Json.Encode.Value" []
              }
            ]
      , unions =
            [ { name = "Decoder"
              , args = [ "a" ]
              , comment = ""
              , tags = []
              }
            , { name = "Error"
              , args = []
              , comment = ""
              , tags =
                    [ ( "Field", [ Type "String.String" [], Type "Json.Decode.Error" [] ] )
                    , ( "Index", [ Type "Basics.Int" [], Type "Json.Decode.Error" [] ] )
                    , ( "OneOf", [ Type "List.List" [ Type "Json.Decode.Error" [] ] ] )
                    , ( "Failure", [ Type "String.String" [], Type "Json.Decode.Value" [] ] )
                    ]
              }
            ]
      , binops = []
      , values =
            [ { name = "andThen"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Decode.Decoder" [ Var "b" ])) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "b" ]))
              }
            , { name = "array"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Array.Array" [ Var "a" ] ])
              }
            , { name = "at"
              , comment = ""
              , tipe = Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "a" ]))
              }
            , { name = "bool"
              , comment = ""
              , tipe = Type "Json.Decode.Decoder" [ Type "Basics.Bool" [] ]
              }
            , { name = "decodeString"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "String.String" []) (Type "Result.Result" [ Type "Json.Decode.Error" [], Var "a" ]))
              }
            , { name = "decodeValue"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Value" []) (Type "Result.Result" [ Type "Json.Decode.Error" [], Var "a" ]))
              }
            , { name = "dict"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Dict.Dict" [ Type "String.String" [], Var "a" ] ])
              }
            , { name = "errorToString"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Error" []) (Type "String.String" [])
              }
            , { name = "fail"
              , comment = ""
              , tipe = Lambda (Type "String.String" []) (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "field"
              , comment = ""
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "a" ]))
              }
            , { name = "float"
              , comment = ""
              , tipe = Type "Json.Decode.Decoder" [ Type "Basics.Float" [] ]
              }
            , { name = "index"
              , comment = ""
              , tipe = Lambda (Type "Basics.Int" []) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "a" ]))
              }
            , { name = "int"
              , comment = ""
              , tipe = Type "Json.Decode.Decoder" [ Type "Basics.Int" [] ]
              }
            , { name = "keyValuePairs"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "List.List" [ Tuple [ Type "String.String" [], Var "a" ] ] ])
              }
            , { name = "lazy"
              , comment = ""
              , tipe = Lambda (Lambda (Tuple []) (Type "Json.Decode.Decoder" [ Var "a" ])) (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "list"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "List.List" [ Var "a" ] ])
              }
            , { name = "map"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Var "value")) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))
              }
            , { name = "map2"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Var "value"))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))
              }
            , { name = "map3"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Var "value")))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))))
              }
            , { name = "map4"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Var "value"))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))))
              }
            , { name = "map5"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Var "value")))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))))))
              }
            , { name = "map6"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Var "value"))))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "f" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))))))
              }
            , { name = "map7"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Lambda (Var "g") (Var "value")))))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "f" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "g" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))))))))
              }
            , { name = "map8"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Lambda (Var "g") (Lambda (Var "h") (Var "value"))))))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "f" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "g" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "h" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))))))))
              }
            , { name = "maybe"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Maybe.Maybe" [ Var "a" ] ])
              }
            , { name = "null"
              , comment = ""
              , tipe = Lambda (Var "a") (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "nullable"
              , comment = ""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Maybe.Maybe" [ Var "a" ] ])
              }
            , { name = "oneOf"
              , comment = ""
              , tipe = Lambda (Type "List.List" [ Type "Json.Decode.Decoder" [ Var "a" ] ]) (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "oneOrMore"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Type "List.List" [ Var "a" ]) (Var "value"))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))
              }
            , { name = "string"
              , comment = ""
              , tipe = Type "Json.Decode.Decoder" [ Type "String.String" [] ]
              }
            , { name = "succeed"
              , comment = ""
              , tipe = Lambda (Var "a") (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "value"
              , comment = ""
              , tipe = Type "Json.Decode.Decoder" [ Type "Json.Decode.Value" [] ]
              }
            ]
      }
    , { name = "Json.Encode"
      , comment = ""
      , aliases = []
      , unions =
            [ { name = "Value"
              , args = []
              , comment = ""
              , tags = []
              }
            ]
      , binops = []
      , values =
            [ { name = "array"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Encode.Value" [])) (Lambda (Type "Array.Array" [ Var "a" ]) (Type "Json.Encode.Value" []))
              }
            , { name = "bool"
              , comment = ""
              , tipe = Lambda (Type "Basics.Bool" []) (Type "Json.Encode.Value" [])
              }
            , { name = "dict"
              , comment = ""
              , tipe = Lambda (Lambda (Var "k") (Type "String.String" [])) (Lambda (Lambda (Var "v") (Type "Json.Encode.Value" [])) (Lambda (Type "Dict.Dict" [ Var "k", Var "v" ]) (Type "Json.Encode.Value" [])))
              }
            , { name = "encode"
              , comment = ""
              , tipe = Lambda (Type "Basics.Int" []) (Lambda (Type "Json.Encode.Value" []) (Type "String.String" []))
              }
            , { name = "float"
              , comment = ""
              , tipe = Lambda (Type "Basics.Float" []) (Type "Json.Encode.Value" [])
              }
            , { name = "int"
              , comment = ""
              , tipe = Lambda (Type "Basics.Int" []) (Type "Json.Encode.Value" [])
              }
            , { name = "list"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Encode.Value" [])) (Lambda (Type "List.List" [ Var "a" ]) (Type "Json.Encode.Value" []))
              }
            , { name = "null"
              , comment = ""
              , tipe = Type "Json.Encode.Value" []
              }
            , { name = "object"
              , comment = ""
              , tipe = Lambda (Type "List.List" [ Tuple [ Type "String.String" [], Type "Json.Encode.Value" [] ] ]) (Type "Json.Encode.Value" [])
              }
            , { name = "set"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Encode.Value" [])) (Lambda (Type "Set.Set" [ Var "a" ]) (Type "Json.Encode.Value" []))
              }
            , { name = "string"
              , comment = ""
              , tipe = Lambda (Type "String.String" []) (Type "Json.Encode.Value" [])
              }
            ]
      }
    ]


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            Debug.todo ("Could not parse " ++ packageName ++ " as a package name")


unsafeModuleName : String -> Elm.Module.Name
unsafeModuleName moduleName =
    case Elm.Module.fromString moduleName of
        Just name ->
            name

        Nothing ->
            Debug.todo ("Could not parse " ++ moduleName ++ " as a module name")


unsafeConstraint : String -> Elm.Constraint.Constraint
unsafeConstraint constraint =
    case Elm.Constraint.fromString constraint of
        Just constr ->
            constr

        Nothing ->
            Debug.todo ("Could not parse " ++ constraint ++ " as a package constraint")
