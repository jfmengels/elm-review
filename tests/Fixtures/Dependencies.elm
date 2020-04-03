module Fixtures.Dependencies exposing (elmCore, elmHtml)

import Elm.Docs
import Elm.Project
import Elm.Type as Type
import Json.Decode as Decode
import Review.Project.Dependency as Dependency exposing (Dependency)


createElmJson : String -> Elm.Project.Project
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            elmJson

        Err _ ->
            createElmJson rawElmJson


elmCore : Dependency
elmCore =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "Basics"
              , comment = ""
              , unions =
                    [ { name = "Bool"
                      , comment = ""
                      , args = []
                      , tags = [ ( "True", [] ), ( "False", [] ) ]
                      }
                    ]
              , aliases = []
              , values =
                    [ { name = "always"
                      , comment = ""
                      , tipe =
                            -- b -> a -> b
                            Type.Lambda (Type.Var "b") (Type.Lambda (Type.Var "a") (Type.Var "b"))
                      }
                    ]
              , binops =
                    [ { name = "+"
                      , comment = "Add numbers"
                      , tipe = Type.Lambda (Type.Var "number") (Type.Lambda (Type.Var "number") (Type.Var "number"))
                      , associativity = Elm.Docs.Left
                      , precedence = 6
                      }
                    ]
              }
            , { name = "Maybe"
              , comment = ""
              , unions =
                    [ { name = "Maybe"
                      , comment = " maybe "
                      , args = [ "a" ]
                      , tags =
                            [ ( "Nothing", [] )
                            , ( "Just", [ Type.Var "a" ] )
                            ]
                      }
                    ]
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            createElmJson """
    {
        "type": "package",
        "name": "elm/core",
        "summary": "Elm's standard libraries",
        "license": "BSD-3-Clause",
        "version": "1.0.4",
        "exposed-modules": {
            "Primitives": [
                "Basics",
                "String",
                "Char",
                "Bitwise",
                "Tuple"
            ],
            "Collections": [
                "List",
                "Dict",
                "Set",
                "Array"
            ],
            "Error Handling": [
                "Maybe",
                "Result"
            ],
            "Debug": [
                "Debug"
            ],
            "Effects": [
                "Platform.Cmd",
                "Platform.Sub",
                "Platform",
                "Process",
                "Task"
            ]
        },
        "elm-version": "0.19.0 <= v < 0.20.0",
        "dependencies": {},
        "test-dependencies": {}
    }
    """
    in
    Dependency.create
        "elm/core"
        elmJson
        modules


elmHtml : Dependency
elmHtml =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "Html"
              , comment = ""
              , unions = []
              , aliases = []
              , values =
                    [ { name = "button"
                      , comment = ""
                      , tipe =
                            -- List.List (Html.Attribute msg) ->
                            Type.Lambda (Type.Type "List.List" [ Type.Type "Html.Attribute" [ Type.Var "msg" ] ])
                                --  List.List (Html.Html msg) ->
                                (Type.Lambda (Type.Type "List.List" [ Type.Type "Html.Html" [ Type.Var "msg" ] ])
                                    -- Html.Html msg
                                    (Type.Type "Html.Html" [ Type.Var "msg" ])
                                )
                      }
                    ]
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            createElmJson """
  {
    "type": "package",
    "name": "elm/html",
    "summary": "Fast HTML, rendered with virtual DOM diffing",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": {
      "HTML": [
        "Html",
        "Html.Attributes",
        "Html.Events"
      ],
      "Optimize": [
        "Html.Keyed",
        "Html.Lazy"
      ]
    },
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
      "elm/core": "1.0.0 <= v < 2.0.0",
      "elm/json": "1.0.0 <= v < 2.0.0",
      "elm/virtual-dom": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
  }
  """
    in
    Dependency.create
        "elm/html"
        elmJson
        modules
