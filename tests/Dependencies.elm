module Dependencies exposing (elmCore, elmHtml)

import Elm.Docs
import Elm.Type as Type


elmCore : { packageName : String, modules : List Elm.Docs.Module }
elmCore =
    { packageName = "elm/html"
    , modules =
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
        ]
    }


elmHtml : { packageName : String, modules : List Elm.Docs.Module }
elmHtml =
    { packageName = "elm/html"
    , modules =
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
    }
