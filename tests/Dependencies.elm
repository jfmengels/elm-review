module Dependencies exposing (elmHtml)

import Elm.Docs
import Elm.Type as Type


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
