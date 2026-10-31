module Review.Resources.ElmCorePrelude exposing
    ( importAliases
    , moduleNames
    , modulesWithExposingList
    )

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range
import NonEmpty exposing (NonEmpty)


modulesWithExposingList : List { moduleName : ModuleName, exposingList : Exposing }
modulesWithExposingList =
    let
        explicit : List Exposing.TopLevelExpose -> Exposing
        explicit exposed =
            exposed
                |> List.map (Node Range.emptyRange)
                |> Exposing.Explicit
    in
    -- These are the default imports implicitly added by the Elm compiler
    -- https://package.elm-lang.org/packages/elm/core/latest
    [ { moduleName = [ "Basics" ]
      , exposingList = Exposing.All Range.emptyRange
      }
    , { moduleName = [ "List" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "List", open = Nothing }
                , Exposing.InfixExpose "::"
                ]
      }
    , { moduleName = [ "Maybe" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Maybe", open = Just Range.emptyRange }
                ]
      }
    , { moduleName = [ "Result" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Result", open = Just Range.emptyRange }
                ]
      }
    , { moduleName = [ "String" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "String", open = Nothing }
                ]
      }
    , { moduleName = [ "Char" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Char", open = Nothing }
                ]
      }
    , { moduleName = [ "Platform" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Program", open = Nothing }
                ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Cmd", open = Nothing }
                ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Sub", open = Nothing }
                ]
      }
    ]


moduleNames : List ModuleName
moduleNames =
    [ [ "Basics" ]
    , [ "List" ]
    , [ "Maybe" ]
    , [ "Result" ]
    , [ "String" ]
    , [ "Char" ]
    , [ "Tuple" ]
    , [ "Debug" ]
    , [ "Platform" ]
    , [ "Platform", "Cmd" ]
    , [ "Platform", "Sub" ]
    ]


importAliases : Dict String (NonEmpty ModuleName)
importAliases =
    Dict.fromList
        [ ( "Basics", NonEmpty.fromElement [ "Basics" ] )
        , ( "List", NonEmpty.fromElement [ "List" ] )
        , ( "Maybe", NonEmpty.fromElement [ "Maybe" ] )
        , ( "Result", NonEmpty.fromElement [ "Result" ] )
        , ( "String", NonEmpty.fromElement [ "String" ] )
        , ( "Char", NonEmpty.fromElement [ "Char" ] )
        , ( "Tuple", NonEmpty.fromElement [ "Tuple" ] )
        , ( "Debug", NonEmpty.fromElement [ "Debug" ] )
        , ( "Platform", NonEmpty.fromElement [ "Platform" ] )
        , ( "Cmd", NonEmpty.fromElement [ "Platform", "Cmd" ] )
        , ( "Sub", NonEmpty.fromElement [ "Platform", "Sub" ] )
        ]
