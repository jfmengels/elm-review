module NoUnused.Variables.ElmPrelude exposing (elmPreludeAliases, elmPreludeExposing)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)



-- These are the default imports implicitly added by the Elm compiler
-- https://package.elm-lang.org/packages/elm/core/latest


{-| Elements explicitly exposed by the prelude.
-}
elmPreludeExposing : Dict (List String) (List String)
elmPreludeExposing =
    Dict.fromList
        [ ( [ "List" ], [ "List", "::" ] )
        , ( [ "Maybe" ], [ "Maybe" ] )
        , ( [ "Result" ], [ "Result" ] )
        , ( [ "String" ], [ "String" ] )
        , ( [ "Char" ], [ "Char" ] )
        , ( [ "Platform" ], [ "Program" ] )
        , ( [ "Platform", "Cmd" ], [ "Cmd" ] )
        , ( [ "Platform", "Sub" ], [ "Sub" ] )
        ]


{-| Module aliases defined by the prelude.
-}
elmPreludeAliases : Dict (List String) ModuleName
elmPreludeAliases =
    Dict.fromList
        [ ( [ "List" ], [ "List" ] )
        , ( [ "Maybe" ], [ "Maybe" ] )
        , ( [ "Result" ], [ "Result" ] )
        , ( [ "String" ], [ "String" ] )
        , ( [ "Char" ], [ "Char" ] )
        , ( [ "Tuple" ], [ "Tuple" ] )
        , ( [ "Debug" ], [ "Debug" ] )
        , ( [ "Platform" ], [ "Platform" ] )
        , ( [ "Platform", "Cmd" ], [ "Cmd" ] )
        , ( [ "Platform", "Sub" ], [ "Sub" ] )
        ]
