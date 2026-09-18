module Elm.TypeInference.ModuleIds exposing
    ( Mapping
    , ModuleId
    , basicsId
    , charId
    , dottedForDisplay
    , empty
    , getId
    , getIdByDotted
    , getName
    , intern
    , listId
    , mathMatrix4Id
    , mathVector2Id
    , mathVector3Id
    , mathVector4Id
    , maybeId
    , moduleNameForDisplay
    , platformCmdId
    , platformId
    , platformSubId
    , resultId
    , stringId
    , webGLId
    , webGLTextureId
    )

{-| Intern `FullModuleName`s to `Int`s.

Allows fast Int comparison instead of deep equality on FullModuleName.

-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)


type alias ModuleId =
    Int


type alias Mapping =
    { byDotted : Dict String ModuleId
    , byId : Dict ModuleId FullModuleName
    , next : ModuleId
    }


{-| Stable hardcoded IDs.

Some code paths (eg. `collapsePrimitive`) compare against them directly instead
of resolving the module name.

-}
basicsId : ModuleId
basicsId =
    0


listId : ModuleId
listId =
    1


maybeId : ModuleId
maybeId =
    2


resultId : ModuleId
resultId =
    3


stringId : ModuleId
stringId =
    4


charId : ModuleId
charId =
    5


platformId : ModuleId
platformId =
    6


platformCmdId : ModuleId
platformCmdId =
    7


platformSubId : ModuleId
platformSubId =
    8


webGLId : ModuleId
webGLId =
    9


webGLTextureId : ModuleId
webGLTextureId =
    10


mathVector2Id : ModuleId
mathVector2Id =
    11


mathVector3Id : ModuleId
mathVector3Id =
    12


mathVector4Id : ModuleId
mathVector4Id =
    13


mathMatrix4Id : ModuleId
mathMatrix4Id =
    14


empty : Mapping
empty =
    let
        predefined : List ( String, ModuleId )
        predefined =
            [ ( "Basics", basicsId )
            , ( "List", listId )
            , ( "Maybe", maybeId )
            , ( "Result", resultId )
            , ( "String", stringId )
            , ( "Char", charId )
            , ( "Platform", platformId )
            , ( "Platform.Cmd", platformCmdId )
            , ( "Platform.Sub", platformSubId )
            , ( "WebGL", webGLId )
            , ( "WebGL.Texture", webGLTextureId )
            , ( "Math.Vector2", mathVector2Id )
            , ( "Math.Vector3", mathVector3Id )
            , ( "Math.Vector4", mathVector4Id )
            , ( "Math.Matrix4", mathMatrix4Id )
            ]
    in
    { byDotted = Dict.fromList predefined
    , byId =
        predefined
            |> List.foldl
                (\( dotted, id ) acc ->
                    Dict.insert id (FullModuleName.fromDotted dotted) acc
                )
                Dict.empty
    , next = 15
    }


internDotted : String -> FullModuleName -> Mapping -> ( ModuleId, Mapping )
internDotted dotted full moduleMapping =
    case Dict.get dotted moduleMapping.byDotted of
        Just existing ->
            ( existing, moduleMapping )

        Nothing ->
            let
                newId : ModuleId
                newId =
                    moduleMapping.next
            in
            ( newId
            , { byDotted = Dict.insert dotted newId moduleMapping.byDotted
              , byId = Dict.insert newId full moduleMapping.byId
              , next = newId + 1
              }
            )


intern : FullModuleName -> Mapping -> ( ModuleId, Mapping )
intern full moduleMapping =
    internDotted (FullModuleName.toString full) full moduleMapping


getIdByDotted : String -> Mapping -> Maybe ModuleId
getIdByDotted dotted moduleMapping =
    Dict.get dotted moduleMapping.byDotted


getId : FullModuleName -> Mapping -> Maybe ModuleId
getId full moduleMapping =
    getIdByDotted (FullModuleName.toString full) moduleMapping


getName : ModuleId -> Mapping -> Maybe FullModuleName
getName moduleId moduleMapping =
    Dict.get moduleId moduleMapping.byId


getNameForDisplay : ModuleId -> Mapping -> FullModuleName
getNameForDisplay moduleId moduleMapping =
    Dict.get moduleId moduleMapping.byId
        |> Maybe.withDefault fullModuleNameUnknownId


fullModuleNameUnknownId : FullModuleName
fullModuleNameUnknownId =
    ( "<unknown module id>", [] )


moduleNameForDisplay : ModuleId -> Mapping -> ModuleName
moduleNameForDisplay moduleId moduleMapping =
    getNameForDisplay moduleId moduleMapping
        |> FullModuleName.toModuleName


dottedForDisplay : ModuleId -> Mapping -> String
dottedForDisplay moduleId moduleMapping =
    getNameForDisplay moduleId moduleMapping
        |> FullModuleName.toString
