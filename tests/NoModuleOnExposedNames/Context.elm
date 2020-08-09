module NoModuleOnExposedNames.Context exposing (Module, expose, initial, isFunctionExposed, isTypeExposed)

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)


type Module
    = Expose (Dict ModuleName Exposing)


initial : Module
initial =
    Expose Dict.empty


expose : ModuleName -> Exposing -> Module -> Module
expose moduleName exposer (Expose exposes) =
    Expose (Dict.insert moduleName exposer exposes)


isFunctionExposed : Module -> ModuleName -> String -> Bool
isFunctionExposed (Expose exposes) moduleName name =
    case Dict.get moduleName exposes of
        Nothing ->
            False

        Just exposer ->
            Exposing.exposesFunction name exposer


isTypeExposed : Module -> ModuleName -> String -> Bool
isTypeExposed (Expose exposes) moduleName name =
    case Dict.get moduleName exposes of
        Nothing ->
            False

        Just (Exposing.All _) ->
            True

        Just (Exposing.Explicit list) ->
            List.any (isTypeNamed name) list



--- HELPERS


isTypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isTypeNamed name topLevelExpose =
    (topLevelExpose |> Node.value |> exposingTypeName) == Just name


exposingTypeName : Exposing.TopLevelExpose -> Maybe String
exposingTypeName topLevelExpose =
    case topLevelExpose of
        Exposing.FunctionExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose name ->
            Just name

        Exposing.TypeExpose { name } ->
            Just name

        Exposing.InfixExpose _ ->
            Nothing
