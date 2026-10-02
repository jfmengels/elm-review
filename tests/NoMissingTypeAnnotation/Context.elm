module NoMissingTypeAnnotation.Context exposing
    ( Context
    , importVisitor
    , init
    )

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.TypeInference.InferError exposing (InferError)
import Elm.TypeInference.Type exposing (Type)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


type alias Context =
    { getType : Range -> Result.Result InferError Elm.TypeInference.Type.Type
    , moduleNameAliases : Dict String String
    , availableTypes : Set ( String, String )
    , importLine : Int
    }


init : Rule.ContextCreator () Context
init =
    Rule.initContextCreator
        (\getType moduleName ast () ->
            { getType = getType
            , moduleNameAliases = Dict.insert (String.join "." moduleName) "" preludeTypeAliases
            , availableTypes = preludeTypeImports
            , importLine =
                case List.head ast.imports of
                    Just (Node range _) ->
                        range.start.row

                    Nothing ->
                        (Node.range ast.moduleDefinition).start.row + 1
            }
        )
        |> Rule.withTypes
        |> Rule.withModuleName
        |> Rule.withFullAst


preludeTypeImports : Set ( String, String )
preludeTypeImports =
    Set.fromList
        [ ( "Basics", "Int" )
        , ( "Basics", "Float" )
        , ( "Basics", "Bool" )
        , ( "Basics", "Order" )
        , ( "Basics", "Never" )
        , ( "Char", "Char" )
        , ( "String", "String" )
        , ( "List", "List" )
        , ( "Maybe", "Maybe" )
        , ( "Platform", "Program" )
        , ( "Platform.Cmd", "Cmd" )
        , ( "Platform.Sub", "Sub" )
        , ( "Result", "Result" )
        ]


preludeTypeAliases : Dict String String
preludeTypeAliases =
    Dict.fromList
        [ ( "Platform.Cmd", "Cmd" )
        , ( "Platform.Sub", "Sub" )
        ]


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor (Node _ { moduleName, moduleAlias, exposingList }) context =
    let
        moduleName_ : String
        moduleName_ =
            String.join "." (Node.value moduleName)

        nameInUse : String
        nameInUse =
            case moduleAlias of
                Just (Node _ alias) ->
                    String.join "." alias

                Nothing ->
                    moduleName_
    in
    ( []
    , { getType = context.getType
      , moduleNameAliases = Dict.insert moduleName_ nameInUse context.moduleNameAliases
      , availableTypes = addImportedTypes moduleName_ exposingList context.availableTypes
      , importLine = context.importLine
      }
    )


addImportedTypes : String -> Maybe (Node Exposing.Exposing) -> Set ( String, String ) -> Set ( String, String )
addImportedTypes moduleName exposingList availableTypes =
    case Maybe.map Node.value exposingList of
        Just (Exposing.Explicit list) ->
            List.foldl
                (\(Node _ elem) acc ->
                    case elem of
                        Exposing.TypeOrAliasExpose name ->
                            Set.insert ( moduleName, name ) acc

                        Exposing.TypeExpose { name } ->
                            Set.insert ( moduleName, name ) acc

                        Exposing.FunctionExpose _ ->
                            acc

                        Exposing.InfixExpose _ ->
                            acc
                )
                availableTypes
                list

        Just (Exposing.All _) ->
            availableTypes

        Nothing ->
            availableTypes
