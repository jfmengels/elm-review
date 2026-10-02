module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.TypeInference.InferError exposing (InferError)
import Elm.TypeInference.Type exposing (Type(..))
import NoMissingTypeAnnotation.Print as Print
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports top-level declarations that do not have a type annotation.

Type annotations help you understand what happens in the code, and it will help the compiler give better error messages.

    config =
        [ NoMissingTypeAnnotation.rule
        ]

This rule does not report declarations without a type annotation inside a `let in`.
For that, enable [`NoMissingTypeAnnotationInLetIn`](./NoMissingTypeAnnotationInLetIn).


## Fail

    a =
        1


## Success

    a : number
    a =
        1

    b : number
    b =
        let
            c =
                2
        in
        c


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoMissingTypeAnnotation
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoMissingTypeAnnotation" initialContext
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { getType : Range -> Result InferError Elm.TypeInference.Type.Type
    , moduleName : String
    , moduleNameAliases : Dict String String
    , availableTypes : Set ( String, String )
    , importLine : Int
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\getType moduleName_ ast () ->
            let
                moduleName : String
                moduleName =
                    String.join "." moduleName_
            in
            { getType = getType
            , moduleName = moduleName
            , moduleNameAliases = Dict.insert moduleName "" preludeTypeAliases
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
      , moduleName = moduleName_
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


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor (Node declRange declaration) context =
    case declaration of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Nothing ->
                    let
                        (Node range name) =
                            function.declaration
                                |> Node.value
                                |> .name

                        fix : List Fix.Edit
                        fix =
                            case context.getType declRange of
                                Ok type_ ->
                                    Print.insertTypeAnnotation
                                        context
                                        (Node.range function.declaration).start
                                        name
                                        type_

                                Err _ ->
                                    []
                    in
                    ( [ Rule.errorWithFix
                            { message = "Missing type annotation for `" ++ name ++ "`"
                            , details = [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages." ]
                            }
                            range
                            fix
                      ]
                    , { getType = context.getType
                      , moduleName = context.moduleName
                      , moduleNameAliases = context.moduleNameAliases
                      , availableTypes = context.availableTypes
                      , importLine = context.importLine
                      }
                    )

                Just _ ->
                    ( [], context )

        _ ->
            ( [], context )
