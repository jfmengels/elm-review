module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.InferError exposing (InferError)
import Elm.TypeInference.Type exposing (Type(..))
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
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\getType moduleName_ () ->
            let
                moduleName : String
                moduleName =
                    String.join "." moduleName_
            in
            { getType = getType
            , moduleName = moduleName
            , moduleNameAliases = Dict.insert moduleName "" preludeTypeAliases
            , availableTypes = preludeTypeImports
            }
        )
        |> Rule.withTypes
        |> Rule.withModuleName


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
    in
    ( []
    , { getType = context.getType
      , moduleName = context.moduleName
      , moduleNameAliases =
            case moduleAlias of
                Just (Node _ alias) ->
                    Dict.insert moduleName_ (String.join "." alias) context.moduleNameAliases

                Nothing ->
                    context.moduleNameAliases
      , availableTypes = addImportedTypes moduleName_ exposingList context.availableTypes
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
                                    [ Fix.insertAt (Node.range function.declaration).start (name ++ " : " ++ toString context type_ ++ "\n") ]

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
                      }
                    )

                Just _ ->
                    ( [], context )

        _ ->
            ( [], context )


toString : Context -> Type -> String
toString context t =
    case t of
        TypeVar name ->
            name

        Function { from, to } ->
            wrappedFrom context from ++ " -> " ++ toString context to

        Int ->
            "Int"

        Float ->
            "Float"

        Char ->
            "Char"

        String ->
            "String"

        Bool ->
            "Bool"

        List inner ->
            "List " ++ wrapped context inner

        Unit ->
            "()"

        Tuple2 a b ->
            "( " ++ toString context a ++ ", " ++ toString context b ++ " )"

        Tuple3 a b c ->
            "( " ++ toString context a ++ ", " ++ toString context b ++ ", " ++ toString context c ++ " )"

        Record { fields } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString context fieldType)
            in
            "{ " ++ String.join ", " fieldStrings ++ " }"

        ExtensibleRecord { fields, extensionTypevar } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString context fieldType)
            in
            "{ " ++ extensionTypevar ++ " | " ++ String.join ", " fieldStrings ++ " }"

        Named { moduleName, name, arguments } ->
            let
                argStrings : List String
                argStrings =
                    List.map (wrapped context) arguments

                dotted : String
                dotted =
                    String.join "." moduleName

                qualifiedName : String
                qualifiedName =
                    if Set.member ( dotted, name ) context.availableTypes then
                        name

                    else
                        case Dict.get dotted context.moduleNameAliases of
                            Just "" ->
                                name

                            Just alias_ ->
                                alias_ ++ "." ++ name

                            Nothing ->
                                dotted ++ "." ++ name
            in
            (qualifiedName :: argStrings)
                |> String.join " "

        WebGLShader r ->
            "Shader "
                ++ wrapped context r.attributes
                ++ " "
                ++ wrapped context r.uniforms
                ++ " "
                ++ wrapped context r.varyings


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously
as an argument of a type constructor application.
-}
wrapped : Context -> Type -> String
wrapped context t =
    case t of
        Function _ ->
            paren (toString context t)

        List _ ->
            paren (toString context t)

        WebGLShader _ ->
            paren (toString context t)

        Named r ->
            if List.isEmpty r.arguments then
                toString context t

            else
                paren (toString context t)

        _ ->
            toString context t


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously on the
left of `->`.

`->` is right-associative and type application binds tighter, so only a nested
`->` needs parens there: `List a -> b` already parses as `(List a) -> b`.

-}
wrappedFrom : Context -> Type -> String
wrappedFrom context t =
    case t of
        Function _ ->
            paren (toString context t)

        _ ->
            toString context t


paren : String -> String
paren str =
    "(" ++ str ++ ")"
