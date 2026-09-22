module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.TypeInference.Type exposing (Type(..))
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import TypeLookupTable exposing (TypeLookupTable)


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
    { typeLookupTable : TypeLookupTable
    , moduleNameAliases : Dict String String
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\typeLookupTable moduleName () ->
            { typeLookupTable = typeLookupTable
            , moduleNameAliases = Dict.singleton (String.join "." moduleName) ""
            }
        )
        |> Rule.withTypeLookupTable
        |> Rule.withModuleName


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor (Node _ { moduleName, moduleAlias, exposingList }) context =
    ( []
    , { typeLookupTable = context.typeLookupTable
      , moduleNameAliases =
            case moduleAlias of
                Just (Node _ alias) ->
                    Dict.insert (String.join "." (Node.value moduleName)) (String.join "." alias) context.moduleNameAliases

                Nothing ->
                    context.moduleNameAliases
      }
    )


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor declaration context =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Nothing ->
                    let
                        (Node range name) =
                            function.declaration
                                |> Node.value
                                |> .name

                        ( inferredType, typeLookupTable ) =
                            TypeLookupTable.get (Node.range declaration) context.typeLookupTable

                        fix : List Fix.Edit
                        fix =
                            case inferredType of
                                Just type_ ->
                                    [ Fix.insertAt (Node.range declaration).start (name ++ " : " ++ toString context.moduleNameAliases type_ ++ "\n") ]

                                Nothing ->
                                    []
                    in
                    ( [ Rule.errorWithFix
                            { message = "Missing type annotation for `" ++ name ++ "`"
                            , details = [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages." ]
                            }
                            range
                            fix
                      ]
                    , { typeLookupTable = typeLookupTable
                      , moduleNameAliases = context.moduleNameAliases
                      }
                    )

                Just _ ->
                    ( [], context )

        _ ->
            ( [], context )


toString : Dict String String -> Type -> String
toString moduleNameAliases t =
    case t of
        TypeVar name ->
            name

        Function { from, to } ->
            wrappedFrom moduleNameAliases from ++ " -> " ++ toString moduleNameAliases to

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
            "List " ++ wrapped moduleNameAliases inner

        Unit ->
            "()"

        Tuple2 a b ->
            "( " ++ toString moduleNameAliases a ++ ", " ++ toString moduleNameAliases b ++ " )"

        Tuple3 a b c ->
            "( " ++ toString moduleNameAliases a ++ ", " ++ toString moduleNameAliases b ++ ", " ++ toString moduleNameAliases c ++ " )"

        Record { fields } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString moduleNameAliases fieldType)
            in
            "{ " ++ String.join ", " fieldStrings ++ " }"

        ExtensibleRecord { fields, extensionTypevar } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString moduleNameAliases fieldType)
            in
            "{ " ++ extensionTypevar ++ " | " ++ String.join ", " fieldStrings ++ " }"

        Named { moduleName, name, arguments } ->
            let
                argStrings : List String
                argStrings =
                    List.map (wrapped moduleNameAliases) arguments

                dotted : String
                dotted =
                    String.join "." moduleName

                qualifiedName : String
                qualifiedName =
                    case Dict.get dotted moduleNameAliases of
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
                ++ toString moduleNameAliases (shaderSlotToType r.attributesFields r.attributesExtensionTypevar)
                ++ " "
                ++ toString moduleNameAliases (shaderSlotToType r.uniformsFields r.uniformsExtensionTypevar)
                ++ " "
                ++ toString moduleNameAliases (shaderSlotToType r.varyingsFields r.varyingsExtensionTypevar)


shaderSlotToType : Dict String Type -> Maybe String -> Type
shaderSlotToType fields extensionTypevar =
    case extensionTypevar of
        Nothing ->
            Record { fields = fields }

        Just var ->
            if Dict.isEmpty fields then
                TypeVar var

            else
                ExtensibleRecord { fields = fields, extensionTypevar = var }


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously
as an argument of a type constructor application.
-}
wrapped : Dict String String -> Type -> String
wrapped moduleNameAliases t =
    case t of
        Function _ ->
            paren (toString moduleNameAliases t)

        List _ ->
            paren (toString moduleNameAliases t)

        WebGLShader _ ->
            paren (toString moduleNameAliases t)

        Named r ->
            if List.isEmpty r.arguments then
                toString moduleNameAliases t

            else
                paren (toString moduleNameAliases t)

        _ ->
            toString moduleNameAliases t


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously on the
left of `->`.

`->` is right-associative and type application binds tighter, so only a nested
`->` needs parens there: `List a -> b` already parses as `(List a) -> b`.

-}
wrappedFrom : Dict String String -> Type -> String
wrappedFrom moduleNameAliases t =
    case t of
        Function _ ->
            paren (toString moduleNameAliases t)

        _ ->
            toString moduleNameAliases t


paren : String -> String
paren str =
    "(" ++ str ++ ")"
