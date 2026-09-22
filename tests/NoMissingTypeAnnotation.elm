module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
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
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    TypeLookupTable


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator (\typeLookupTable () -> typeLookupTable)
        |> Rule.withTypeLookupTable


declarationVisitor : Node Declaration -> TypeLookupTable -> ( List (Error {}), TypeLookupTable )
declarationVisitor declaration typeLookupTable =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Nothing ->
                    let
                        (Node range name) =
                            function.declaration
                                |> Node.value
                                |> .name

                        ( inferredType, newTypeLookupTable ) =
                            TypeLookupTable.get (Node.range declaration) typeLookupTable

                        fix : List Fix.Edit
                        fix =
                            case inferredType of
                                Just type_ ->
                                    [ Fix.insertAt (Node.range declaration).start (name ++ " : " ++ toString type_ ++ "\n") ]

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
                    , newTypeLookupTable
                    )

                Just _ ->
                    ( [], typeLookupTable )

        _ ->
            ( [], typeLookupTable )


toString : Type -> String
toString t =
    case t of
        TypeVar name ->
            name

        Function { from, to } ->
            wrappedFrom from ++ " -> " ++ toString to

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
            "List " ++ wrapped inner

        Unit ->
            "()"

        Tuple2 a b ->
            "( " ++ toString a ++ ", " ++ toString b ++ " )"

        Tuple3 a b c ->
            "( " ++ toString a ++ ", " ++ toString b ++ ", " ++ toString c ++ " )"

        Record { fields } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString fieldType)
            in
            "{" ++ String.join ", " fieldStrings ++ "}"

        ExtensibleRecord { fields, extensionTypevar } ->
            let
                fieldStrings : List String
                fieldStrings =
                    fields
                        |> Dict.toList
                        |> List.map (\( name, fieldType ) -> name ++ " : " ++ toString fieldType)
            in
            "{ " ++ extensionTypevar ++ " | " ++ String.join ", " fieldStrings ++ " }"

        Named { moduleName, name, arguments } ->
            let
                argStrings : List String
                argStrings =
                    arguments
                        |> List.map wrapped

                qualifiedName : String
                qualifiedName =
                    String.join "." moduleName ++ "." ++ name
            in
            (qualifiedName :: argStrings)
                |> String.join " "

        WebGLShader r ->
            [ "Shader"
            , shaderSlotToString r.attributesFields r.attributesExtensionTypevar
            , shaderSlotToString r.uniformsFields r.uniformsExtensionTypevar
            , shaderSlotToString r.varyingsFields r.varyingsExtensionTypevar
            ]
                |> String.join " "


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


shaderSlotToString : Dict String Type -> Maybe String -> String
shaderSlotToString fields extensionTypevar =
    toString (shaderSlotToType fields extensionTypevar)


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously
as an argument of a type constructor application.
-}
wrapped : Type -> String
wrapped t =
    case t of
        Function _ ->
            paren t

        List _ ->
            paren t

        WebGLShader _ ->
            paren t

        Named r ->
            if List.isEmpty r.arguments then
                toString t

            else
                paren t

        _ ->
            toString t


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously on the
left of `->`.

`->` is right-associative and type application binds tighter, so only a nested
`->` needs parens there: `List a -> b` already parses as `(List a) -> b`.

-}
wrappedFrom : Type -> String
wrappedFrom t =
    case t of
        Function _ ->
            paren t

        _ ->
            toString t


paren : Type -> String
paren t =
    "(" ++ toString t ++ ")"
