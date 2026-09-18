module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.TypeInference.Type
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
        |> Rule.withDeclarationEnterVisitor (\node ctx -> ( declarationVisitor node ctx, ctx ))
        |> Rule.fromModuleRuleSchema


type alias Context =
    TypeLookupTable


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator (\typeLookupTable () -> typeLookupTable)
        |> Rule.withTypeLookupTable


declarationVisitor : Node Declaration -> TypeLookupTable -> List (Error {})
declarationVisitor declaration typeLookupTable =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Nothing ->
                    let
                        name : Node String
                        name =
                            function.declaration
                                |> Node.value
                                |> .name

                        inferredType : String
                        inferredType =
                            case TypeLookupTable.get (function.declaration |> Node.value |> .expression |> Node.range) typeLookupTable of
                                Just type_ ->
                                    Elm.TypeInference.Type.toString type_

                                Nothing ->
                                    "???"
                    in
                    [ Rule.error
                        { message = "Missing type annotation for `" ++ Node.value name ++ "`"
                        , details =
                            [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
                            , "Add the following line:"
                            , "    " ++ Node.value name ++ " : " ++ inferredType
                            ]
                        }
                        (Node.range name)
                    ]

                Just _ ->
                    []

        _ ->
            []
