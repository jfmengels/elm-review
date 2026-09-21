module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.TypeInference.Type
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
                                    [ Fix.insertAt (Node.range declaration).start (name ++ " : " ++ Elm.TypeInference.Type.toString type_ ++ "\n") ]

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
