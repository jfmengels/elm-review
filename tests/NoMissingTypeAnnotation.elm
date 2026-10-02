module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import NoMissingTypeAnnotation.Context as Context exposing (Context)
import NoMissingTypeAnnotation.Print as Print
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


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
    Rule.newModuleRuleSchemaUsingContextCreator "NoMissingTypeAnnotation" Context.init
        |> Rule.withImportVisitor Context.importVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


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
                    , context
                    )

                Just _ ->
                    ( [], context )

        _ ->
            ( [], context )
