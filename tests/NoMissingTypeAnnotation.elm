module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import NoMissingTypeAnnotation.Context as Context exposing (Context)
import NoMissingTypeAnnotation.Print as Print
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports declarations that do not have a type annotation.

Type annotations help you understand what happens in the code, and it will help the compiler give better error messages.

    config =
        [ NoMissingTypeAnnotation.rule { nested = True }
        ]

Setting `nested` to `False` will make it so that functions declared in let declarations will not be reported.


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
            c : number
            c =
                2
        in
        c

    -- With `nested = False`
    d : number
    d =
        let
            e =
                2
        in
        e


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoMissingTypeAnnotation
```

-}
rule : { nested : Bool } -> Rule
rule { nested } =
    Rule.newModuleRuleSchemaUsingContextCreator "NoMissingTypeAnnotation" Context.init
        |> Rule.withImportVisitor Context.importVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> visitLetDeclarations nested
        |> Rule.fromModuleRuleSchema


visitLetDeclarations : Bool -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } Context -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } Context
visitLetDeclarations nested schema =
    if nested then
        Rule.withExpressionEnterVisitor expressionVisitor schema

    else
        schema


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor (Node _ declaration) context =
    case declaration of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Nothing ->
                    ( [ reportFunctionWithoutSignature context function.declaration ]
                    , context
                    )

                Just _ ->
                    ( [], context )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor (Node _ expression) context =
    case expression of
        Expression.LetExpression { declarations } ->
            let
                errors : List (Error {})
                errors =
                    List.filterMap
                        (\declaration ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    case function.signature of
                                        Just _ ->
                                            Nothing

                                        Nothing ->
                                            Just (reportFunctionWithoutSignature context function.declaration)

                                _ ->
                                    Nothing
                        )
                        declarations
            in
            ( errors
            , context
            )

        _ ->
            ( [], context )


reportFunctionWithoutSignature : Context -> Node Expression.FunctionImplementation -> Error {}
reportFunctionWithoutSignature context (Node declRange functionDeclaration) =
    let
        (Node nameRange name) =
            functionDeclaration.name

        fix : List Fix.Edit
        fix =
            case context.getType declRange of
                Ok type_ ->
                    Print.insertTypeAnnotation
                        context
                        nameRange.start
                        name
                        type_

                Err _ ->
                    []
    in
    Rule.errorWithFix
        { message = "Missing type annotation for `" ++ name ++ "`"
        , details =
            [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
            ]
        }
        nameRange
        fix
