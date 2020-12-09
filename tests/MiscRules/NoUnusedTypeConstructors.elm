module MiscRules.NoUnusedTypeConstructors exposing (rule)

{-| Forbid having unused custom type constructors in a file.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid having unused custom type constructors in a file.

    config =
        [ NoUnusedTypeConstructors.rule
        ]

Note that this does not report a constructor if it is exposed in the module, even
if it is not used anywhere in the project. For a more accurate detection of
unused constructors (and functions) across your project, you might want to check
out [elm-xref](https://github.com/zwilias/elm-xref). You may still want to use
this rule in your config so that you get notified of unused constructors earlier
in your editor, rather than when running your tests or [elm-xref](https://github.com/zwilias/elm-xref).


## Fail

    module A exposing (a)

    type MyType
        = UsedType
        | UnusedType -- Will get reported

    a =
        UsedType


## Success

    module A exposing (ExposedType(..))

    type MyType
        = UsedType

    a =
        UsedType

    type ExposedType
        = A
        | B
        | C

    -----------------------
    module A exposing (..)

    type ExposedType
        = A
        | B
        | C

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnusedTypeConstructors" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { exposedCustomTypesWithConstructors : Set String
    , exposesEverything : Bool
    , declaredTypesWithConstructors : Dict String (Node String)
    , usedFunctionOrValues : Set String
    }


initialContext : Context
initialContext =
    { exposedCustomTypesWithConstructors = Set.empty
    , exposesEverything = False
    , declaredTypesWithConstructors = Dict.empty
    , usedFunctionOrValues = Set.empty
    }


error : Node String -> Error {}
error node =
    Rule.error
        { message = "Type constructor `" ++ Node.value node ++ "` is not used."
        , details = [ "Since it is not being used, I recommend removing it. It should make the code clearer to read for other people." ]
        }
        (Node.range node)


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor moduleNode context =
    case Module.exposingList (Node.value moduleNode) of
        All _ ->
            ( [], { context | exposesEverything = True } )

        Explicit list ->
            let
                names : List String
                names =
                    List.filterMap
                        (\node ->
                            case Node.value node of
                                TypeExpose { name, open } ->
                                    case open of
                                        Just _ ->
                                            Just name

                                        Nothing ->
                                            Nothing

                                _ ->
                                    Nothing
                        )
                        list
            in
            ( []
            , { context
                | exposedCustomTypesWithConstructors =
                    Set.union (Set.fromList names) context.exposedCustomTypesWithConstructors
              }
            )


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        CustomTypeDeclaration { name, constructors } ->
            if Set.member (Node.value name) context.exposedCustomTypesWithConstructors then
                ( [], context )

            else
                let
                    newContext : Context
                    newContext =
                        List.foldl
                            (\constructor ctx ->
                                let
                                    nameNode : Node String
                                    nameNode =
                                        (Node.value constructor).name
                                in
                                { ctx
                                    | declaredTypesWithConstructors =
                                        Dict.insert
                                            (Node.value nameNode)
                                            nameNode
                                            ctx.declaredTypesWithConstructors
                                }
                            )
                            context
                            constructors
                in
                ( [], newContext )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node context =
    if context.exposesEverything then
        ( [], context )

    else
        case Node.value node of
            FunctionOrValue [] name ->
                ( [], { context | usedFunctionOrValues = Set.insert name context.usedFunctionOrValues } )

            _ ->
                ( [], context )


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    if context.exposesEverything then
        []

    else
        context.declaredTypesWithConstructors
            |> Dict.filter (\name _ -> not <| Set.member name context.usedFunctionOrValues)
            |> Dict.toList
            |> List.map (\( _, node ) -> error node)
