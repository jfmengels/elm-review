module NoUnused.CustomTypeConstructors2 exposing (rule)

{-| Forbid having unused custom type constructors in a file.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Direction, Error, Rule)
import Scope2 as Scope
import Set exposing (Set)


{-| Forbid having unused custom type constructors in a file.

    config =
        [ NoUnused.CustomTypeConstructors.rule
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
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructors"
        { moduleVisitor = moduleVisitor
        , initProjectContext = initProjectContext
        , fromProjectToModule = fromProjectToModule
        , fromModuleToProject = fromModuleToProject
        , foldProjectContexts = foldProjectContexts
        }
        |> Scope.addProjectVisitors
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema



-- MODULE VISITOR


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalModuleEvaluation



-- CONTEXT


type alias ProjectContext =
    { scope : Scope.ProjectContext
    , exposedModules : Set String
    , exposedConstructors : Dict String { moduleKey : Rule.ModuleKey, constructors : Dict String (Node String) }
    }


type alias ModuleContext =
    { scope : Scope.ModuleContext
    , exposedCustomTypesWithConstructors : Set String
    , isExposed : Bool
    , exposesEverything : Bool
    , declaredTypesWithConstructors : Dict String (Dict String (Node String))
    , usedFunctionOrValues : Set String
    , phantomVariables : List ( String, Int )
    }


initProjectContext : ProjectContext
initProjectContext =
    { scope = Scope.initProjectContext
    , exposedModules = Set.empty
    , exposedConstructors = Dict.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ (Node.Node _ moduleName) projectContext =
    { scope = Scope.fromProjectToModule projectContext.scope
    , exposedCustomTypesWithConstructors = Set.empty
    , isExposed = Set.member (String.join "." moduleName) projectContext.exposedModules
    , exposesEverything = False
    , declaredTypesWithConstructors = Dict.empty
    , usedFunctionOrValues = Set.empty
    , phantomVariables = []
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject _ moduleName moduleContext =
    { scope = Scope.fromModuleToProject moduleName moduleContext.scope
    , exposedModules = Set.empty

    -- TODO
    , exposedConstructors = Dict.empty
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { scope = Scope.foldProjectContexts previousContext.scope newContext.scope
    , exposedModules = previousContext.exposedModules

    -- TODO
    , exposedConstructors = previousContext.exposedConstructors
    }


error : Node String -> Error
error node =
    Rule.error
        { message = "Type constructor `" ++ Node.value node ++ "` is not used."
        , details =
            [ "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."
            , "You should either use this value somewhere, or remove it at the location I pointed at."
            , "If you remove it, you may find that other pieces of code are never used, and can themselves be removed too. This could end up simplifying your code a lot."
            ]
        }
        (Node.range node)



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ProjectContext
elmJsonVisitor maybeElmJson projectContext =
    case maybeElmJson |> Maybe.map .project of
        Just (Elm.Project.Package package) ->
            let
                exposedModules : List Elm.Module.Name
                exposedModules =
                    case package.exposed of
                        Elm.Project.ExposedList list ->
                            list

                        Elm.Project.ExposedDict list ->
                            List.concatMap Tuple.second list

                exposedNames : Set String
                exposedNames =
                    exposedModules
                        |> List.map Elm.Module.toString
                        |> Set.fromList
            in
            { projectContext | exposedModules = exposedNames }

        Just (Elm.Project.Application _) ->
            projectContext

        Nothing ->
            projectContext



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor moduleNode context =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            ( [], { context | exposesEverything = True } )

        Exposing.Explicit list ->
            let
                names : List String
                names =
                    List.filterMap
                        (\node ->
                            case Node.value node of
                                Exposing.TypeExpose { name, open } ->
                                    Just name

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



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    ( [], List.foldl register context nodes )


register : Node Declaration -> ModuleContext -> ModuleContext
register node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, generics, constructors } ->
            let
                nonPhantomVariables : Set String
                nonPhantomVariables =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.concatMap collectGenericsFromTypeAnnotation
                        |> Set.fromList

                phantomVariables : List ( String, Int )
                phantomVariables =
                    generics
                        |> List.map Node.value
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, genericName ) -> not <| Set.member genericName nonPhantomVariables)
                        |> List.map (\( indexOfPhantomVariable, _ ) -> ( Node.value name, indexOfPhantomVariable ))
            in
            { context | phantomVariables = phantomVariables ++ context.phantomVariables }

        _ ->
            context



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> Direction -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.CustomTypeDeclaration { name, constructors } ) ->
            -- if Set.member (Node.value name) context.exposedCustomTypesWithConstructors then
            --     ( [], context )
            --
            -- else
            let
                constructorsForCustomType : Dict String (Node String)
                constructorsForCustomType =
                    List.foldl
                        (\constructor dict ->
                            let
                                nameNode : Node String
                                nameNode =
                                    (Node.value constructor).name
                            in
                            Dict.insert
                                (Node.value nameNode)
                                nameNode
                                dict
                        )
                        Dict.empty
                        constructors
            in
            ( []
            , { context
                | declaredTypesWithConstructors =
                    Dict.insert
                        (Node.value name)
                        constructorsForCustomType
                        context.declaredTypesWithConstructors
              }
            )

        ( Rule.OnEnter, Declaration.FunctionDeclaration function ) ->
            ( [], markPhantomTypesFromTypeSignatureAsUsed function.signature context )

        _ ->
            ( [], context )



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> Direction -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node direction context =
    if context.exposesEverything then
        ( [], context )

    else
        case ( direction, Node.value node ) of
            ( Rule.OnEnter, Expression.FunctionOrValue [] name ) ->
                ( [], { context | usedFunctionOrValues = Set.insert name context.usedFunctionOrValues } )

            ( Rule.OnEnter, Expression.LetExpression { declarations } ) ->
                ( []
                , declarations
                    |> List.filterMap
                        (\declaration ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    Just function.signature

                                Expression.LetDestructuring _ _ ->
                                    Nothing
                        )
                    |> List.foldl markPhantomTypesFromTypeSignatureAsUsed context
                )

            _ ->
                ( [], context )



-- FINAL MODULE EVALUATION


finalModuleEvaluation : ModuleContext -> List Error
finalModuleEvaluation context =
    -- TODO Turn this into a finalProjectEvaluation
    if context.exposesEverything && context.isExposed then
        []

    else
        context.declaredTypesWithConstructors
            |> Dict.filter (\customTypeName _ -> not (context.isExposed && Set.member customTypeName context.exposedCustomTypesWithConstructors))
            |> Dict.values
            |> List.concatMap
                (\dict ->
                    dict
                        |> Dict.filter (\name _ -> not (Set.member name context.usedFunctionOrValues || (context.isExposed && Set.member name context.exposedCustomTypesWithConstructors)))
                        |> Dict.toList
                        |> List.map (\( _, node ) -> error node)
                )


constructorsToWarnAbout : ModuleContext -> Dict String (Dict String (Node String))
constructorsToWarnAbout context =
    Dict.empty



-- FINAL PROJECT EVALUATION


finalProjectEvaluation : ProjectContext -> List Error
finalProjectEvaluation context =
    -- let
    --     _ =
    --         context.exposedConstructors
    --             |> Dict.filter (\moduleName { moduleKey, constructors, exposedConstructors } -> True)
    -- in
    -- TODO Turn this into a finalProjectEvaluation
    -- if context.exposesEverything && context.isExposed then
    --     []
    --
    -- else
    --     context.declaredTypesWithConstructors
    --         |> Dict.filter (\customTypeName _ -> not (context.isExposed && Set.member customTypeName context.exposedCustomTypesWithConstructors))
    --         |> Dict.values
    --         |> List.concatMap
    --             (\dict ->
    --                 dict
    --                     |> Dict.filter (\name _ -> not (Set.member name context.usedFunctionOrValues || (context.isExposed && Set.member name context.exposedCustomTypesWithConstructors)))
    --                     |> Dict.toList
    --                     |> List.map (\( _, node ) -> error node)
    --             )
    []



-- TYPE ANNOTATION UTILITARY FUNCTIONS


markPhantomTypesFromTypeSignatureAsUsed : Maybe (Node Signature) -> ModuleContext -> ModuleContext
markPhantomTypesFromTypeSignatureAsUsed maybeSignature context =
    let
        used : List String
        used =
            case maybeSignature of
                Just signature ->
                    signature
                        |> Node.value
                        |> .typeAnnotation
                        |> collectTypesUsedAsPhantomVariables context.phantomVariables

                Nothing ->
                    []
    in
    { context | usedFunctionOrValues = Set.union (Set.fromList used) context.usedFunctionOrValues }


collectGenericsFromTypeAnnotation : Node TypeAnnotation -> List String
collectGenericsFromTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectGenericsFromTypeAnnotation a ++ collectGenericsFromTypeAnnotation b

        TypeAnnotation.Typed nameNode params ->
            List.concatMap collectGenericsFromTypeAnnotation params

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.GenericRecord name list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.Tupled list ->
            List.concatMap collectGenericsFromTypeAnnotation list

        TypeAnnotation.GenericType var ->
            [ var ]

        TypeAnnotation.Unit ->
            []


collectTypesUsedAsPhantomVariables : List ( String, Int ) -> Node TypeAnnotation -> List String
collectTypesUsedAsPhantomVariables phantomVariables node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectTypesUsedAsPhantomVariables phantomVariables a
                ++ collectTypesUsedAsPhantomVariables phantomVariables b

        TypeAnnotation.Typed (Node.Node _ ( [], name )) params ->
            let
                typesUsedInThePhantomVariablePosition : List String
                typesUsedInThePhantomVariablePosition =
                    phantomVariables
                        |> List.filter (\( type_, _ ) -> type_ == name)
                        |> List.filterMap
                            (\( _, index ) ->
                                case listAtIndex index params |> Maybe.map Node.value of
                                    Just (TypeAnnotation.Typed (Node.Node _ ( [], typeName )) []) ->
                                        Just typeName

                                    _ ->
                                        Nothing
                            )
            in
            List.concat
                [ typesUsedInThePhantomVariablePosition
                , List.concatMap (collectTypesUsedAsPhantomVariables phantomVariables) params
                ]

        TypeAnnotation.Typed nameNode params ->
            List.concatMap (collectTypesUsedAsPhantomVariables phantomVariables) params

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectTypesUsedAsPhantomVariables phantomVariables)

        TypeAnnotation.GenericRecord name list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectTypesUsedAsPhantomVariables phantomVariables)

        TypeAnnotation.Tupled list ->
            List.concatMap (collectTypesUsedAsPhantomVariables phantomVariables) list

        TypeAnnotation.GenericType var ->
            []

        TypeAnnotation.Unit ->
            []


listAtIndex : Int -> List a -> Maybe a
listAtIndex index list =
    case ( index, list ) of
        ( 0, a :: [] ) ->
            Just a

        ( _, [] ) ->
            Nothing

        ( n, _ :: rest ) ->
            listAtIndex (n - 1) rest
