module NoUnused.CustomTypeConstructorArgs exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import List.Extra
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports arguments of custom type constructors that are never used.

    config =
        [ NoUnused.CustomTypeConstructorArgs.rule
        ]

Custom type constructors can contain data that is never extracted out of the constructor.
This rule will warn arguments that are always pattern matched using a wildcard (`_`).

For package projects, custom types whose constructors are exposed as part of the package API are not reported.

Note that this rule **may report false positives** if you compare custom types with the `==` or `/=` operators
(and never destructure the custom type), like when you do `value == Just 0`, or store them in lists for instance with
[`assoc-list`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest).
This rule attempts to detect when the custom type is used in comparisons, but it may still result in false positives.


## Fail

    type CustomType
      = CustomType Used Unused

    case customType of
      CustomType value _ -> value


## Success

    type CustomType
      = CustomType Used Unused

    case customType of
      CustomType value maybeUsed -> value


## When not to enable this rule?

If you like giving names to all arguments when pattern matching, then this rule will not find many problems.
This rule will work well when enabled along with [`NoUnused.Patterns`](./NoUnused-Patterns).

Also, if you like comparing custom types in the way described above, you might pass on this rule, or want to be very careful when enabling it.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.CustomTypeConstructorArgs
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructorArgs" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { exposedModules : Set ModuleName
    , customTypeArgs :
        Dict
            ModuleName
            { moduleKey : Rule.ModuleKey
            , args : Dict String (List Range)
            }
    , usedArguments : Dict ( ModuleName, String ) (Set Int)
    , customTypesNotToReport : Set ( ModuleName, String )
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , isModuleExposed : Bool
    , exposed : Exposing
    , customTypeArgs : List ( String, Dict String (List Range) )
    , usedArguments : Dict ( ModuleName, String ) (Set Int)
    , customTypesNotToReport : Set ( ModuleName, String )
    }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\node context -> ( [], moduleDefinitionVisitor node context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeEProject projectContext =
    case Maybe.map .project maybeEProject of
        Just (Elm.Project.Package package) ->
            let
                exposedModules : List Elm.Module.Name
                exposedModules =
                    case package.exposed of
                        Elm.Project.ExposedList list ->
                            list

                        Elm.Project.ExposedDict list ->
                            List.concatMap Tuple.second list

                exposedNames : Set ModuleName
                exposedNames =
                    exposedModules
                        |> List.map (Elm.Module.toString >> String.split ".")
                        |> Set.fromList
            in
            ( [], { projectContext | exposedModules = exposedNames } )

        _ ->
            ( [], projectContext )


initialProjectContext : ProjectContext
initialProjectContext =
    { exposedModules = Set.empty
    , customTypeArgs = Dict.empty
    , usedArguments = Dict.empty
    , customTypesNotToReport = Set.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable moduleName projectContext ->
            { lookupTable = lookupTable
            , isModuleExposed = Set.member moduleName projectContext.exposedModules
            , exposed = Exposing.Explicit []
            , customTypeArgs = []
            , usedArguments = Dict.empty
            , customTypesNotToReport = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleName


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleName moduleContext ->
            { exposedModules = Set.empty
            , customTypeArgs =
                Dict.singleton
                    moduleName
                    { moduleKey = moduleKey
                    , args = getNonExposedCustomTypes moduleContext
                    }
            , usedArguments = replaceLocalModuleNameForDict moduleName moduleContext.usedArguments
            , customTypesNotToReport = replaceLocalModuleNameForSet moduleName moduleContext.customTypesNotToReport
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleName


replaceLocalModuleNameForSet : ModuleName -> Set ( ModuleName, comparable ) -> Set ( ModuleName, comparable )
replaceLocalModuleNameForSet moduleName set =
    Set.map
        (\( moduleNameForType, name ) ->
            case moduleNameForType of
                [] ->
                    ( moduleName, name )

                _ ->
                    ( moduleNameForType, name )
        )
        set


replaceLocalModuleNameForDict : ModuleName -> Dict ( ModuleName, comparable ) b -> Dict ( ModuleName, comparable ) b
replaceLocalModuleNameForDict moduleName dict =
    Dict.foldl
        (\( moduleNameForType, name ) value acc ->
            case moduleNameForType of
                [] ->
                    Dict.insert ( moduleName, name ) value acc

                _ ->
                    Dict.insert ( moduleNameForType, name ) value acc
        )
        Dict.empty
        dict


getNonExposedCustomTypes : ModuleContext -> Dict String (List Range)
getNonExposedCustomTypes moduleContext =
    if moduleContext.isModuleExposed then
        case moduleContext.exposed of
            Exposing.All _ ->
                Dict.empty

            Exposing.Explicit list ->
                let
                    exposedCustomTypes : Set String
                    exposedCustomTypes =
                        list
                            |> List.filterMap
                                (\exposed ->
                                    case Node.value exposed of
                                        Exposing.TypeExpose { name, open } ->
                                            case open of
                                                Just _ ->
                                                    Just name

                                                Nothing ->
                                                    Nothing

                                        _ ->
                                            Nothing
                                )
                            |> Set.fromList
                in
                List.foldl
                    (\( typeName, args ) acc ->
                        if Set.member typeName exposedCustomTypes then
                            acc

                        else
                            Dict.union args acc
                    )
                    Dict.empty
                    moduleContext.customTypeArgs

    else
        List.foldl
            (\( _, args ) acc -> Dict.union args acc)
            Dict.empty
            moduleContext.customTypeArgs


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedModules = previousContext.exposedModules
    , customTypeArgs =
        Dict.union
            newContext.customTypeArgs
            previousContext.customTypeArgs
    , usedArguments =
        Dict.foldl
            (\key newSet acc ->
                case Dict.get key acc of
                    Just existingSet ->
                        Dict.insert key (Set.union newSet existingSet) acc

                    Nothing ->
                        Dict.insert key newSet acc
            )
            previousContext.usedArguments
            newContext.usedArguments
    , customTypesNotToReport = Set.union newContext.customTypesNotToReport previousContext.customTypesNotToReport
    }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ModuleContext
moduleDefinitionVisitor node moduleContext =
    { moduleContext | exposed = Module.exposingList (Node.value node) }


isNotNever : ModuleNameLookupTable -> Node TypeAnnotation -> Bool
isNotNever lookupTable node =
    case Node.value node of
        TypeAnnotation.Typed (Node neverRange ( _, "Never" )) [] ->
            ModuleNameLookupTable.moduleNameAt lookupTable neverRange /= Just [ "Basics" ]

        _ ->
            True



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            { context
                | usedArguments =
                    registerUsedPatterns
                        (collectUsedPatternsFromFunctionDeclaration context function)
                        context.usedArguments
            }

        Declaration.CustomTypeDeclaration typeDeclaration ->
            if List.isEmpty typeDeclaration.constructors then
                context

            else
                let
                    customTypeConstructors : Dict String (List Range)
                    customTypeConstructors =
                        List.foldl
                            (\(Node _ { name, arguments }) acc ->
                                Dict.insert
                                    (Node.value name)
                                    (createArguments context.lookupTable arguments)
                                    acc
                            )
                            Dict.empty
                            typeDeclaration.constructors
                in
                { context
                    | customTypeArgs = ( Node.value typeDeclaration.name, customTypeConstructors ) :: context.customTypeArgs
                }

        _ ->
            context


createArguments : ModuleNameLookupTable -> List (Node TypeAnnotation) -> List Range
createArguments lookupTable arguments =
    List.foldr
        (\argument acc ->
            if isNotNever lookupTable argument then
                Node.range argument :: acc

            else
                acc
        )
        []
        arguments


collectUsedPatternsFromFunctionDeclaration : ModuleContext -> Expression.Function -> List ( ( ModuleName, String ), Set Int )
collectUsedPatternsFromFunctionDeclaration context { declaration } =
    collectUsedCustomTypeArgs context.lookupTable (Node.value declaration).arguments



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionVisitor node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            let
                usedArguments : List ( ( ModuleName, String ), Set Int )
                usedArguments =
                    collectUsedCustomTypeArgs context.lookupTable (List.map Tuple.first cases)
            in
            { context | usedArguments = registerUsedPatterns usedArguments context.usedArguments }

        Expression.LetExpression { declarations } ->
            let
                usedArguments : List ( ( ModuleName, String ), Set Int )
                usedArguments =
                    List.concatMap
                        (\declaration ->
                            case Node.value declaration of
                                Expression.LetDestructuring pattern _ ->
                                    collectUsedCustomTypeArgs context.lookupTable [ pattern ]

                                Expression.LetFunction function ->
                                    collectUsedPatternsFromFunctionDeclaration context function
                        )
                        declarations
            in
            { context | usedArguments = registerUsedPatterns usedArguments context.usedArguments }

        Expression.LambdaExpression { args } ->
            { context
                | usedArguments =
                    registerUsedPatterns
                        (collectUsedCustomTypeArgs context.lookupTable args)
                        context.usedArguments
            }

        Expression.OperatorApplication operator _ left right ->
            if operator == "==" || operator == "/=" then
                let
                    customTypesNotToReport : Set ( ModuleName, String )
                    customTypesNotToReport =
                        findCustomTypes context.lookupTable [ left, right ]
                in
                { context | customTypesNotToReport = Set.union customTypesNotToReport context.customTypesNotToReport }

            else
                context

        Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: restOfArgs) ->
            if operator == "==" || operator == "/=" then
                let
                    customTypesNotToReport : Set ( ModuleName, String )
                    customTypesNotToReport =
                        findCustomTypes context.lookupTable restOfArgs
                in
                { context | customTypesNotToReport = Set.union customTypesNotToReport context.customTypesNotToReport }

            else
                context

        _ ->
            context


findCustomTypes : ModuleNameLookupTable -> List (Node Expression) -> Set ( ModuleName, String )
findCustomTypes lookupTable nodes =
    findCustomTypesHelp lookupTable nodes []
        |> Set.fromList


findCustomTypesHelp : ModuleNameLookupTable -> List (Node Expression) -> List ( ModuleName, String ) -> List ( ModuleName, String )
findCustomTypesHelp lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                Expression.FunctionOrValue rawModuleName functionName ->
                    if isCustomTypeConstructor functionName then
                        case ModuleNameLookupTable.moduleNameFor lookupTable node of
                            Just moduleName ->
                                findCustomTypesHelp lookupTable restOfNodes (( moduleName, functionName ) :: acc)

                            Nothing ->
                                findCustomTypesHelp lookupTable restOfNodes (( rawModuleName, functionName ) :: acc)

                    else
                        findCustomTypesHelp lookupTable restOfNodes acc

                Expression.TupledExpression expressions ->
                    findCustomTypesHelp lookupTable (expressions ++ restOfNodes) acc

                Expression.ParenthesizedExpression expression ->
                    findCustomTypesHelp lookupTable (expression :: restOfNodes) acc

                Expression.Application (((Node _ (Expression.FunctionOrValue _ functionName)) as first) :: expressions) ->
                    if isCustomTypeConstructor functionName then
                        findCustomTypesHelp lookupTable (first :: (expressions ++ restOfNodes)) acc

                    else
                        findCustomTypesHelp lookupTable restOfNodes acc

                Expression.OperatorApplication _ _ left right ->
                    findCustomTypesHelp lookupTable (left :: right :: restOfNodes) acc

                Expression.Negation expression ->
                    findCustomTypesHelp lookupTable (expression :: restOfNodes) acc

                Expression.ListExpr expressions ->
                    findCustomTypesHelp lookupTable (expressions ++ restOfNodes) acc

                _ ->
                    findCustomTypesHelp lookupTable restOfNodes acc


isCustomTypeConstructor : String -> Bool
isCustomTypeConstructor functionName =
    String.slice 0 1 functionName
        |> String.all Char.isUpper


registerUsedPatterns : List ( ( ModuleName, String ), Set Int ) -> Dict ( ModuleName, String ) (Set Int) -> Dict ( ModuleName, String ) (Set Int)
registerUsedPatterns newUsedArguments previouslyUsedArguments =
    List.foldl
        (\( key, usedPositions ) acc ->
            let
                previouslyUsedPositions : Set Int
                previouslyUsedPositions =
                    Dict.get key acc
                        |> Maybe.withDefault Set.empty
            in
            Dict.insert key (Set.union previouslyUsedPositions usedPositions) acc
        )
        previouslyUsedArguments
        newUsedArguments


collectUsedCustomTypeArgs : ModuleNameLookupTable -> List (Node Pattern) -> List ( ( ModuleName, String ), Set Int )
collectUsedCustomTypeArgs lookupTable nodes =
    collectUsedCustomTypeArgsHelp lookupTable nodes []


collectUsedCustomTypeArgsHelp : ModuleNameLookupTable -> List (Node Pattern) -> List ( ( ModuleName, String ), Set Int ) -> List ( ( ModuleName, String ), Set Int )
collectUsedCustomTypeArgsHelp lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        (Node range pattern) :: restOfNodes ->
            case pattern of
                Pattern.NamedPattern { name } args ->
                    let
                        newAcc : List ( ( ModuleName, String ), Set Int )
                        newAcc =
                            case ModuleNameLookupTable.moduleNameAt lookupTable range of
                                Just moduleName ->
                                    ( ( moduleName, name ), computeUsedPositions 0 args Set.empty ) :: acc

                                Nothing ->
                                    acc
                    in
                    collectUsedCustomTypeArgsHelp lookupTable (args ++ restOfNodes) newAcc

                Pattern.TuplePattern patterns ->
                    collectUsedCustomTypeArgsHelp lookupTable (patterns ++ restOfNodes) acc

                Pattern.ListPattern patterns ->
                    collectUsedCustomTypeArgsHelp lookupTable (patterns ++ restOfNodes) acc

                Pattern.UnConsPattern left right ->
                    collectUsedCustomTypeArgsHelp lookupTable (left :: right :: restOfNodes) acc

                Pattern.ParenthesizedPattern subPattern ->
                    collectUsedCustomTypeArgsHelp lookupTable (subPattern :: restOfNodes) acc

                Pattern.AsPattern subPattern _ ->
                    collectUsedCustomTypeArgsHelp lookupTable (subPattern :: restOfNodes) acc

                _ ->
                    collectUsedCustomTypeArgsHelp lookupTable restOfNodes acc


computeUsedPositions : Int -> List (Node Pattern) -> Set Int -> Set Int
computeUsedPositions index arguments acc =
    case arguments of
        [] ->
            acc

        arg :: restOfArgs ->
            let
                newAcc : Set Int
                newAcc =
                    if isWildcard arg then
                        acc

                    else
                        Set.insert index acc
            in
            computeUsedPositions (index + 1) restOfArgs newAcc


isWildcard : Node Pattern -> Bool
isWildcard node =
    case Node.value node of
        Pattern.AllPattern ->
            True

        Pattern.ParenthesizedPattern pattern ->
            isWildcard pattern

        _ ->
            False



-- FINAL EVALUATION


finalEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation context =
    Dict.foldl (finalEvaluationForSingleModule context) [] context.customTypeArgs


finalEvaluationForSingleModule : ProjectContext -> ModuleName -> { moduleKey : Rule.ModuleKey, args : Dict String (List Range) } -> List (Error { useErrorForModule : () }) -> List (Error { useErrorForModule : () })
finalEvaluationForSingleModule context moduleName { moduleKey, args } previousErrors =
    Dict.foldl
        (\name ranges acc ->
            let
                constructor : ( ModuleName, String )
                constructor =
                    ( moduleName, name )
            in
            if Set.member constructor context.customTypesNotToReport then
                acc

            else
                errorsForUnusedArguments context.usedArguments moduleKey constructor ranges acc
        )
        previousErrors
        args


errorsForUnusedArguments : Dict ( ModuleName, String ) (Set Int) -> Rule.ModuleKey -> ( ModuleName, String ) -> List Range -> List (Error anywhere) -> List (Error anywhere)
errorsForUnusedArguments usedArguments moduleKey constructor ranges acc =
    case Dict.get constructor usedArguments of
        Just usedArgumentPositions ->
            List.Extra.indexedFilterMap
                (\index range ->
                    if Set.member index usedArgumentPositions then
                        Nothing

                    else
                        Just (error moduleKey range)
                )
                0
                ranges
                acc

        Nothing ->
            List.map (error moduleKey) ranges ++ acc


error : Rule.ModuleKey -> Range -> Error anywhere
error moduleKey range =
    Rule.errorForModule moduleKey
        { message = "Argument is never extracted and therefore never used."
        , details =
            [ "This argument is never used. You should either use it somewhere, or remove it at the location I pointed at."
            ]
        }
        range
