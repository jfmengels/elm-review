module NoUnused.CustomTypeConstructors exposing (rule)

{-| Forbid having unused custom type constructors inside the project.

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
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import List.Extra
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid having unused custom type constructors.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoUnused.CustomTypeConstructors.rule []
        ]

Note that this rule reports any custom type constructor that isn't used
anywhere _in the project_.

If the project is a package and the module that declared the type is exposed and
the type's constructors are exposed, then the constructors will not be reported.

This does not prevent you from using phantom types.
I highly suggest chaning your phantom types to the following shape: `type TypeName = ConstructorName Never`.
This shape makes it obvious to tooling and readers that the type can't be created, so if it is used, it must be as a phantom type.

**Deprecated configuration for phantom types**

_I recommend changing your types like mentioned right above, and to configure the rule like `NoUnused.CustomTypeConstructors.rule []`.
I'll keep this section and configuration option around until the next major version comes out._

**Note**: At the time of writing, there may be cases where phantom types are not well detected.
When an opaque type is defined in a dependency, we don't know whether a type variable should be considered as a phantom type.

Therefore, sometimes this rule will need some help, by having you tell it what type variables of which type is a phantom type variable.
That's what the argument to the rule is for.

To explain that the `a` in `type Id a = Id String` from the `IdModule` module
corresponds to a phantom type variable, you would configure the rule like this:

    config =
        [ NoUnused.CustomTypeConstructors.rule
            [ { moduleName = "IdModule"
              , typeName = "Id"
              , index = 0 -- Position of the phantom variable in the type's arguments
              }
            ]
        ]

This rule could do a much better job than it currently does at figuring this out,
by following the definitions of custom types and type aliases, until it finds out that the type
variable is not used, or that it hits the limit related to dependencies described above.
In the meantime, you can configure the rule with all the phantom type exceptions.

**End of deprecated section**


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


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.CustomTypeConstructors
```

-}
rule : List { moduleName : String, typeName : String, index : Int } -> Rule
rule phantomTypes =
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructors" (initialProjectContext phantomTypes)
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema



-- MODULE VISITOR


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\node context -> ( [], moduleDefinitionVisitor node context ))
        |> Rule.withDeclarationListVisitor (\node context -> ( [], declarationListVisitor node context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))
        |> Rule.withCaseBranchEnterVisitor (\caseBlock casePattern context -> ( [], caseBranchEnterVisitor caseBlock casePattern context ))
        |> Rule.withCaseBranchExitVisitor (\caseBlock casePattern context -> ( [], caseBranchExitVisitor caseBlock casePattern context ))



-- CONTEXT


type alias ModuleNameAsString =
    String


type alias CustomTypeName =
    String


type alias ConstructorName =
    String


type ExposedConstructors
    = ExposedConstructors
        { moduleKey : Rule.ModuleKey
        , customTypes : Dict CustomTypeName (Dict ConstructorName ConstructorInformation)
        }


type alias ConstructorInformation =
    { name : String
    , rangeToReport : Range
    , rangeToRemove : Maybe Range
    }


type alias ProjectContext =
    { exposedModules : Set ModuleNameAsString
    , declaredConstructors : Dict ModuleNameAsString ExposedConstructors
    , usedConstructors : Dict ModuleNameAsString (Set ConstructorName)
    , phantomVariables : Dict ModuleName (List ( CustomTypeName, Int ))
    , wasUsedInLocationThatNeedsItself : Set ( ModuleNameAsString, ConstructorName )
    , wasUsedInComparisons : Set ( ModuleNameAsString, ConstructorName )
    , wasUsedInOtherModules : Set ( ModuleNameAsString, ConstructorName )
    , fixesForRemovingConstructor : Dict ( ModuleNameAsString, ConstructorName ) (List Fix)
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , exposedCustomTypesWithConstructors : Set CustomTypeName
    , isExposed : Bool
    , exposesEverything : Bool
    , exposedConstructors : Dict ModuleNameAsString ExposedConstructors
    , declaredTypesWithConstructors : Dict CustomTypeName (Dict ConstructorName ConstructorInformation)
    , usedFunctionsOrValues : Dict ModuleNameAsString (Set ConstructorName)
    , phantomVariables : Dict ModuleName (List ( CustomTypeName, Int ))
    , constructorsToIgnore : List (Set ( ModuleName, ConstructorName ))
    , wasUsedInLocationThatNeedsItself : Set ( ModuleNameAsString, ConstructorName )
    , wasUsedInComparisons : Set ( ModuleNameAsString, ConstructorName )
    , fixesForRemovingConstructor : Dict ConstructorName (List Fix)
    , wasUsedInOtherModules : Set ( ModuleNameAsString, ConstructorName )
    , ignoredComparisonRanges : List Range
    }


initialProjectContext : List { moduleName : String, typeName : String, index : Int } -> ProjectContext
initialProjectContext phantomTypes =
    { exposedModules = Set.empty
    , declaredConstructors = Dict.empty
    , usedConstructors = Dict.empty
    , phantomVariables =
        List.foldl
            (\{ moduleName, typeName, index } dict ->
                updateToAdd (String.split "." moduleName) ( typeName, index ) dict
            )
            Dict.empty
            phantomTypes
    , wasUsedInLocationThatNeedsItself = Set.empty
    , wasUsedInComparisons = Set.empty
    , wasUsedInOtherModules = Set.empty
    , fixesForRemovingConstructor = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable moduleName projectContext ->
            { lookupTable = lookupTable
            , exposedCustomTypesWithConstructors = Set.empty
            , isExposed = Set.member (String.join "." moduleName) projectContext.exposedModules
            , exposedConstructors = projectContext.declaredConstructors
            , exposesEverything = False
            , declaredTypesWithConstructors = Dict.empty
            , usedFunctionsOrValues = Dict.empty
            , phantomVariables = projectContext.phantomVariables
            , constructorsToIgnore = []
            , wasUsedInLocationThatNeedsItself = Set.empty
            , wasUsedInComparisons = Set.empty
            , wasUsedInOtherModules = Set.empty
            , fixesForRemovingConstructor = Dict.empty
            , ignoredComparisonRanges = []
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleName


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleName moduleContext ->
            let
                localUsed : Set ConstructorName
                localUsed =
                    moduleContext.usedFunctionsOrValues
                        |> Dict.get ""
                        |> Maybe.withDefault Set.empty

                localPhantomTypes : List ( CustomTypeName, Int )
                localPhantomTypes =
                    moduleContext.phantomVariables
                        |> Dict.get []
                        |> Maybe.withDefault []

                moduleNameAsString : ModuleNameAsString
                moduleNameAsString =
                    String.join "." moduleName
            in
            { exposedModules = Set.empty
            , declaredConstructors =
                if moduleContext.isExposed then
                    if moduleContext.exposesEverything then
                        Dict.empty

                    else
                        Dict.singleton
                            moduleNameAsString
                            (ExposedConstructors
                                { moduleKey = moduleKey
                                , customTypes =
                                    moduleContext.declaredTypesWithConstructors
                                        |> Dict.filter (\typeName _ -> not <| Set.member typeName moduleContext.exposedCustomTypesWithConstructors)
                                }
                            )

                else
                    Dict.singleton
                        moduleNameAsString
                        (ExposedConstructors
                            { moduleKey = moduleKey
                            , customTypes = moduleContext.declaredTypesWithConstructors
                            }
                        )
            , usedConstructors =
                moduleContext.usedFunctionsOrValues
                    |> Dict.remove ""
                    |> Dict.insert moduleNameAsString localUsed
            , phantomVariables = Dict.singleton moduleName localPhantomTypes
            , wasUsedInLocationThatNeedsItself =
                Set.map
                    (\(( moduleName_, constructorName ) as untouched) ->
                        if moduleName_ == "" then
                            ( moduleNameAsString, constructorName )

                        else
                            untouched
                    )
                    moduleContext.wasUsedInLocationThatNeedsItself
            , wasUsedInComparisons =
                Set.map
                    (\(( moduleName_, constructorName ) as untouched) ->
                        if moduleName_ == "" then
                            ( moduleNameAsString, constructorName )

                        else
                            untouched
                    )
                    moduleContext.wasUsedInComparisons
            , wasUsedInOtherModules =
                List.foldl
                    (\( moduleName_, constructors ) acc ->
                        Set.union
                            (Set.map (Tuple.pair moduleName_) constructors)
                            acc
                    )
                    moduleContext.wasUsedInOtherModules
                    -- TODO add test to make sure we don't fix something that is pattern matched in other modules
                    (Dict.toList <| Dict.remove "" moduleContext.usedFunctionsOrValues)
            , fixesForRemovingConstructor =
                mapDictKeys
                    (\constructorName ->
                        ( moduleNameAsString, constructorName )
                    )
                    moduleContext.fixesForRemovingConstructor
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedModules = previousContext.exposedModules
    , declaredConstructors = Dict.union newContext.declaredConstructors previousContext.declaredConstructors
    , usedConstructors =
        Dict.foldl
            (\key newSet acc ->
                case Dict.get key acc of
                    Just existingSet ->
                        Dict.insert key (Set.union newSet existingSet) acc

                    Nothing ->
                        Dict.insert key newSet acc
            )
            previousContext.usedConstructors
            newContext.usedConstructors
    , phantomVariables = Dict.union newContext.phantomVariables previousContext.phantomVariables
    , wasUsedInLocationThatNeedsItself = Set.union newContext.wasUsedInLocationThatNeedsItself previousContext.wasUsedInLocationThatNeedsItself
    , wasUsedInComparisons = Set.union newContext.wasUsedInComparisons previousContext.wasUsedInComparisons
    , wasUsedInOtherModules = Set.union newContext.wasUsedInOtherModules previousContext.wasUsedInOtherModules
    , fixesForRemovingConstructor =
        Dict.foldl
            (\key newFixes acc ->
                case Dict.get key acc of
                    Just existingFixes ->
                        Dict.insert key (List.foldl (::) existingFixes newFixes) acc

                    Nothing ->
                        Dict.insert key newFixes acc
            )
            previousContext.fixesForRemovingConstructor
            newContext.fixesForRemovingConstructor
    }


updateToAdd : comparable -> a -> Dict comparable (List a) -> Dict comparable (List a)
updateToAdd key value dict =
    Dict.update
        key
        (\existingValues ->
            case existingValues of
                Just values ->
                    Just (value :: values)

                Nothing ->
                    Just [ value ]
        )
        dict


updateToInsert : comparable1 -> comparable2 -> Dict comparable1 (Set comparable2) -> Dict comparable1 (Set comparable2)
updateToInsert key value dict =
    Dict.update
        key
        (\existingValues ->
            case existingValues of
                Just values ->
                    Just (Set.insert value values)

                Nothing ->
                    Just (Set.singleton value)
        )
        dict



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
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
            ( [], { projectContext | exposedModules = exposedNames } )

        Just (Elm.Project.Application _) ->
            ( [], projectContext )

        Nothing ->
            ( [], projectContext )



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ModuleContext
moduleDefinitionVisitor moduleNode context =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            { context | exposesEverything = True }

        Exposing.Explicit list ->
            let
                exposedCustomTypesWithConstructors : Set String
                exposedCustomTypesWithConstructors =
                    List.foldl
                        (\node acc ->
                            case Node.value node of
                                Exposing.TypeExpose { name } ->
                                    Set.insert name acc

                                _ ->
                                    acc
                        )
                        context.exposedCustomTypesWithConstructors
                        list
            in
            { context | exposedCustomTypesWithConstructors = exposedCustomTypesWithConstructors }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor nodes context =
    List.foldl register context nodes


register : Node Declaration -> ModuleContext -> ModuleContext
register node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, generics, constructors } ->
            let
                arguments : List (Node TypeAnnotation)
                arguments =
                    List.concatMap (\(Node _ value) -> value.arguments) constructors

                nonPhantomVariables : Set String
                nonPhantomVariables =
                    collectGenericsFromTypeAnnotation arguments Set.empty

                newPhantomVariables : Dict (List String) (List ( String, Int ))
                newPhantomVariables =
                    Dict.update
                        []
                        (\maybeSet ->
                            let
                                previousPhantomVariables : List ( String, Int )
                                previousPhantomVariables =
                                    case maybeSet of
                                        Just old ->
                                            old

                                        Nothing ->
                                            []
                            in
                            List.Extra.indexedFilterMap
                                (\indexOfPhantomVariable (Node _ genericName) ->
                                    if Set.member genericName nonPhantomVariables then
                                        Nothing

                                    else
                                        Just ( Node.value name, indexOfPhantomVariable )
                                )
                                0
                                generics
                                previousPhantomVariables
                                |> Just
                        )
                        context.phantomVariables
            in
            { context | phantomVariables = newPhantomVariables }

        _ ->
            context



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            if isPhantomCustomType context.lookupTable (Node.value name) constructors then
                context

            else
                { context
                    | declaredTypesWithConstructors =
                        Dict.insert
                            (Node.value name)
                            (constructorsForCustomType constructors)
                            context.declaredTypesWithConstructors
                }

        Declaration.FunctionDeclaration function ->
            markPhantomTypesFromTypeAnnotationAsUsed
                (Maybe.map (\(Node _ value) -> value.typeAnnotation) function.signature)
                { context | ignoredComparisonRanges = [] }

        Declaration.AliasDeclaration { typeAnnotation } ->
            markPhantomTypesFromTypeAnnotationAsUsed (Just typeAnnotation) context

        _ ->
            context


constructorsForCustomType : List (Node Type.ValueConstructor) -> Dict String ConstructorInformation
constructorsForCustomType constructors =
    let
        constructorsAndNext : List ( Maybe (Node Type.ValueConstructor), Node Type.ValueConstructor )
        constructorsAndNext =
            List.map2 Tuple.pair
                (List.map Just (List.drop 1 constructors) ++ [ Nothing ])
                constructors
    in
    List.foldl
        (\( next, constructor ) ( prev, dict ) ->
            let
                nameNode : Node String
                nameNode =
                    (Node.value constructor).name

                constructorName : String
                constructorName =
                    Node.value nameNode

                constructorInformation : ConstructorInformation
                constructorInformation =
                    { name = constructorName
                    , rangeToReport = Node.range nameNode
                    , rangeToRemove = findRangeToRemove prev constructor next
                    }
            in
            ( Just constructor
            , Dict.insert
                constructorName
                constructorInformation
                dict
            )
        )
        ( Nothing, Dict.empty )
        constructorsAndNext
        |> Tuple.second


findRangeToRemove : Maybe (Node a) -> Node Type.ValueConstructor -> Maybe (Node c) -> Maybe { start : Elm.Syntax.Range.Location, end : Elm.Syntax.Range.Location }
findRangeToRemove previousConstructor constructor nextConstructor =
    case previousConstructor of
        Just prev ->
            Just
                { start = (Node.range prev).end
                , end = (Node.range constructor).end
                }

        Nothing ->
            case nextConstructor of
                Just next ->
                    Just
                        { start = constructor |> Node.value |> .name |> Node.range |> .start
                        , end = (Node.range next).start
                        }

                Nothing ->
                    Nothing


isPhantomCustomType : ModuleNameLookupTable -> String -> List (Node Type.ValueConstructor) -> Bool
isPhantomCustomType lookupTable typeName constructors =
    case constructors of
        [ Node _ constructor ] ->
            case constructor.arguments of
                [ arg ] ->
                    isNeverOrItself lookupTable typeName arg

                _ ->
                    False

        _ ->
            False


isNeverOrItself : ModuleNameLookupTable -> String -> Node TypeAnnotation -> Bool
isNeverOrItself lookupTable typeName node =
    case Node.value node of
        TypeAnnotation.Typed (Node neverRange ( _, "Never" )) [] ->
            ModuleNameLookupTable.moduleNameAt lookupTable neverRange == Just [ "Basics" ]

        TypeAnnotation.Typed (Node _ ( [], argName )) [] ->
            typeName == argName

        _ ->
            False



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionVisitor node moduleContext =
    case Node.value node of
        Expression.FunctionOrValue _ name ->
            case ModuleNameLookupTable.moduleNameFor moduleContext.lookupTable node of
                Just moduleName ->
                    registerUsedFunctionOrValue (Node.range node) moduleName name moduleContext

                Nothing ->
                    moduleContext

        Expression.OperatorApplication operator _ left right ->
            if operator == "==" || operator == "/=" then
                let
                    { fromThisModule, fromOtherModules } =
                        findConstructors moduleContext.lookupTable [ left, right ] moduleContext.wasUsedInOtherModules

                    replacement : String
                    replacement =
                        if operator == "==" then
                            "False"

                        else
                            "True"

                    fixes : Dict ConstructorName (List Fix)
                    fixes =
                        List.foldl
                            (\( _, constructor ) dict ->
                                updateToAdd constructor (Fix.replaceRangeBy (Node.range node) replacement) dict
                            )
                            moduleContext.fixesForRemovingConstructor
                            fromThisModule
                in
                { moduleContext
                    | ignoredComparisonRanges = staticRanges [ node ] moduleContext.ignoredComparisonRanges
                    , fixesForRemovingConstructor = fixes
                    , wasUsedInOtherModules = fromOtherModules
                }

            else
                moduleContext

        Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: arguments) ->
            if operator == "==" || operator == "/=" then
                let
                    { fromThisModule, fromOtherModules } =
                        findConstructors moduleContext.lookupTable arguments moduleContext.wasUsedInOtherModules

                    replacementBoolean : String
                    replacementBoolean =
                        if operator == "==" then
                            "False"

                        else
                            "True"

                    replacement : String
                    replacement =
                        if List.length arguments == 2 then
                            replacementBoolean

                        else
                            "always " ++ replacementBoolean

                    fixes : Dict ConstructorName (List Fix)
                    fixes =
                        List.foldl
                            (\( _, constructor ) dict ->
                                updateToAdd constructor (Fix.replaceRangeBy (Node.range node) replacement) dict
                            )
                            moduleContext.fixesForRemovingConstructor
                            fromThisModule
                in
                { moduleContext
                    | ignoredComparisonRanges = staticRanges [ node ] moduleContext.ignoredComparisonRanges
                    , fixesForRemovingConstructor = fixes
                    , wasUsedInOtherModules = fromOtherModules
                }

            else
                moduleContext

        Expression.LetExpression { declarations } ->
            List.foldl
                (\declaration ctx ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            markPhantomTypesFromTypeAnnotationAsUsed
                                (Maybe.map (\(Node _ value) -> value.typeAnnotation) function.signature)
                                ctx

                        Expression.LetDestructuring _ _ ->
                            ctx
                )
                moduleContext
                declarations

        _ ->
            moduleContext


caseBranchEnterVisitor : Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> ModuleContext -> ModuleContext
caseBranchEnterVisitor caseExpression ( casePattern, body ) moduleContext =
    let
        previousLocation : Maybe Elm.Syntax.Range.Location
        previousLocation =
            findEndLocationOfPreviousElement (Node.value caseExpression).cases (Node.range casePattern) Nothing

        constructors : { fromThisModule : Set ConstructorName, fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
        constructors =
            constructorsInPattern moduleContext.lookupTable [ casePattern ] { fromThisModule = Set.empty, fromOtherModules = Set.empty }

        fixes : Dict ConstructorName (List Fix)
        fixes =
            List.foldl
                (\constructorName acc ->
                    let
                        fix : Fix
                        fix =
                            Fix.removeRange
                                { start = Maybe.withDefault (Node.range casePattern).start previousLocation
                                , end = (Node.range body).end
                                }
                    in
                    updateToAdd constructorName fix acc
                )
                moduleContext.fixesForRemovingConstructor
                (Set.toList constructors.fromThisModule)

        constructorsToIgnore : Set ( ModuleName, ConstructorName )
        constructorsToIgnore =
            Set.union
                (Set.map (\( moduleName, constructorName ) -> ( String.split "." moduleName, constructorName )) constructors.fromOtherModules)
                (Set.map (\constructorName -> ( [], constructorName )) constructors.fromThisModule)
    in
    { moduleContext
        | wasUsedInOtherModules = Set.union constructors.fromOtherModules moduleContext.wasUsedInOtherModules
        , constructorsToIgnore = constructorsToIgnore :: moduleContext.constructorsToIgnore
        , fixesForRemovingConstructor = fixes
    }


caseBranchExitVisitor : Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> ModuleContext -> ModuleContext
caseBranchExitVisitor _ _ moduleContext =
    { moduleContext | constructorsToIgnore = List.drop 1 moduleContext.constructorsToIgnore }


findEndLocationOfPreviousElement : List ( Node a, Node b ) -> Range -> Maybe Elm.Syntax.Range.Location -> Maybe Elm.Syntax.Range.Location
findEndLocationOfPreviousElement nodes nodeRange previousRangeEnd =
    case nodes of
        ( Node patternRange _, Node bodyRange _ ) :: tail ->
            if patternRange == nodeRange then
                previousRangeEnd

            else
                findEndLocationOfPreviousElement tail nodeRange (Just bodyRange.end)

        [] ->
            Nothing


staticRanges : List (Node Expression) -> List Range -> List Range
staticRanges nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                Expression.FunctionOrValue _ _ ->
                    staticRanges restOfNodes (Node.range node :: acc)

                Expression.Application ((Node _ (Expression.FunctionOrValue _ name)) :: restOfArgs) ->
                    if isCapitalized name then
                        staticRanges (restOfArgs ++ restOfNodes) (Node.range node :: acc)

                    else
                        staticRanges restOfNodes acc

                Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: restOfArgs) ->
                    if List.member operator [ "+", "-", "==", "/=" ] then
                        staticRanges (restOfArgs ++ restOfNodes) acc

                    else
                        staticRanges restOfNodes acc

                Expression.OperatorApplication operator _ left right ->
                    if List.member operator [ "+", "-", "==", "/=" ] then
                        staticRanges (left :: right :: restOfNodes) acc

                    else
                        staticRanges restOfNodes acc

                Expression.ListExpr subNodes ->
                    staticRanges (subNodes ++ restOfNodes) acc

                Expression.TupledExpression subNodes ->
                    staticRanges (subNodes ++ restOfNodes) acc

                Expression.ParenthesizedExpression expr ->
                    staticRanges (expr :: restOfNodes) acc

                Expression.RecordExpr fields ->
                    let
                        newNodes : List (Node Expression)
                        newNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    staticRanges (newNodes ++ restOfNodes) acc

                Expression.RecordUpdateExpression _ fields ->
                    let
                        newNodes : List (Node Expression)
                        newNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    staticRanges (newNodes ++ restOfNodes) acc

                Expression.RecordAccess expr _ ->
                    staticRanges (expr :: restOfNodes) acc

                _ ->
                    staticRanges restOfNodes acc


findConstructors : ModuleNameLookupTable -> List (Node Expression) -> Set ( ModuleNameAsString, ConstructorName ) -> { fromThisModule : List ( ModuleNameAsString, ConstructorName ), fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
findConstructors lookupTable nodes fromOtherModulesBase =
    findConstructorsHelp lookupTable nodes { fromThisModule = [], fromOtherModules = fromOtherModulesBase }


findConstructorsHelp :
    ModuleNameLookupTable
    -> List (Node Expression)
    -> { fromThisModule : List ( ModuleNameAsString, ConstructorName ), fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
    -> { fromThisModule : List ( ModuleNameAsString, ConstructorName ), fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
findConstructorsHelp lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                Expression.FunctionOrValue _ name ->
                    if isCapitalized name then
                        findConstructorsHelp
                            lookupTable
                            restOfNodes
                            (addElementToUniqueList lookupTable node name acc)

                    else
                        findConstructorsHelp lookupTable restOfNodes acc

                Expression.Application ((Node _ (Expression.FunctionOrValue _ name)) :: restOfArgs) ->
                    if isCapitalized name then
                        findConstructorsHelp
                            lookupTable
                            (restOfArgs ++ restOfNodes)
                            (addElementToUniqueList lookupTable node name acc)

                    else
                        findConstructorsHelp lookupTable restOfNodes acc

                Expression.OperatorApplication operator _ left right ->
                    if List.member operator [ "+", "-" ] then
                        findConstructorsHelp lookupTable (left :: right :: restOfNodes) acc

                    else
                        findConstructorsHelp lookupTable restOfNodes acc

                Expression.ListExpr subNodes ->
                    findConstructorsHelp lookupTable (subNodes ++ restOfNodes) acc

                Expression.TupledExpression subNodes ->
                    findConstructorsHelp lookupTable (subNodes ++ restOfNodes) acc

                Expression.ParenthesizedExpression expr ->
                    findConstructorsHelp lookupTable (expr :: restOfNodes) acc

                Expression.RecordExpr fields ->
                    let
                        expressions : List (Node Expression)
                        expressions =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    findConstructorsHelp lookupTable (expressions ++ restOfNodes) acc

                Expression.RecordUpdateExpression _ fields ->
                    let
                        expressions : List (Node Expression)
                        expressions =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    findConstructorsHelp lookupTable (expressions ++ restOfNodes) acc

                Expression.RecordAccess expr _ ->
                    findConstructorsHelp lookupTable (expr :: restOfNodes) acc

                _ ->
                    findConstructorsHelp lookupTable restOfNodes acc


addElementToUniqueList :
    ModuleNameLookupTable
    -> Node Expression
    -> ConstructorName
    -> { fromThisModule : List ( ModuleNameAsString, ConstructorName ), fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
    -> { fromThisModule : List ( ModuleNameAsString, ConstructorName ), fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
addElementToUniqueList lookupTable node name acc =
    case ModuleNameLookupTable.moduleNameFor lookupTable node of
        Just realModuleName ->
            let
                moduleName : ModuleNameAsString
                moduleName =
                    String.join "." realModuleName

                key : ( ModuleNameAsString, ConstructorName )
                key =
                    ( moduleName, name )
            in
            if moduleName == "" then
                if List.member key acc.fromThisModule then
                    acc

                else
                    { fromThisModule = key :: acc.fromThisModule
                    , fromOtherModules = acc.fromOtherModules
                    }

            else
                { fromThisModule = acc.fromThisModule
                , fromOtherModules = Set.insert key acc.fromOtherModules
                }

        Nothing ->
            acc


constructorsInPattern : ModuleNameLookupTable -> List (Node Pattern) -> { fromThisModule : Set ConstructorName, fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) } -> { fromThisModule : Set ConstructorName, fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
constructorsInPattern lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                Pattern.NamedPattern qualifiedNameRef patterns ->
                    let
                        newAcc : { fromThisModule : Set ConstructorName, fromOtherModules : Set ( ModuleNameAsString, ConstructorName ) }
                        newAcc =
                            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                                Just [] ->
                                    { fromThisModule = Set.insert qualifiedNameRef.name acc.fromThisModule
                                    , fromOtherModules = acc.fromOtherModules
                                    }

                                Just realModuleName ->
                                    { fromThisModule = acc.fromThisModule
                                    , fromOtherModules = Set.insert ( String.join "." realModuleName, qualifiedNameRef.name ) acc.fromOtherModules
                                    }

                                Nothing ->
                                    acc
                    in
                    constructorsInPattern lookupTable (patterns ++ restOfNodes) newAcc

                Pattern.TuplePattern patterns ->
                    constructorsInPattern lookupTable (patterns ++ restOfNodes) acc

                Pattern.UnConsPattern left right ->
                    constructorsInPattern lookupTable (left :: right :: restOfNodes) acc

                Pattern.ListPattern patterns ->
                    constructorsInPattern lookupTable (patterns ++ restOfNodes) acc

                Pattern.AsPattern pattern _ ->
                    constructorsInPattern lookupTable (pattern :: restOfNodes) acc

                Pattern.ParenthesizedPattern pattern ->
                    constructorsInPattern lookupTable (pattern :: restOfNodes) acc

                _ ->
                    constructorsInPattern lookupTable restOfNodes acc


registerUsedFunctionOrValue : Range -> ModuleName -> ConstructorName -> ModuleContext -> ModuleContext
registerUsedFunctionOrValue range moduleName name moduleContext =
    if not (isCapitalized name) then
        moduleContext

    else if List.member range moduleContext.ignoredComparisonRanges then
        { moduleContext
            | wasUsedInComparisons =
                Set.insert
                    ( String.join "." moduleName, name )
                    moduleContext.wasUsedInComparisons
        }

    else if List.any (Set.member ( moduleName, name )) moduleContext.constructorsToIgnore then
        { moduleContext | wasUsedInLocationThatNeedsItself = Set.insert ( String.join "." moduleName, name ) moduleContext.wasUsedInLocationThatNeedsItself }

    else
        { moduleContext | usedFunctionsOrValues = updateToInsert (String.join "." moduleName) name moduleContext.usedFunctionsOrValues }


isCapitalized : String -> Bool
isCapitalized name =
    case String.uncons name of
        Just ( char, _ ) ->
            Char.isUpper char

        Nothing ->
            False



-- FINAL PROJECT EVALUATION


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    projectContext.declaredConstructors
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, ExposedConstructors { moduleKey, customTypes } ) ->
                let
                    usedConstructors : Set ConstructorName
                    usedConstructors =
                        Dict.get moduleName projectContext.usedConstructors
                            |> Maybe.withDefault Set.empty
                in
                customTypes
                    |> Dict.values
                    |> List.concatMap
                        (\constructors ->
                            constructors
                                |> Dict.filter (\constructorName _ -> not <| Set.member constructorName usedConstructors)
                                |> Dict.values
                                |> List.map
                                    (\constructorInformation ->
                                        errorForModule
                                            moduleKey
                                            { wasUsedInLocationThatNeedsItself = Set.member ( moduleName, constructorInformation.name ) projectContext.wasUsedInLocationThatNeedsItself
                                            , wasUsedInComparisons = Set.member ( moduleName, constructorInformation.name ) projectContext.wasUsedInComparisons
                                            , isUsedInOtherModules = Set.member ( moduleName, constructorInformation.name ) projectContext.wasUsedInOtherModules
                                            , fixesForRemovingConstructor = Dict.get ( moduleName, constructorInformation.name ) projectContext.fixesForRemovingConstructor |> Maybe.withDefault []
                                            }
                                            constructorInformation
                                    )
                        )
            )



-- ERROR


errorInformation : { wasUsedInLocationThatNeedsItself : Bool, wasUsedInComparisons : Bool } -> String -> { message : String, details : List String }
errorInformation { wasUsedInLocationThatNeedsItself, wasUsedInComparisons } name =
    { message = "Type constructor `" ++ name ++ "` is not used."
    , details =
        [ ( defaultDetails, True )
        , ( "I found it used in comparisons, but since it is never created anywhere, all of those can be evaluated to False (for (==), True for (/=)).", wasUsedInComparisons )
        , ( "The only locations where I found it being created require already having one.", wasUsedInLocationThatNeedsItself )
        ]
            |> List.filter Tuple.second
            |> List.map Tuple.first
    }


defaultDetails : String
defaultDetails =
    "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."


errorForModule :
    Rule.ModuleKey
    ->
        { wasUsedInLocationThatNeedsItself : Bool
        , wasUsedInComparisons : Bool
        , isUsedInOtherModules : Bool
        , fixesForRemovingConstructor : List Fix
        }
    -> ConstructorInformation
    -> Error scope
errorForModule moduleKey params constructorInformation =
    Rule.errorForModuleWithFix
        moduleKey
        (errorInformation
            { wasUsedInLocationThatNeedsItself = params.wasUsedInLocationThatNeedsItself
            , wasUsedInComparisons = params.wasUsedInComparisons
            }
            constructorInformation.name
        )
        constructorInformation.rangeToReport
        (case constructorInformation.rangeToRemove of
            Just rangeToRemove ->
                if params.isUsedInOtherModules then
                    []

                else
                    Fix.removeRange rangeToRemove :: params.fixesForRemovingConstructor

            Nothing ->
                []
        )



-- TYPE ANNOTATION UTILITY FUNCTIONS


markPhantomTypesFromTypeAnnotationAsUsed : Maybe (Node TypeAnnotation) -> ModuleContext -> ModuleContext
markPhantomTypesFromTypeAnnotationAsUsed maybeTypeAnnotation moduleContext =
    case maybeTypeAnnotation of
        Just typeAnnotation ->
            let
                usedFunctionsOrValues : Dict ModuleNameAsString (Set ConstructorName)
                usedFunctionsOrValues =
                    collectTypesUsedAsPhantomVariables
                        moduleContext
                        moduleContext.phantomVariables
                        [ typeAnnotation ]
                        moduleContext.usedFunctionsOrValues
            in
            { moduleContext | usedFunctionsOrValues = usedFunctionsOrValues }

        Nothing ->
            moduleContext


collectGenericsFromTypeAnnotation : List (Node TypeAnnotation) -> Set String -> Set String
collectGenericsFromTypeAnnotation nodes acc =
    case nodes of
        [] ->
            acc

        (Node _ node) :: restOfNodes ->
            case node of
                TypeAnnotation.FunctionTypeAnnotation a b ->
                    collectGenericsFromTypeAnnotation (a :: b :: restOfNodes) acc

                TypeAnnotation.Typed _ params ->
                    collectGenericsFromTypeAnnotation (params ++ restOfNodes) acc

                TypeAnnotation.Record fields ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectGenericsFromTypeAnnotation (subNodes ++ restOfNodes) acc

                TypeAnnotation.GenericRecord (Node _ var) (Node _ fields) ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectGenericsFromTypeAnnotation (subNodes ++ restOfNodes) (Set.insert var acc)

                TypeAnnotation.Tupled list ->
                    collectGenericsFromTypeAnnotation (list ++ restOfNodes) acc

                TypeAnnotation.GenericType var ->
                    collectGenericsFromTypeAnnotation restOfNodes (Set.insert var acc)

                TypeAnnotation.Unit ->
                    collectGenericsFromTypeAnnotation restOfNodes acc


collectTypesUsedAsPhantomVariables : ModuleContext -> Dict ModuleName (List ( CustomTypeName, Int )) -> List (Node TypeAnnotation) -> Dict ModuleNameAsString (Set ConstructorName) -> Dict ModuleNameAsString (Set ConstructorName)
collectTypesUsedAsPhantomVariables moduleContext phantomVariables nodes used =
    case nodes of
        [] ->
            used

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.FunctionTypeAnnotation a b ->
                    collectTypesUsedAsPhantomVariables moduleContext phantomVariables (a :: b :: restOfNodes) used

                TypeAnnotation.Typed (Node.Node typeRange ( _, name )) params ->
                    case
                        ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable typeRange
                            |> Maybe.andThen (\moduleNameOfPhantomContainer -> Dict.get moduleNameOfPhantomContainer phantomVariables)
                    of
                        Just things ->
                            let
                                newUsed : Dict ModuleNameAsString (Set ConstructorName)
                                newUsed =
                                    List.foldl
                                        (\( type_, index ) acc ->
                                            if type_ /= name then
                                                acc

                                            else
                                                case listAtIndex index params |> Maybe.map Node.value of
                                                    Just (TypeAnnotation.Typed (Node.Node subTypeRange ( _, typeName )) _) ->
                                                        case ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable subTypeRange of
                                                            Just moduleNameOfPhantomVariable ->
                                                                updateToInsert (String.join "." moduleNameOfPhantomVariable) typeName acc

                                                            Nothing ->
                                                                acc

                                                    _ ->
                                                        acc
                                        )
                                        used
                                        things
                            in
                            collectTypesUsedAsPhantomVariables
                                moduleContext
                                phantomVariables
                                (params ++ restOfNodes)
                                newUsed

                        Nothing ->
                            collectTypesUsedAsPhantomVariables
                                moduleContext
                                phantomVariables
                                (params ++ restOfNodes)
                                used

                TypeAnnotation.Record fields ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectTypesUsedAsPhantomVariables moduleContext phantomVariables (subNodes ++ restOfNodes) used

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectTypesUsedAsPhantomVariables moduleContext phantomVariables (subNodes ++ restOfNodes) used

                TypeAnnotation.Tupled list ->
                    collectTypesUsedAsPhantomVariables moduleContext phantomVariables (list ++ restOfNodes) used

                TypeAnnotation.GenericType _ ->
                    collectTypesUsedAsPhantomVariables moduleContext phantomVariables restOfNodes used

                TypeAnnotation.Unit ->
                    collectTypesUsedAsPhantomVariables moduleContext phantomVariables restOfNodes used


listAtIndex : Int -> List a -> Maybe a
listAtIndex index list =
    case ( index, list ) of
        ( 0, a :: [] ) ->
            Just a

        ( _, [] ) ->
            Nothing

        ( n, _ :: rest ) ->
            listAtIndex (n - 1) rest


mapDictKeys : (comparable -> comparable1) -> Dict comparable v -> Dict comparable1 v
mapDictKeys keyMapper dict =
    Dict.foldl
        (\key value acc -> Dict.insert (keyMapper key) value acc)
        Dict.empty
        dict
