module TypeInference exposing
    ( ModuleContext
    , ProjectContext
    , addProjectVisitors
    , foldProjectContexts
    , fromModuleToProject
    , fromProjectToModule
    , inferType
    , initialProjectContext
    )

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import ElmCorePrelude
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Rule as Rule
import Set exposing (Set)
import TypeInference.Binop as Binop
import TypeInference.ModuleInformation as ModuleInformation exposing (ModuleInformationDict)
import TypeInference.Type as Type exposing (Type)
import TypeInference.TypeByNameLookup as TypeByNameLookup exposing (TypeByNameLookup)
import TypeInference.Value as Value exposing (Value)


type ProjectContext
    = ProjectContext InternalProjectContext


type alias InternalProjectContext =
    { dependencies : Dict ModuleName Review.Project.Dependency.Dependency
    , moduleInformationDict : ModuleInformationDict
    }


unwrapProject : ProjectContext -> InternalProjectContext
unwrapProject (ProjectContext projectContext) =
    projectContext


type alias OuterModuleContext a =
    { a
        | moduleNameLookupTable : ModuleNameLookupTable
        , typeByNameLookup : TypeByNameLookup
        , typeInference : ModuleContext
    }


type alias ModuleContext =
    { moduleInformationDict : ModuleInformationDict
    , operatorsInScope : Dict String Type
    , moduleValues : List Value
    }


initialProjectContext : ProjectContext
initialProjectContext =
    ProjectContext
        { dependencies = Dict.empty
        , moduleInformationDict = ModuleInformation.empty
        }


fromProjectToModule : { projectContext | typeInference : ProjectContext } -> ModuleContext
fromProjectToModule { typeInference } =
    let
        projectContext : InternalProjectContext
        projectContext =
            unwrapProject typeInference
    in
    { moduleInformationDict = projectContext.moduleInformationDict
    , operatorsInScope =
        List.concatMap
            (\import_ ->
                ModuleInformation.forModule (Node.value import_.moduleName) projectContext.moduleInformationDict
                    |> Maybe.map (ModuleInformation.binops >> Dict.values >> List.map (\binop -> ( Binop.name binop, Binop.tipe binop )))
                    |> Maybe.withDefault []
            )
            ElmCorePrelude.elmCorePrelude
            |> Dict.fromList
    , moduleValues = []
    }


fromModuleToProject : ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleName moduleContext =
    ProjectContext
        { dependencies = Dict.empty
        , moduleInformationDict =
            ModuleInformation.fromVisitedModule moduleName
                { values = List.map (Value.relateToModule moduleName) moduleContext.moduleValues
                }
        }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts (ProjectContext newContext) (ProjectContext previousContext) =
    ProjectContext
        { dependencies = previousContext.dependencies
        , moduleInformationDict = ModuleInformation.merge newContext.moduleInformationDict previousContext.moduleInformationDict
        }


addProjectVisitors :
    Rule.ProjectRuleSchema
        { projectSchemaState | canAddModuleVisitor : () }
        { projectContext | typeInference : ProjectContext }
        (OuterModuleContext a)
    ->
        Rule.ProjectRuleSchema
            { projectSchemaState | canAddModuleVisitor : (), hasAtLeastOneVisitor : (), withModuleContext : Rule.Required }
            { projectContext | typeInference : ProjectContext }
            (OuterModuleContext a)
addProjectVisitors schema =
    schema
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withContextFromImportedModules


moduleVisitor : Rule.ModuleRuleSchema schemaState (OuterModuleContext a) -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } (OuterModuleContext a)
moduleVisitor schema =
    schema |> Rule.withDeclarationListVisitor declarationListVisitor



-- DEPENDENCIES VISITOR


dependenciesVisitor : Dict String Review.Project.Dependency.Dependency -> { projectContext | typeInference : ProjectContext } -> ( List nothing, { projectContext | typeInference : ProjectContext } )
dependenciesVisitor rawDependencies context =
    let
        projectContext : InternalProjectContext
        projectContext =
            unwrapProject context.typeInference

        dependencies : Dict (List String) Review.Project.Dependency.Dependency
        dependencies =
            rawDependencies
                |> Dict.toList
                |> List.map (\( key, value ) -> ( String.split "." key, value ))
                |> Dict.fromList
    in
    ( []
    , { context
        | typeInference =
            ProjectContext
                { projectContext
                    | dependencies = dependencies
                    , moduleInformationDict = ModuleInformation.fromDependencies rawDependencies
                }
      }
    )



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> OuterModuleContext a -> ( List nothing, OuterModuleContext a )
declarationListVisitor nodes context =
    let
        moduleContext : ModuleContext
        moduleContext =
            context.typeInference
    in
    ( []
    , { context
        | typeByNameLookup =
            TypeByNameLookup.addType
                (List.concatMap (typeOfDeclaration context.moduleNameLookupTable) nodes)
                context.typeByNameLookup
        , typeInference = { moduleContext | moduleValues = List.concatMap (takeValues context.moduleNameLookupTable) nodes }
      }
    )


typeOfDeclaration : ModuleNameLookupTable -> Node Declaration -> List ( String, Type )
typeOfDeclaration moduleNameLookupTable node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionName : String
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            case function.signature of
                Just signature ->
                    [ ( functionName
                      , signature
                            |> Node.value
                            |> .typeAnnotation
                            |> typeAnnotationToElmType moduleNameLookupTable
                      )
                    ]

                Nothing ->
                    []

        Declaration.CustomTypeDeclaration type_ ->
            let
                customTypeType : Type
                customTypeType =
                    Type.Type
                        []
                        (Node.value type_.name)
                        (List.map (Node.value >> Type.Generic) type_.generics)
            in
            List.map
                (\(Node _ { name, arguments }) ->
                    let
                        functionType : Type
                        functionType =
                            List.foldr
                                (\input output ->
                                    Type.Function
                                        (typeAnnotationToElmType moduleNameLookupTable input)
                                        output
                                )
                                customTypeType
                                arguments
                    in
                    ( Node.value name, functionType )
                )
                type_.constructors

        Declaration.AliasDeclaration typeAlias ->
            let
                aliasType : Type
                aliasType =
                    Type.Type []
                        (Node.value typeAlias.name)
                        (List.map (Node.value >> Type.Generic) typeAlias.generics)
            in
            case typeAnnotationToElmType moduleNameLookupTable typeAlias.typeAnnotation of
                Type.Record { fields } ->
                    let
                        functionType : Type
                        functionType =
                            List.foldr
                                (\( _, type_ ) output -> Type.Function type_ output)
                                aliasType
                                fields
                    in
                    [ ( Node.value typeAlias.name, functionType ) ]

                _ ->
                    []

        Declaration.PortDeclaration { name, typeAnnotation } ->
            [ ( Node.value name, typeAnnotationToElmType moduleNameLookupTable typeAnnotation ) ]

        Declaration.InfixDeclaration _ ->
            []

        Declaration.Destructuring _ _ ->
            -- Can't occur
            []


takeValues : ModuleNameLookupTable -> Node Declaration -> List Value
takeValues moduleNameLookupTable node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionName : String
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            [ Value.create
                { name = functionName
                , documentation =
                    case function.documentation of
                        Just documentation ->
                            Node.value documentation

                        Nothing ->
                            ""
                , tipe =
                    case function.signature of
                        Just signature ->
                            signature
                                |> Node.value
                                |> .typeAnnotation
                                |> typeAnnotationToElmType moduleNameLookupTable

                        Nothing ->
                            Type.Unknown
                }
            ]

        Declaration.CustomTypeDeclaration type_ ->
            let
                customTypeType : Type
                customTypeType =
                    Type.Type
                        []
                        (Node.value type_.name)
                        (List.map (Node.value >> Type.Generic) type_.generics)
            in
            List.map
                (\(Node _ { name, arguments }) ->
                    let
                        functionType : Type
                        functionType =
                            List.foldr
                                (\input output ->
                                    Type.Function
                                        (typeAnnotationToElmType moduleNameLookupTable input)
                                        output
                                )
                                customTypeType
                                arguments
                    in
                    Value.create
                        { name = Node.value name
                        , documentation = ""
                        , tipe = functionType
                        }
                )
                type_.constructors

        Declaration.AliasDeclaration typeAlias ->
            let
                aliasType : Type
                aliasType =
                    Type.Type []
                        (Node.value typeAlias.name)
                        (List.map (Node.value >> Type.Generic) typeAlias.generics)
            in
            case typeAnnotationToElmType moduleNameLookupTable typeAlias.typeAnnotation of
                Type.Record { fields } ->
                    let
                        functionType : Type
                        functionType =
                            List.foldr
                                (\( _, type_ ) output -> Type.Function type_ output)
                                aliasType
                                fields
                    in
                    [ Value.create
                        { name = Node.value typeAlias.name
                        , documentation =
                            case typeAlias.documentation of
                                Just documentation ->
                                    Node.value documentation

                                Nothing ->
                                    ""
                        , tipe = functionType
                        }
                    ]

                _ ->
                    []

        Declaration.PortDeclaration { name, typeAnnotation } ->
            [ Value.create
                { name = Node.value name
                , documentation = ""
                , tipe = typeAnnotationToElmType moduleNameLookupTable typeAnnotation
                }
            ]

        Declaration.InfixDeclaration _ ->
            []

        Declaration.Destructuring _ _ ->
            -- Can't occur
            []



-- TYPE INFERENCE


inferType : OuterModuleContext a -> Node Expression -> Maybe Type
inferType context node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            inferType context expr

        Expression.Literal _ ->
            Just (Type.Type [ "String" ] "String" [])

        Expression.CharLiteral _ ->
            Just (Type.Type [ "Char" ] "Char" [])

        Expression.Integer _ ->
            Just (Type.Generic "number")

        Expression.Hex _ ->
            Just (Type.Generic "number")

        Expression.Floatable _ ->
            Just (Type.Type [ "Basics" ] "Float" [])

        Expression.UnitExpr ->
            Just (Type.Tuple [])

        Expression.FunctionOrValue _ name ->
            case ModuleNameLookupTable.moduleNameFor context.moduleNameLookupTable node of
                Just [] ->
                    TypeByNameLookup.byName context.typeByNameLookup name

                Just moduleName ->
                    case ModuleInformation.forModule moduleName context.typeInference.moduleInformationDict of
                        Just module_ ->
                            ModuleInformation.values module_
                                |> Dict.get name
                                |> Maybe.map Value.tipe

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Expression.Application elements ->
            case elements of
                [] ->
                    Nothing

                function :: arguments ->
                    inferType context function
                        |> Maybe.andThen (applyArguments context arguments)

        Expression.TupledExpression nodes ->
            let
                inferredTypes : List Type
                inferredTypes =
                    List.filterMap (inferType context) nodes
            in
            if List.length inferredTypes == List.length nodes then
                Just (Type.Tuple inferredTypes)

            else
                Nothing

        Expression.ListExpr nodes ->
            if List.isEmpty nodes then
                Just (Type.Type [ "List" ] "List" [ Type.Generic "nothing" ])

            else
                inferTypeFromCombinationOf (List.map (\nodeInList () -> ( context, nodeInList )) nodes)
                    |> Maybe.map (\type_ -> Type.Type [ "List" ] "List" [ type_ ])

        Expression.RecordExpr fields ->
            let
                inferredFields : List ( String, Type )
                inferredFields =
                    List.filterMap
                        (Node.value
                            >> (\( fieldName, fieldValue ) ->
                                    Maybe.map
                                        (Tuple.pair (Node.value fieldName))
                                        (inferType context fieldValue)
                               )
                        )
                        fields
            in
            if List.length inferredFields == List.length fields then
                Just (Type.Record { fields = inferredFields, generic = Nothing, canHaveMoreFields = False })

            else
                Nothing

        Expression.RecordAccess expression (Node _ fieldName) ->
            case inferType context expression of
                Just (Type.Record { fields }) ->
                    find (\( name, _ ) -> fieldName == name) fields
                        |> Maybe.map Tuple.second

                _ ->
                    Nothing

        Expression.RecordAccessFunction fieldName ->
            Type.Function
                (Type.Record
                    { fields =
                        [ ( String.dropLeft 1 fieldName, Type.Generic "a" )
                        ]
                    , generic = Just "b"
                    , canHaveMoreFields = False
                    }
                )
                (Type.Generic "a")
                |> Just

        Expression.OperatorApplication operator _ left right ->
            Dict.get operator context.typeInference.operatorsInScope
                |> Maybe.andThen (\function -> applyArguments context [ left, right ] function)

        Expression.IfBlock _ ifTrue ifFalse ->
            inferTypeFromCombinationOf (List.map (\branchNode () -> ( context, branchNode )) [ ifTrue, ifFalse ])

        Expression.PrefixOperator operator ->
            Dict.get operator context.typeInference.operatorsInScope

        Expression.Operator _ ->
            -- Never occurs
            Nothing

        Expression.Negation expr ->
            inferType context expr

        Expression.LetExpression { declarations, expression } ->
            case inferType context expression of
                Just inferredType ->
                    Just inferredType

                Nothing ->
                    let
                        newContext : OuterModuleContext a
                        newContext =
                            { context
                                | typeByNameLookup =
                                    context.typeByNameLookup
                                        |> TypeByNameLookup.addNewScope
                                        |> TypeByNameLookup.addType (List.concatMap (typeOfLetDeclaration context) declarations)
                            }
                    in
                    inferType newContext expression

        Expression.CaseExpression { expression, cases } ->
            let
                inferredTypeForEvaluatedExpression : Maybe Type
                inferredTypeForEvaluatedExpression =
                    inferType context expression
            in
            cases
                |> List.map
                    (\( pattern, expr ) () ->
                        let
                            typeByNameLookup : TypeByNameLookup
                            typeByNameLookup =
                                case inferredTypeForEvaluatedExpression of
                                    Just inferred ->
                                        TypeByNameLookup.addType (assignTypeToPattern inferred pattern) context.typeByNameLookup

                                    Nothing ->
                                        case ( Node.value expression, inferTypeFromPattern pattern ) of
                                            ( Expression.FunctionOrValue [] name, Just inferred ) ->
                                                TypeByNameLookup.addType [ ( name, inferred ) ] context.typeByNameLookup

                                            _ ->
                                                context.typeByNameLookup

                            contextToUse : OuterModuleContext a
                            contextToUse =
                                { context | typeByNameLookup = typeByNameLookup }
                        in
                        ( addTypeFromPatternToContext pattern contextToUse
                        , expr
                        )
                    )
                |> inferTypeFromCombinationOf

        Expression.LambdaExpression _ ->
            -- TODO Handle this case
            -- Needs inferring of arguments
            Nothing

        Expression.RecordUpdateExpression name _ ->
            TypeByNameLookup.byName context.typeByNameLookup (Node.value name)

        Expression.GLSLExpression _ ->
            -- TODO Handle this case
            Nothing


addTypeFromPatternToContext : Node Pattern -> OuterModuleContext a -> OuterModuleContext a
addTypeFromPatternToContext pattern context =
    case Node.value pattern of
        Pattern.AllPattern ->
            context

        Pattern.UnitPattern ->
            context

        Pattern.CharPattern _ ->
            context

        Pattern.StringPattern _ ->
            context

        Pattern.IntPattern _ ->
            context

        Pattern.HexPattern _ ->
            context

        Pattern.FloatPattern _ ->
            context

        Pattern.TuplePattern _ ->
            --List.foldl addTypeFromPatternToContext context patterns
            context

        Pattern.RecordPattern _ ->
            context

        Pattern.UnConsPattern _ _ ->
            context

        Pattern.ListPattern _ ->
            context

        Pattern.VarPattern _ ->
            context

        Pattern.NamedPattern { name } argumentPatterns ->
            case TypeByNameLookup.byName context.typeByNameLookup name of
                Just type_ ->
                    let
                        typeVariablesInType : Set String
                        typeVariablesInType =
                            findTypeVariables type_
                    in
                    { context
                        | typeByNameLookup =
                            TypeByNameLookup.addType (assignTypesToPatterns typeVariablesInType type_ argumentPatterns) context.typeByNameLookup
                    }

                Nothing ->
                    context

        Pattern.AsPattern _ _ ->
            context

        Pattern.ParenthesizedPattern _ ->
            context


assignTypesToPatterns : Set String -> Type -> List (Node Pattern) -> List ( String, Type )
assignTypesToPatterns typeVariables type_ patterns =
    case patterns of
        [] ->
            []

        head :: rest ->
            case type_ of
                Type.Function input output ->
                    (assignTypeToPattern input head
                        |> List.filter
                            (\( _, typeForPattern ) ->
                                Set.isEmpty <|
                                    Set.intersect
                                        typeVariables
                                        (findTypeVariables typeForPattern)
                            )
                    )
                        ++ assignTypesToPatterns typeVariables output rest

                _ ->
                    []


assignTypeToPattern : Type -> Node Pattern -> List ( String, Type )
assignTypeToPattern type_ node =
    case ( Node.value node, type_ ) of
        ( Pattern.VarPattern name, _ ) ->
            [ ( name, type_ ) ]

        ( Pattern.TuplePattern subPatterns, Type.Tuple tuples ) ->
            List.map2 assignTypeToPattern
                tuples
                subPatterns
                |> List.concat

        ( Pattern.RecordPattern patternFieldNames, Type.Record { fields } ) ->
            List.filterMap
                (Node.value
                    >> (\patternFieldName ->
                            find
                                (\( typeFieldName, _ ) ->
                                    typeFieldName == patternFieldName
                                )
                                fields
                       )
                )
                patternFieldNames

        _ ->
            []


inferTypeFromPattern : Node Pattern -> Maybe Type
inferTypeFromPattern node =
    case Node.value node of
        Pattern.VarPattern _ ->
            Nothing

        Pattern.AllPattern ->
            Nothing

        Pattern.UnitPattern ->
            Just (Type.Tuple [])

        Pattern.CharPattern _ ->
            Nothing

        Pattern.StringPattern _ ->
            Nothing

        Pattern.IntPattern _ ->
            Nothing

        Pattern.HexPattern _ ->
            Nothing

        Pattern.FloatPattern _ ->
            Nothing

        Pattern.TuplePattern _ ->
            Nothing

        Pattern.RecordPattern _ ->
            Nothing

        Pattern.UnConsPattern _ _ ->
            Nothing

        Pattern.ListPattern _ ->
            Nothing

        Pattern.NamedPattern _ _ ->
            Nothing

        Pattern.AsPattern _ _ ->
            Nothing

        Pattern.ParenthesizedPattern _ ->
            Nothing


inferTypeFromCombinationOf : List (() -> ( OuterModuleContext a, Node Expression )) -> Maybe Type
inferTypeFromCombinationOf expressions =
    inferTypeFromCombinationOfInternal
        { hasUnknowns = False, maybeInferred = Nothing, typeVariablesList = [] }
        expressions


inferTypeFromCombinationOfInternal :
    { hasUnknowns : Bool
    , maybeInferred : Maybe Type
    , typeVariablesList : List (Set String)
    }
    -> List (() -> ( OuterModuleContext a, Node Expression ))
    -> Maybe Type
inferTypeFromCombinationOfInternal previousItemsResult expressions =
    case expressions of
        [] ->
            if previousItemsResult.hasUnknowns then
                Nothing

            else
                case previousItemsResult.typeVariablesList of
                    [] ->
                        -- Should not happen?
                        Nothing

                    head :: tail ->
                        if List.all ((==) head) tail then
                            previousItemsResult.maybeInferred

                        else
                            Nothing

        head :: tail ->
            let
                ( context, node ) =
                    head ()
            in
            case inferType context node of
                Just inferredType ->
                    let
                        typeVariables : Set String
                        typeVariables =
                            findTypeVariables inferredType

                        refinedType_ : Type
                        refinedType_ =
                            case previousItemsResult.maybeInferred of
                                Just previouslyInferred ->
                                    refineInferredType previouslyInferred inferredType

                                Nothing ->
                                    inferredType
                    in
                    if Set.isEmpty typeVariables then
                        Just inferredType

                    else
                        inferTypeFromCombinationOfInternal
                            { previousItemsResult
                                | maybeInferred = Just refinedType_
                                , typeVariablesList = typeVariables :: previousItemsResult.typeVariablesList
                            }
                            tail

                Nothing ->
                    inferTypeFromCombinationOfInternal
                        { previousItemsResult | hasUnknowns = True }
                        tail


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if predicate head then
                Just head

            else
                find predicate tail


refineInferredType : Type -> Type -> Type
refineInferredType _ typeB =
    typeB


applyArguments : OuterModuleContext a -> List (Node Expression) -> Type -> Maybe Type
applyArguments context arguments type_ =
    -- TODO Use Unknown where applicable
    applyArgumentsInternal context arguments Set.empty type_


applyArgumentsInternal : OuterModuleContext a -> List (Node Expression) -> Set String -> Type -> Maybe Type
applyArgumentsInternal context arguments previousTypeVariables type_ =
    case arguments of
        [] ->
            if Set.intersect (findTypeVariables type_) previousTypeVariables |> Set.isEmpty then
                Just type_

            else
                Nothing

        _ :: restOfArguments ->
            case type_ of
                Type.Function input output ->
                    let
                        typeVariables : Set String
                        typeVariables =
                            Set.union
                                (findTypeVariables input)
                                previousTypeVariables
                    in
                    applyArgumentsInternal context restOfArguments typeVariables output

                _ ->
                    Nothing


findTypeVariables : Type -> Set String
findTypeVariables type_ =
    case type_ of
        Type.Unknown ->
            Set.empty

        Type.Generic string ->
            Set.singleton string

        Type.Function input output ->
            Set.union
                (findTypeVariables input)
                (findTypeVariables output)

        Type.Tuple types ->
            types
                |> List.map findTypeVariables
                |> List.foldl Set.union Set.empty

        Type.Type _ _ types ->
            types
                |> List.map findTypeVariables
                |> List.foldl Set.union Set.empty

        Type.Record record ->
            let
                startSet : Set String
                startSet =
                    case record.generic of
                        Just generic ->
                            Set.singleton generic

                        Nothing ->
                            Set.empty
            in
            record.fields
                |> List.map (Tuple.second >> findTypeVariables)
                |> List.foldl Set.union startSet


typeAnnotationToElmType : ModuleNameLookupTable -> Node TypeAnnotation -> Type
typeAnnotationToElmType moduleNameLookupTable node =
    case Node.value node of
        TypeAnnotation.GenericType var ->
            Type.Generic var

        TypeAnnotation.Typed (Node typeRange ( rawModuleName, name )) nodes ->
            case ModuleNameLookupTable.moduleNameAt moduleNameLookupTable typeRange of
                Just moduleName ->
                    Type.Type moduleName name (List.map (typeAnnotationToElmType moduleNameLookupTable) nodes)

                Nothing ->
                    -- TODO Should probably be Type.Unknown
                    Type.Type rawModuleName name (List.map (typeAnnotationToElmType moduleNameLookupTable) nodes)

        TypeAnnotation.Unit ->
            Type.Tuple []

        TypeAnnotation.Tupled nodes ->
            Type.Tuple (List.map (typeAnnotationToElmType moduleNameLookupTable) nodes)

        TypeAnnotation.Record recordDefinition ->
            Type.Record
                { fields =
                    List.map
                        (Node.value >> (\( fieldName, fieldType ) -> ( Node.value fieldName, typeAnnotationToElmType moduleNameLookupTable fieldType )))
                        recordDefinition
                , generic = Nothing
                , canHaveMoreFields = False
                }

        TypeAnnotation.GenericRecord genericVar recordDefinition ->
            Type.Record
                { fields =
                    List.map
                        (Node.value >> (\( fieldName, fieldType ) -> ( Node.value fieldName, typeAnnotationToElmType moduleNameLookupTable fieldType )))
                        (Node.value recordDefinition)
                , generic = Just (Node.value genericVar)
                , canHaveMoreFields = False
                }

        TypeAnnotation.FunctionTypeAnnotation input output ->
            Type.Function
                (typeAnnotationToElmType moduleNameLookupTable input)
                (typeAnnotationToElmType moduleNameLookupTable output)



-- DECLARATION LIST VISITOR


typeOfLetDeclaration : OuterModuleContext a -> Node Expression.LetDeclaration -> List ( String, Type )
typeOfLetDeclaration context node =
    case Node.value node of
        Expression.LetFunction function ->
            typeOfFunctionDeclaration context function

        Expression.LetDestructuring _ _ ->
            []


typeOfFunctionDeclaration : OuterModuleContext a -> Expression.Function -> List ( String, Type )
typeOfFunctionDeclaration context function =
    let
        functionName : String
        functionName =
            function.declaration
                |> Node.value
                |> .name
                |> Node.value
    in
    case function.signature of
        Just signature ->
            [ ( functionName
              , signature
                    |> Node.value
                    |> .typeAnnotation
                    |> typeAnnotationToElmType context.moduleNameLookupTable
              )
            ]

        Nothing ->
            case inferType context (function.declaration |> Node.value |> .expression) of
                Just inferredType ->
                    [ ( functionName, inferredType ) ]

                Nothing ->
                    []
