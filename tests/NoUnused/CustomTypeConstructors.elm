module NoUnused.CustomTypeConstructors exposing (rule)

{-| Forbid having unused custom type constructors inside the project.


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
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid having unused custom type constructors.

    config =
        [ NoUnused.CustomTypeConstructors.rule []
        ]

Note that this rule reports any custom type constructor that isn't used
anywhere _in the project_.

If the project is a package and the module that declared the type is exposed and
the type's constructors are exposed, then the constructors will not be reported.

This does not prevent you from using phantom types: A constructor won't be reported if

  - It is the only constructor of a type that has no type variable
  - It has no parameters
  - It is used as an argument of a custom type, in the stead of a type variable that is not used in the definition in any of the type's constructors

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

I would love help with improving this :)


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
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator fromProjectToModule |> Rule.withModuleNameLookupTable |> Rule.withMetadata
            , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withModuleKey |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema



-- MODULE VISITOR


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor



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
        , customTypes : Dict CustomTypeName (Dict ConstructorName (Node ConstructorName))
        }


type alias ProjectContext =
    { exposedModules : Set ModuleNameAsString
    , exposedConstructors : Dict ModuleNameAsString ExposedConstructors
    , usedConstructors : Dict ModuleNameAsString (Set ConstructorName)
    , phantomVariables : Dict ModuleName (List ( CustomTypeName, Int ))
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , exposedCustomTypesWithConstructors : Set CustomTypeName
    , isExposed : Bool
    , exposesEverything : Bool
    , exposedConstructors : Dict ModuleNameAsString ExposedConstructors
    , declaredTypesWithConstructors : Dict CustomTypeName (Dict ConstructorName (Node ConstructorName))
    , usedFunctionsOrValues : Dict ModuleNameAsString (Set ConstructorName)
    , phantomVariables : Dict ModuleName (List ( CustomTypeName, Int ))
    }


initialProjectContext : List { moduleName : String, typeName : String, index : Int } -> ProjectContext
initialProjectContext phantomTypes =
    { exposedModules = Set.empty
    , exposedConstructors = Dict.empty
    , usedConstructors = Dict.empty
    , phantomVariables =
        List.foldl
            (\{ moduleName, typeName, index } dict ->
                Dict.update (String.split "." moduleName)
                    (Maybe.withDefault [] >> (::) ( typeName, index ) >> Just)
                    dict
            )
            Dict.empty
            phantomTypes
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    { lookupTable = lookupTable
    , exposedCustomTypesWithConstructors = Set.empty
    , isExposed = Set.member (Rule.moduleNameFromMetadata metadata |> String.join ".") projectContext.exposedModules
    , exposedConstructors = projectContext.exposedConstructors
    , exposesEverything = False
    , declaredTypesWithConstructors = Dict.empty
    , usedFunctionsOrValues = Dict.empty
    , phantomVariables = projectContext.phantomVariables
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
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

        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata

        moduleNameAsString : ModuleNameAsString
        moduleNameAsString =
            String.join "." moduleName
    in
    { exposedModules = Set.empty
    , exposedConstructors =
        if moduleContext.isExposed then
            Dict.empty

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
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedModules = previousContext.exposedModules
    , exposedConstructors = Dict.union newContext.exposedConstructors previousContext.exposedConstructors
    , usedConstructors =
        Dict.merge
            Dict.insert
            (\key newUsed previousUsed dict -> Dict.insert key (Set.union newUsed previousUsed) dict)
            Dict.insert
            newContext.usedConstructors
            previousContext.usedConstructors
            Dict.empty
    , phantomVariables = Dict.union newContext.phantomVariables previousContext.phantomVariables
    }



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
                                Exposing.TypeExpose { name } ->
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

                newPhantomVariables : Dict (List String) (List ( String, Int ))
                newPhantomVariables =
                    Dict.update
                        []
                        (\maybeSet ->
                            case maybeSet of
                                Just old ->
                                    Just (phantomVariables ++ old)

                                Nothing ->
                                    Just phantomVariables
                        )
                        context.phantomVariables
            in
            { context | phantomVariables = newPhantomVariables }

        _ ->
            context



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, constructors } ->
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

        Declaration.FunctionDeclaration function ->
            ( [], markPhantomTypesFromTypeAnnotationAsUsed (Maybe.map (Node.value >> .typeAnnotation) function.signature) context )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( [], markPhantomTypesFromTypeAnnotationAsUsed (Just typeAnnotation) context )

        _ ->
            ( [], context )



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node moduleContext =
    case Node.value node of
        Expression.FunctionOrValue _ name ->
            case ModuleNameLookupTable.moduleNameFor moduleContext.lookupTable node of
                Just moduleName ->
                    ( [], registerUsedFunctionOrValue moduleName name moduleContext )

                Nothing ->
                    ( [], moduleContext )

        Expression.LetExpression { declarations } ->
            ( []
            , declarations
                |> List.filterMap
                    (\declaration ->
                        case Node.value declaration of
                            Expression.LetFunction function ->
                                Just (Maybe.map (Node.value >> .typeAnnotation) function.signature)

                            Expression.LetDestructuring _ _ ->
                                Nothing
                    )
                |> List.foldl markPhantomTypesFromTypeAnnotationAsUsed moduleContext
            )

        _ ->
            ( [], moduleContext )


registerUsedFunctionOrValue : List String -> ConstructorName -> ModuleContext -> ModuleContext
registerUsedFunctionOrValue moduleName name moduleContext =
    if not (isCapitalized name) then
        moduleContext

    else
        { moduleContext
            | usedFunctionsOrValues =
                insertIntoUsedFunctionsOrValues
                    ( moduleName, name )
                    moduleContext.usedFunctionsOrValues
        }


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
    projectContext.exposedConstructors
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
                                |> List.map (errorForModule moduleKey)
                        )
            )



-- ERROR


errorInformation : String -> { message : String, details : List String }
errorInformation name =
    { message = "Type constructor `" ++ name ++ "` is not used."
    , details = [ "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created." ]
    }


errorForModule : Rule.ModuleKey -> Node String -> Error scope
errorForModule moduleKey node =
    Rule.errorForModule
        moduleKey
        (errorInformation (Node.value node))
        (Node.range node)



-- TYPE ANNOTATION UTILITY FUNCTIONS


markPhantomTypesFromTypeAnnotationAsUsed : Maybe (Node TypeAnnotation) -> ModuleContext -> ModuleContext
markPhantomTypesFromTypeAnnotationAsUsed maybeTypeAnnotation moduleContext =
    let
        used : List ( ModuleName, CustomTypeName )
        used =
            case maybeTypeAnnotation of
                Just typeAnnotation ->
                    collectTypesUsedAsPhantomVariables
                        moduleContext
                        moduleContext.phantomVariables
                        typeAnnotation

                Nothing ->
                    []

        usedFunctionsOrValues : Dict ModuleNameAsString (Set ConstructorName)
        usedFunctionsOrValues =
            List.foldl
                insertIntoUsedFunctionsOrValues
                moduleContext.usedFunctionsOrValues
                used
    in
    { moduleContext | usedFunctionsOrValues = usedFunctionsOrValues }


insertIntoUsedFunctionsOrValues : ( ModuleName, ConstructorName ) -> Dict ModuleNameAsString (Set ConstructorName) -> Dict ModuleNameAsString (Set ConstructorName)
insertIntoUsedFunctionsOrValues ( moduleName, constructorName ) dict =
    Dict.update
        (String.join "." moduleName)
        (\maybeSet ->
            case maybeSet of
                Just set ->
                    Just (Set.insert constructorName set)

                Nothing ->
                    Just (Set.singleton constructorName)
        )
        dict


collectGenericsFromTypeAnnotation : Node TypeAnnotation -> List String
collectGenericsFromTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectGenericsFromTypeAnnotation a ++ collectGenericsFromTypeAnnotation b

        TypeAnnotation.Typed _ params ->
            List.concatMap collectGenericsFromTypeAnnotation params

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.GenericRecord _ list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.Tupled list ->
            List.concatMap collectGenericsFromTypeAnnotation list

        TypeAnnotation.GenericType var ->
            [ var ]

        TypeAnnotation.Unit ->
            []


collectTypesUsedAsPhantomVariables : ModuleContext -> Dict ModuleName (List ( CustomTypeName, Int )) -> Node TypeAnnotation -> List ( ModuleName, CustomTypeName )
collectTypesUsedAsPhantomVariables moduleContext phantomVariables node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectTypesUsedAsPhantomVariables moduleContext phantomVariables a
                ++ collectTypesUsedAsPhantomVariables moduleContext phantomVariables b

        TypeAnnotation.Typed (Node.Node typeRange ( _, name )) params ->
            let
                moduleNameOfPhantomContainer : ModuleName
                moduleNameOfPhantomContainer =
                    ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable typeRange
                        |> Maybe.withDefault []

                typesUsedInThePhantomVariablePosition : List ( ModuleName, CustomTypeName )
                typesUsedInThePhantomVariablePosition =
                    Dict.get moduleNameOfPhantomContainer phantomVariables
                        |> Maybe.withDefault []
                        |> List.filter (\( type_, _ ) -> type_ == name)
                        |> List.filterMap
                            (\( _, index ) ->
                                case listAtIndex index params |> Maybe.map Node.value of
                                    Just (TypeAnnotation.Typed (Node.Node subTypeRange ( _, typeName )) _) ->
                                        ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable subTypeRange
                                            |> Maybe.map (\moduleNameOfPhantomVariable -> ( moduleNameOfPhantomVariable, typeName ))

                                    _ ->
                                        Nothing
                            )
            in
            List.concat
                [ typesUsedInThePhantomVariablePosition
                , List.concatMap (collectTypesUsedAsPhantomVariables moduleContext phantomVariables) params
                ]

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectTypesUsedAsPhantomVariables moduleContext phantomVariables)

        TypeAnnotation.GenericRecord _ list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectTypesUsedAsPhantomVariables moduleContext phantomVariables)

        TypeAnnotation.Tupled list ->
            List.concatMap (collectTypesUsedAsPhantomVariables moduleContext phantomVariables) list

        TypeAnnotation.GenericType _ ->
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
