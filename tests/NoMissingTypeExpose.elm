module NoMissingTypeExpose exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs exposing (Module)
import Elm.Module
import Elm.Project exposing (Project)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports types that should be exposed but are not.

🔧 Running with `--fix` will automatically fix all the reported errors.

If a type is not exposed then it can be impossible to annotate functions or values that use them outside of the module. Affected types may be used in exposed function signatures, type aliases or other custom types.

    import NoMissingTypeExpose

    config : List Rule
    config =
        [ NoMissingTypeExpose.rule
        ]


## Fail

    module Happiness exposing (happy, toString)

    -- Type `Happiness` is private because it's not been exposed

    type Happiness
        = Happy

    -- Private type `Happiness` used by exposed function `toString`
    toString : Happiness -> String
    toString happiness =
        "Happy"

    -- Private type `Happiness` used by exposed value `happy`
    happy : Happiness
    happy =
        Happy

## Success

    module Happiness exposing (Happiness, happy, toString)

    type Happiness
        = Happy

    toString : Happiness -> String
    toString happiness =
        "Happy"

    happy : Happiness
    happy =
        Happy


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoMissingTypeExpose
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoMissingTypeExpose" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependencyDictVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModuleContext
            , fromModuleToProject = fromModuleToProjectContext
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


elmJsonVisitor : Maybe { a | project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject context =
    case maybeProject of
        Just { project } ->
            ( []
            , { context
                | exposedModules = exposedModulesForElmJson project
              }
            )

        Nothing ->
            ( [], context )


exposedModulesForElmJson : Project -> ExposedModules
exposedModulesForElmJson project =
    case project of
        Elm.Project.Package { exposed } ->
            Package (elmProjectExposedList exposed)

        Elm.Project.Application _ ->
            Application


elmProjectExposedList : Elm.Project.Exposed -> Set String
elmProjectExposedList exposed =
    case exposed of
        Elm.Project.ExposedList list ->
            List.foldl (Elm.Module.toString >> Set.insert) Set.empty list

        Elm.Project.ExposedDict dict ->
            List.foldl
                (\( _, list ) acc ->
                    List.foldl (Elm.Module.toString >> Set.insert) acc list
                )
                Set.empty
                dict


dependencyDictVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependencyDictVisitor dependencies context =
    ( []
    , { context
        | exposedModules =
            Dict.values dependencies
                |> List.foldl exposedModulesForDependency context.exposedModules
      }
    )


exposedModulesForDependency : Dependency -> ExposedModules -> ExposedModules
exposedModulesForDependency dependency exposedModules =
    Dependency.modules dependency
        |> List.foldl (.name >> addExposedModule) exposedModules


moduleVisitor :
    Rule.ModuleRuleSchema state ModuleContext
    -> Rule.ModuleRuleSchema { state | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor (Node _ mod) context =
    case context.moduleType of
        InternalModule data ->
            ( []
            , { lookupTable = context.lookupTable
              , modulesFromTheProject = context.modulesFromTheProject
              , moduleType = InternalModule { data | exposes = Module.exposingList mod }
              }
            )

        ExposedModule data ->
            ( []
            , { lookupTable = context.lookupTable
              , modulesFromTheProject = context.modulesFromTheProject
              , moduleType =
                    ExposedModule
                        { data
                            | exposes = Module.exposingList mod
                            , exposingListStart = exposingListStartLocation (Module.exposingList mod)
                        }
              }
            )


exposingListStartLocation : Exposing -> Maybe Range.Location
exposingListStartLocation exposes =
    case exposes of
        Exposing.Explicit ((Node range _) :: _) ->
            Just range.start

        _ ->
            Nothing


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor (Node _ { moduleName, moduleAlias }) context =
    case context.moduleType of
        InternalModule _ ->
            ( [], context )

        ExposedModule data ->
            ( []
            , { lookupTable = context.lookupTable
              , modulesFromTheProject = context.modulesFromTheProject
              , moduleType =
                    ExposedModule
                        { data | exposedModules = exposedModulesForImportAlias (Node.value moduleName) moduleAlias data.exposedModules }
              }
            )


exposedModulesForImportAlias : ModuleName -> Maybe (Node ModuleName) -> ExposedModules -> ExposedModules
exposedModulesForImportAlias moduleName maybeModuleAlias exposedModules =
    case maybeModuleAlias of
        Just (Node _ moduleAlias) ->
            addExposedModuleAlias moduleName
                (String.join "." moduleAlias)
                exposedModules

        Nothing ->
            exposedModules


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    ( []
    , case context.moduleType of
        InternalModule data ->
            { lookupTable = context.lookupTable
            , modulesFromTheProject = context.modulesFromTheProject
            , moduleType =
                InternalModule
                    { data
                        | exposedTypes =
                            exposedTypesForDeclarationList data.exposes nodes data.exposedTypes
                    }
            }

        ExposedModule data ->
            { lookupTable = context.lookupTable
            , modulesFromTheProject = context.modulesFromTheProject
            , moduleType =
                ExposedModule
                    { data
                        | declaredTypes = declaredTypesForDeclarationList nodes data.declaredTypes
                        , exposedSignatureTypes = exposedSignatureTypesForDeclarationList context.lookupTable data.exposes nodes data.exposedSignatureTypes
                    }
            }
    )


exposedTypesForDeclarationList : Exposing -> List (Node Declaration) -> Set String -> Set String
exposedTypesForDeclarationList exposes list exposedTypes =
    List.foldl (exposedTypesForDeclaration exposes) exposedTypes list


exposedTypesForDeclaration : Exposing -> Node Declaration -> Set String -> Set String
exposedTypesForDeclaration exposes (Node _ declaration) exposedTypes =
    case declaration of
        Declaration.CustomTypeDeclaration { name } ->
            rememberExposedType exposes name exposedTypes

        Declaration.AliasDeclaration { name } ->
            rememberExposedType exposes name exposedTypes

        _ ->
            exposedTypes


rememberExposedType : Exposing -> Node String -> Set String -> Set String
rememberExposedType exposes (Node _ name) exposedTypes =
    if isTypeExposed exposes name then
        Set.insert name exposedTypes

    else
        exposedTypes


declaredTypesForDeclarationList : List (Node Declaration) -> Set String -> Set String
declaredTypesForDeclarationList list declaredTypes =
    List.foldl declaredTypesForDeclaration declaredTypes list


declaredTypesForDeclaration : Node Declaration -> Set String -> Set String
declaredTypesForDeclaration (Node _ declaration) declaredTypes =
    case declaration of
        Declaration.CustomTypeDeclaration { name } ->
            rememberDeclaredType name declaredTypes

        Declaration.AliasDeclaration { name } ->
            rememberDeclaredType name declaredTypes

        _ ->
            declaredTypes


rememberDeclaredType : Node String -> Set String -> Set String
rememberDeclaredType (Node _ name) declaredTypes =
    Set.insert name declaredTypes


exposedSignatureTypesForDeclarationList :
    ModuleNameLookupTable
    -> Exposing
    -> List (Node Declaration)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForDeclarationList lookupTable exposes list exposedSignatureTypes =
    List.foldl (exposedSignatureTypesForDeclaration lookupTable exposes) exposedSignatureTypes list


exposedSignatureTypesForDeclaration :
    ModuleNameLookupTable
    -> Exposing
    -> Node Declaration
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForDeclaration lookupTable exposes (Node _ declaration) exposedSignatureTypes =
    case declaration of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            exposedSignatureTypesForConstructorList lookupTable exposes name constructors exposedSignatureTypes

        Declaration.AliasDeclaration { name, typeAnnotation } ->
            exposedSignatureTypesForAlias lookupTable exposes name typeAnnotation exposedSignatureTypes

        Declaration.FunctionDeclaration { signature } ->
            exposedSignatureTypesForSignature lookupTable exposes signature exposedSignatureTypes

        _ ->
            exposedSignatureTypes


exposedSignatureTypesForConstructorList :
    ModuleNameLookupTable
    -> Exposing
    -> Node String
    -> List (Node Type.ValueConstructor)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForConstructorList lookupTable exposes (Node _ name) list exposedSignatureTypes =
    if isTypeExposedOpen exposes name then
        List.foldl (exposedSignatureTypesForConstructor lookupTable) exposedSignatureTypes list

    else
        exposedSignatureTypes


exposedSignatureTypesForConstructor :
    ModuleNameLookupTable
    -> Node Type.ValueConstructor
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForConstructor lookupTable (Node _ { arguments }) exposedSignatureTypes =
    exposedSignatureTypesForTypeAnnotationList lookupTable arguments exposedSignatureTypes


exposedSignatureTypesForAlias :
    ModuleNameLookupTable
    -> Exposing
    -> Node String
    -> Node TypeAnnotation
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForAlias lookupTable exposes (Node _ name) typeAnnotation exposedSignatureTypes =
    if isTypeExposed exposes name then
        case typeAnnotation of
            Node _ (TypeAnnotation.Typed _ list) ->
                exposedSignatureTypesForTypeAnnotationList lookupTable list exposedSignatureTypes

            _ ->
                exposedSignatureTypesForTypeAnnotation lookupTable typeAnnotation exposedSignatureTypes

    else
        exposedSignatureTypes


exposedSignatureTypesForSignature :
    ModuleNameLookupTable
    -> Exposing
    -> Maybe (Node Signature)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForSignature lookupTable exposes maybeSignature exposedSignatureTypes =
    case maybeSignature of
        Just (Node _ { name, typeAnnotation }) ->
            if Exposing.exposesFunction (Node.value name) exposes then
                exposedSignatureTypesForTypeAnnotation lookupTable typeAnnotation exposedSignatureTypes

            else
                exposedSignatureTypes

        Nothing ->
            exposedSignatureTypes


exposedSignatureTypesForRecordFieldList :
    ModuleNameLookupTable
    -> List (Node TypeAnnotation.RecordField)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForRecordFieldList lookupTable fields exposedSignatureTypes =
    List.foldl (exposedSignatureTypesForRecordField lookupTable) exposedSignatureTypes fields


exposedSignatureTypesForRecordField :
    ModuleNameLookupTable
    -> Node TypeAnnotation.RecordField
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForRecordField lookupTable (Node _ ( _, typeAnnotation )) exposedSignatureTypes =
    exposedSignatureTypesForTypeAnnotation lookupTable typeAnnotation exposedSignatureTypes


exposedSignatureTypesForTypeAnnotationList :
    ModuleNameLookupTable
    -> List (Node TypeAnnotation)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForTypeAnnotationList lookupTable list exposedSignatureTypes =
    List.foldl (exposedSignatureTypesForTypeAnnotation lookupTable) exposedSignatureTypes list


exposedSignatureTypesForTypeAnnotation :
    ModuleNameLookupTable
    -> Node TypeAnnotation
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForTypeAnnotation lookupTable (Node _ typeAnnotation) exposedSignatureTypes =
    case typeAnnotation of
        TypeAnnotation.Typed name list ->
            case ModuleNameLookupTable.moduleNameFor lookupTable name of
                Just moduleName ->
                    (Node.map (\( _, typeName ) -> ( moduleName, typeName )) name :: exposedSignatureTypes)
                        |> exposedSignatureTypesForTypeAnnotationList lookupTable list

                Nothing ->
                    (name :: exposedSignatureTypes)
                        |> exposedSignatureTypesForTypeAnnotationList lookupTable list

        TypeAnnotation.FunctionTypeAnnotation left right ->
            exposedSignatureTypes
                |> exposedSignatureTypesForTypeAnnotation lookupTable left
                |> exposedSignatureTypesForTypeAnnotation lookupTable right

        TypeAnnotation.Tupled list ->
            exposedSignatureTypes
                |> exposedSignatureTypesForTypeAnnotationList lookupTable list

        TypeAnnotation.Record fields ->
            exposedSignatureTypes
                |> exposedSignatureTypesForRecordFieldList lookupTable fields

        TypeAnnotation.GenericRecord _ (Node _ fields) ->
            exposedSignatureTypes
                |> exposedSignatureTypesForRecordFieldList lookupTable fields

        TypeAnnotation.Unit ->
            exposedSignatureTypes

        TypeAnnotation.GenericType _ ->
            exposedSignatureTypes


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    case context.moduleType of
        InternalModule _ ->
            []

        ExposedModule data ->
            data.exposedSignatureTypes
                |> List.filter (isTypePrivate context.modulesFromTheProject data)
                |> List.map (makeError data.exposingListStart)


isTypePrivate : Set ModuleName -> ExposedModuleData -> Node ( ModuleName, String ) -> Bool
isTypePrivate modulesFromTheProject data (Node _ typeCall) =
    case typeCall of
        ( [], name ) ->
            Set.member name data.declaredTypes
                && not (isTypeExposed data.exposes name)

        ( moduleName, _ ) ->
            Set.member moduleName modulesFromTheProject
                && not (isModuleExposed data.exposedModules moduleName)


isTypeExposed : Exposing -> String -> Bool
isTypeExposed exposes name =
    case exposes of
        Exposing.All _ ->
            True

        Exposing.Explicit list ->
            List.any (isExposingATypeNamed name) list


isTypeExposedOpen : Exposing -> String -> Bool
isTypeExposedOpen exposes name =
    case exposes of
        Exposing.All _ ->
            True

        Exposing.Explicit list ->
            List.any (isExposingAnOpenTypeNamed name) list


isExposingATypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isExposingATypeNamed needle (Node _ topLevelExpose) =
    case topLevelExpose of
        Exposing.InfixExpose _ ->
            False

        Exposing.FunctionExpose _ ->
            False

        Exposing.TypeOrAliasExpose name ->
            name == needle

        Exposing.TypeExpose { name } ->
            name == needle


isExposingAnOpenTypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isExposingAnOpenTypeNamed needle (Node _ expose) =
    case expose of
        Exposing.TypeExpose { name, open } ->
            name == needle && open /= Nothing

        _ ->
            False


addExposedModule : String -> ExposedModules -> ExposedModules
addExposedModule moduleName exposedModules =
    case exposedModules of
        Application ->
            exposedModules

        Package list ->
            Package (Set.insert moduleName list)


addExposedModuleAlias : ModuleName -> String -> ExposedModules -> ExposedModules
addExposedModuleAlias moduleName moduleAlias exposedModules =
    case exposedModules of
        Application ->
            exposedModules

        Package list ->
            if Set.member (String.join "." moduleName) list then
                Package (Set.insert moduleAlias list)

            else
                exposedModules


isModuleExposed : ExposedModules -> ModuleName -> Bool
isModuleExposed exposedModules moduleName =
    case exposedModules of
        Application ->
            True

        Package list ->
            Set.member (String.join "." moduleName) list


makeError : Maybe Range.Location -> Node ( ModuleName, String ) -> Rule.Error {}
makeError exposingListStart (Node range typeName) =
    let
        formattedName : String
        formattedName =
            formatTypeName typeName
    in
    Rule.errorWithFix
        { message = "Private type `" ++ formattedName ++ "` should be exposed"
        , details =
            [ "Users of this module will not be able to annotate a value of this type if they wanted to. You should expose this type or an alias of this type."
            ]
        }
        range
        (exposeTypeFix exposingListStart typeName)


exposeTypeFix : Maybe Range.Location -> ( ModuleName, String ) -> List Fix
exposeTypeFix exposingListStart ( moduleName, name ) =
    case ( exposingListStart, moduleName ) of
        ( Just start, [] ) ->
            [ Fix.insertAt start (name ++ ", ") ]

        _ ->
            []


formatTypeName : ( ModuleName, String ) -> String
formatTypeName ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])


fromProjectToModuleContext : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModuleContext =
    Rule.initContextCreator
        (\lookupTable metadata { exposedModules, moduleTypes } ->
            let
                moduleType : ModuleType
                moduleType =
                    if isModuleExposed exposedModules (Rule.moduleNameFromMetadata metadata) then
                        initialExposedModuleType exposedModules moduleTypes

                    else
                        initialInternalModuleType
            in
            { lookupTable = lookupTable
            , modulesFromTheProject = Dict.keys moduleTypes |> Set.fromList
            , moduleType = moduleType
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withMetadata


fromModuleToProjectContext : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProjectContext =
    Rule.initContextCreator
        (\metadata context ->
            case context.moduleType of
                InternalModule { exposedTypes } ->
                    { initialProjectContext | moduleTypes = Dict.singleton (Rule.moduleNameFromMetadata metadata) exposedTypes }

                ExposedModule _ ->
                    initialProjectContext
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new old =
    { exposedModules = foldExposedModules new.exposedModules old.exposedModules
    , moduleTypes = foldModuleTypes new.moduleTypes old.moduleTypes
    }


foldExposedModules : ExposedModules -> ExposedModules -> ExposedModules
foldExposedModules newExposedModules oldExposedModules =
    case ( oldExposedModules, newExposedModules ) of
        ( Application, Application ) ->
            Application

        ( Application, Package _ ) ->
            newExposedModules

        ( Package _, Application ) ->
            oldExposedModules

        ( Package oldList, Package newList ) ->
            Package (Set.union oldList newList)


foldModuleTypes : Dict ModuleName (Set String) -> Dict ModuleName (Set String) -> Dict ModuleName (Set String)
foldModuleTypes newModuleTypes oldModuleTypes =
    Dict.foldl foldModuleTypesHelp newModuleTypes oldModuleTypes


foldModuleTypesHelp : ModuleName -> Set String -> Dict ModuleName (Set String) -> Dict ModuleName (Set String)
foldModuleTypesHelp moduleName newTypes moduleTypes =
    case Dict.get moduleName moduleTypes of
        Just oldTypes ->
            Dict.insert moduleName (Set.union oldTypes newTypes) moduleTypes

        Nothing ->
            Dict.insert moduleName newTypes moduleTypes


initialProjectContext : ProjectContext
initialProjectContext =
    { exposedModules = Application
    , moduleTypes = Dict.empty
    }


initialInternalModuleType : ModuleType
initialInternalModuleType =
    InternalModule
        { exposedTypes = Set.empty
        , exposes = Exposing.Explicit []
        }


initialExposedModuleType : ExposedModules -> Dict ModuleName (Set String) -> ModuleType
initialExposedModuleType exposedModules moduleTypes =
    ExposedModule
        { declaredTypes = Set.empty
        , exposedModules = exposedModules
        , exposedSignatureTypes = []
        , exposes = Exposing.Explicit []
        , exposingListStart = Nothing
        , moduleTypes = moduleTypes
        }


type alias ProjectContext =
    { exposedModules : ExposedModules
    , moduleTypes : Dict ModuleName (Set String)
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , modulesFromTheProject : Set ModuleName
    , moduleType : ModuleType
    }


type ModuleType
    = InternalModule InternalModuleData
    | ExposedModule ExposedModuleData


type alias InternalModuleData =
    { exposedTypes : Set String
    , exposes : Exposing
    }


type alias ExposedModuleData =
    { declaredTypes : Set String
    , exposedModules : ExposedModules
    , exposedSignatureTypes : List (Node ( ModuleName, String ))
    , exposes : Exposing
    , exposingListStart : Maybe Range.Location
    , moduleTypes : Dict ModuleName (Set String)
    }


type ExposedModules
    = Application
    | Package (Set String)
