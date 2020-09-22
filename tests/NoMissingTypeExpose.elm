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
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports types that should be exposed but are not.

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
        |> Rule.withModuleContext
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
    case context of
        InternalModule data ->
            ( [], InternalModule { data | exposes = Module.exposingList mod } )

        ExposedModule data ->
            ( []
            , ExposedModule
                { data
                    | exposes = Module.exposingList mod
                    , exposingListStart = exposingListStartLocation (Module.exposingList mod)
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
importVisitor (Node _ { moduleName, moduleAlias, exposingList }) context =
    case context of
        InternalModule _ ->
            ( [], context )

        ExposedModule data ->
            ( []
            , ExposedModule
                { data
                    | exposedModules =
                        exposedModulesForImportAlias (Node.value moduleName) moduleAlias data.exposedModules
                    , importedTypes =
                        importedTypesForImportExposing (Node.value moduleName) exposingList data.moduleTypes data.importedTypes
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


importedTypesForImportExposing :
    ModuleName
    -> Maybe (Node Exposing)
    -> Dict ModuleName (Set String)
    -> Dict String ModuleName
    -> Dict String ModuleName
importedTypesForImportExposing moduleName maybeExposing moduleTypes importedTypes =
    case maybeExposing of
        Just (Node _ (Exposing.Explicit list)) ->
            List.foldl (importedTypesForImportExpose moduleName) importedTypes list

        Just (Node _ (Exposing.All _)) ->
            importedTypesForModule moduleName moduleTypes importedTypes

        Nothing ->
            importedTypes


importedTypesForImportExpose : ModuleName -> Node Exposing.TopLevelExpose -> Dict String ModuleName -> Dict String ModuleName
importedTypesForImportExpose moduleName (Node _ expose) importedTypes =
    case expose of
        Exposing.TypeExpose { name } ->
            rememberImportedType moduleName name importedTypes

        Exposing.TypeOrAliasExpose name ->
            rememberImportedType moduleName name importedTypes

        Exposing.FunctionExpose _ ->
            importedTypes

        Exposing.InfixExpose _ ->
            importedTypes


importedTypesForModule :
    ModuleName
    -> Dict ModuleName (Set String)
    -> Dict String ModuleName
    -> Dict String ModuleName
importedTypesForModule moduleName moduleTypes importedTypes =
    case Dict.get moduleName moduleTypes of
        Just types ->
            Set.foldl (rememberImportedType moduleName) importedTypes types

        Nothing ->
            importedTypes


rememberImportedType : ModuleName -> String -> Dict String ModuleName -> Dict String ModuleName
rememberImportedType moduleName typeName importedTypes =
    Dict.insert typeName moduleName importedTypes


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    ( []
    , case context of
        InternalModule data ->
            InternalModule
                { data
                    | exposedTypes =
                        exposedTypesForDeclarationList data.exposes nodes data.exposedTypes
                }

        ExposedModule data ->
            ExposedModule
                { data
                    | declaredTypes = declaredTypesForDeclarationList nodes data.declaredTypes
                    , exposedSignatureTypes = exposedSignatureTypesForDeclarationList data.exposes nodes data.exposedSignatureTypes
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
    Exposing
    -> List (Node Declaration)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForDeclarationList exposes list exposedSignatureTypes =
    List.foldl (exposedSignatureTypesForDeclaration exposes) exposedSignatureTypes list


exposedSignatureTypesForDeclaration :
    Exposing
    -> Node Declaration
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForDeclaration exposes (Node _ declaration) exposedSignatureTypes =
    case declaration of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            exposedSignatureTypesForConstructorList exposes name constructors exposedSignatureTypes

        Declaration.AliasDeclaration { name, typeAnnotation } ->
            exposedSignatureTypesForAlias exposes name typeAnnotation exposedSignatureTypes

        Declaration.FunctionDeclaration { signature } ->
            exposedSignatureTypesForSignature exposes signature exposedSignatureTypes

        _ ->
            exposedSignatureTypes


exposedSignatureTypesForConstructorList :
    Exposing
    -> Node String
    -> List (Node Type.ValueConstructor)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForConstructorList exposes (Node _ name) list exposedSignatureTypes =
    if isTypeExposedOpen exposes name then
        List.foldl exposedSignatureTypesForConstructor exposedSignatureTypes list

    else
        exposedSignatureTypes


exposedSignatureTypesForConstructor :
    Node Type.ValueConstructor
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForConstructor (Node _ { arguments }) exposedSignatureTypes =
    exposedSignatureTypesForTypeAnnotationList arguments exposedSignatureTypes


exposedSignatureTypesForAlias :
    Exposing
    -> Node String
    -> Node TypeAnnotation
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForAlias exposes (Node _ name) typeAnnotation exposedSignatureTypes =
    if isTypeExposed exposes name then
        case typeAnnotation of
            Node _ (TypeAnnotation.Typed _ list) ->
                exposedSignatureTypesForTypeAnnotationList list exposedSignatureTypes

            _ ->
                exposedSignatureTypesForTypeAnnotation typeAnnotation exposedSignatureTypes

    else
        exposedSignatureTypes


exposedSignatureTypesForSignature :
    Exposing
    -> Maybe (Node Signature)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForSignature exposes maybeSignature exposedSignatureTypes =
    case maybeSignature of
        Just (Node _ { name, typeAnnotation }) ->
            if Exposing.exposesFunction (Node.value name) exposes then
                exposedSignatureTypesForTypeAnnotation typeAnnotation exposedSignatureTypes

            else
                exposedSignatureTypes

        Nothing ->
            exposedSignatureTypes


exposedSignatureTypesForRecordFieldList :
    List (Node TypeAnnotation.RecordField)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForRecordFieldList fields exposedSignatureTypes =
    List.foldl exposedSignatureTypesForRecordField exposedSignatureTypes fields


exposedSignatureTypesForRecordField :
    Node TypeAnnotation.RecordField
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForRecordField (Node _ ( _, typeAnnotation )) exposedSignatureTypes =
    exposedSignatureTypesForTypeAnnotation typeAnnotation exposedSignatureTypes


exposedSignatureTypesForTypeAnnotationList :
    List (Node TypeAnnotation)
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForTypeAnnotationList list exposedSignatureTypes =
    List.foldl exposedSignatureTypesForTypeAnnotation exposedSignatureTypes list


exposedSignatureTypesForTypeAnnotation :
    Node TypeAnnotation
    -> List (Node ( ModuleName, String ))
    -> List (Node ( ModuleName, String ))
exposedSignatureTypesForTypeAnnotation (Node _ typeAnnotation) exposedSignatureTypes =
    case typeAnnotation of
        TypeAnnotation.Typed name list ->
            (name :: exposedSignatureTypes)
                |> exposedSignatureTypesForTypeAnnotationList list

        TypeAnnotation.FunctionTypeAnnotation left right ->
            exposedSignatureTypes
                |> exposedSignatureTypesForTypeAnnotation left
                |> exposedSignatureTypesForTypeAnnotation right

        TypeAnnotation.Tupled list ->
            exposedSignatureTypes
                |> exposedSignatureTypesForTypeAnnotationList list

        TypeAnnotation.Record fields ->
            exposedSignatureTypes
                |> exposedSignatureTypesForRecordFieldList fields

        TypeAnnotation.GenericRecord _ (Node _ fields) ->
            exposedSignatureTypes
                |> exposedSignatureTypesForRecordFieldList fields

        TypeAnnotation.Unit ->
            exposedSignatureTypes

        TypeAnnotation.GenericType _ ->
            exposedSignatureTypes


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    case context of
        InternalModule _ ->
            []

        ExposedModule data ->
            data.exposedSignatureTypes
                |> List.map (Node.map (moduleNameForType data.importedTypes))
                |> List.filter (isTypePrivate data)
                |> List.map (makeError data.exposingListStart)


isTypePrivate : ExposedModuleData -> Node ( ModuleName, String ) -> Bool
isTypePrivate data (Node _ typeCall) =
    case typeCall of
        ( [], name ) ->
            if Set.member name data.declaredTypes then
                not (isTypeExposed data.exposes name)

            else
                False

        ( moduleName, _ ) ->
            not (isModuleExposed data.exposedModules moduleName)


moduleNameForType : Dict String ModuleName -> ( ModuleName, String ) -> ( ModuleName, String )
moduleNameForType importedTypes ( moduleName, typeName ) =
    case Dict.get typeName importedTypes of
        Just typeModuleName ->
            ( typeModuleName, typeName )

        _ ->
            ( moduleName, typeName )


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


fromProjectToModuleContext : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModuleContext _ (Node _ moduleName) { exposedModules, moduleTypes } =
    if isModuleExposed exposedModules moduleName then
        initialExposedModuleContext exposedModules moduleTypes

    else
        initialInternalModuleContext


fromModuleToProjectContext : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProjectContext _ (Node _ moduleName) context =
    case context of
        InternalModule { exposedTypes } ->
            { initialProjectContext | moduleTypes = Dict.singleton moduleName exposedTypes }

        ExposedModule _ ->
            initialProjectContext


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


initialInternalModuleContext : ModuleContext
initialInternalModuleContext =
    InternalModule initialAnyModuleData


initialExposedModuleContext : ExposedModules -> Dict ModuleName (Set String) -> ModuleContext
initialExposedModuleContext exposedModules moduleTypes =
    ExposedModule
        { declaredTypes = Set.empty
        , exposedModules = exposedModules
        , exposedSignatureTypes = []
        , exposes = Exposing.Explicit []
        , exposingListStart = Nothing
        , importedTypes = Dict.empty
        , moduleTypes = moduleTypes
        }


initialAnyModuleData : InternalModuleData
initialAnyModuleData =
    { exposedTypes = Set.empty
    , exposes = Exposing.Explicit []
    }


type alias ProjectContext =
    { exposedModules : ExposedModules
    , moduleTypes : Dict ModuleName (Set String)
    }


type ModuleContext
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
    , importedTypes : Dict String ModuleName
    , moduleTypes : Dict ModuleName (Set String)
    }


type ExposedModules
    = Application
    | Package (Set String)
