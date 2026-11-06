module NoRedundantlyQualifiedType exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..))
import Elm.Syntax.Expression as Expression exposing (Function, LetDeclaration(..))
import Elm.Syntax.File
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (RecordField, TypeAnnotation(..))
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports when a type is qualified by a module (alias) of the same name.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoRedundantlyQualifiedType.rule
        ]

There is a convention in Elm to center modules around a type, and to name the module after the type. Usually, such a module is imported like this:

    import Inventory.Fruit as Fruit exposing (Fruit)

That allows you to refer to values like `Fruit.apple` and to the type `Fruit`. When inferring types, you might end up with `Fruit.Fruit`. It can be considered a bit silly to say the same word twice like that. It's clear where the `Fruit` type comes from anyway. This rule simplifies to just `Fruit`.


## Fail

    import Set

    directions : Set.Set String
    directions =
        Set.fromList [ "north", "south", "east", "west" ]


## Success

    import Set exposing (Set)

    directions : Set String
    directions =
        Set.fromList [ "north", "south", "east", "west" ]


## When (not) to enable this rule

This rule resolves a minor style issue, and may not be worth enforcing depending on how strongly you feel about this issue.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoRedundantlyQualifiedType
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoRedundantlyQualifiedType" initialContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationEnterVisitor (\node context -> ( declarationVisitor node context, context ))
        |> Rule.withLetDeclarationEnterVisitor (\_ node context -> ( letDeclarationEnterVisitor node context, context ))


type alias ProjectContext =
    { exposedTypes : Dict ModuleName (Set String)
    , preludeImportedTypes : Dict String ImportedType
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , imports : List (Node Import)
    , typesDefinedInModule : Set String
    , importedTypes : Dict String ImportedType
    }


type ImportedType
    = FromSingleModule ModuleName
    | FromMultipleModules


initialContext : ProjectContext
initialContext =
    { exposedTypes = Dict.empty
    , preludeImportedTypes = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable ast projectContext ->
            { lookupTable = lookupTable
            , imports = ast.imports
            , typesDefinedInModule = collectTypesDefinedInModule ast.declarations
            , importedTypes = List.foldl (collectImportedTypes projectContext.exposedTypes) projectContext.preludeImportedTypes ast.imports
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withFullAst


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName ast moduleContext ->
            { exposedTypes = collectExposedFromModule moduleName ast moduleContext
            , preludeImportedTypes = Dict.empty
            }
        )
        |> Rule.withModuleName
        |> Rule.withFullAst


collectExposedFromModule : ModuleName -> Elm.Syntax.File.File -> ModuleContext -> Dict ModuleName (Set String)
collectExposedFromModule moduleName ast moduleContext =
    let
        exposedTypes : Set String
        exposedTypes =
            case Module.exposingList (Node.value ast.moduleDefinition) of
                Exposing.All _ ->
                    moduleContext.typesDefinedInModule

                Explicit nodes ->
                    List.foldl
                        (\(Node _ topLevelExposeNode) acc ->
                            case topLevelExposeNode of
                                Exposing.FunctionExpose _ ->
                                    acc

                                Exposing.InfixExpose _ ->
                                    acc

                                Exposing.TypeOrAliasExpose name ->
                                    Set.insert name acc

                                Exposing.TypeExpose { name } ->
                                    Set.insert name acc
                        )
                        Set.empty
                        nodes
    in
    if Set.isEmpty exposedTypes then
        Dict.empty

    else
        Dict.singleton moduleName exposedTypes


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedTypes = Dict.union newContext.exposedTypes previousContext.exposedTypes
    , preludeImportedTypes = previousContext.preludeImportedTypes
    }


dependenciesVisitor : Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        exposedTypes : Dict ModuleName (Set String)
        exposedTypes =
            Dict.foldl
                (\_ dep acc ->
                    List.foldl
                        (\{ name, unions, aliases } subAcc ->
                            let
                                accWithUnions : Set String
                                accWithUnions =
                                    List.foldl
                                        (\union subSubAcc -> Set.insert union.name subSubAcc)
                                        Set.empty
                                        unions

                                accWithUnionsAndAliases : Set String
                                accWithUnionsAndAliases =
                                    List.foldl
                                        (\alias_ subSubAcc -> Set.insert alias_.name subSubAcc)
                                        accWithUnions
                                        aliases
                            in
                            Dict.insert (String.split "." name) accWithUnionsAndAliases subAcc
                        )
                        acc
                        (Review.Project.Dependency.modules dep)
                )
                projectContext.exposedTypes
                dependencies
    in
    ( []
    , { exposedTypes = exposedTypes
      , preludeImportedTypes = List.foldl (collectImportedTypes exposedTypes) Dict.empty elmCorePrelude
      }
    )


collectTypesDefinedInModule : List (Node Declaration) -> Set String
collectTypesDefinedInModule list =
    List.foldl
        (\(Node _ decl) set ->
            case decl of
                AliasDeclaration typeAlias ->
                    Set.insert (Node.value typeAlias.name) set

                CustomTypeDeclaration type_ ->
                    Set.insert (Node.value type_.name) set

                _ ->
                    set
        )
        Set.empty
        list


declarationVisitor : Node Declaration -> ModuleContext -> List (Rule.Error {})
declarationVisitor (Node _ decl) context =
    case decl of
        FunctionDeclaration function ->
            doFunction context function

        AliasDeclaration typeAlias ->
            doTypeAnnotation context typeAlias.typeAnnotation

        CustomTypeDeclaration type_ ->
            List.concatMap
                (\(Node _ { arguments }) -> List.concatMap (doTypeAnnotation context) arguments)
                type_.constructors

        PortDeclaration signature ->
            doTypeAnnotation context signature.typeAnnotation

        InfixDeclaration _ ->
            []

        Destructuring _ _ ->
            []


letDeclarationEnterVisitor : Node Expression.LetDeclaration -> ModuleContext -> List (Rule.Error {})
letDeclarationEnterVisitor (Node _ letDeclaration) context =
    case letDeclaration of
        LetFunction function ->
            doFunction context function

        LetDestructuring _ _ ->
            []


doFunction : ModuleContext -> Function -> List (Rule.Error {})
doFunction context function =
    case function.signature of
        Just (Node _ signature) ->
            doTypeAnnotation context signature.typeAnnotation

        Nothing ->
            []


doTypeAnnotation : ModuleContext -> Node TypeAnnotation -> List (Rule.Error {})
doTypeAnnotation context (Node _ typeAnnotation) =
    case typeAnnotation of
        GenericType _ ->
            []

        Typed constructor arguments ->
            doConstructor context constructor ++ (arguments |> List.concatMap (doTypeAnnotation context))

        Unit ->
            []

        Tupled arguments ->
            arguments |> List.concatMap (doTypeAnnotation context)

        Record recordDefinition ->
            doRecordDefinition recordDefinition context

        GenericRecord _ (Node _ recordDefinition) ->
            doRecordDefinition recordDefinition context

        FunctionTypeAnnotation left right ->
            doTypeAnnotation context left ++ doTypeAnnotation context right


doRecordDefinition : List (Node RecordField) -> ModuleContext -> List (Rule.Error {})
doRecordDefinition recordDefinition context =
    List.concatMap
        (\(Node _ ( _, typeAnnotation )) ->
            doTypeAnnotation context typeAnnotation
        )
        recordDefinition


doConstructor : ModuleContext -> Node ( ModuleName, String ) -> List (Rule.Error {})
doConstructor context (Node range constructor) =
    case constructor of
        ( [ qualifier ], name ) ->
            if qualifier /= name || Set.member name context.typesDefinedInModule then
                []

            else
                case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                    Just moduleName ->
                        reportErrorBasedOnImportedTypes context moduleName range name

                    -- Should not happen.
                    Nothing ->
                        []

        _ ->
            []


reportErrorBasedOnImportedTypes : ModuleContext -> ModuleName -> Range -> String -> List (Rule.Error {})
reportErrorBasedOnImportedTypes context moduleName range name =
    case Dict.get name context.importedTypes of
        Nothing ->
            [ reportError name range (importFix moduleName name context.imports) ]

        Just (FromSingleModule moduleNameWhichExposesType) ->
            if moduleName == moduleNameWhichExposesType then
                [ reportError name range [] ]

            else
                []

        Just FromMultipleModules ->
            []


reportError : String -> Range -> List Fix -> Rule.Error {}
reportError name range additionalFixes =
    Rule.errorWithFix
        { message = "This type can be simplified to just `" ++ name ++ "`."
        , details = [ "It can be considered a bit silly to say the same word twice like in `" ++ name ++ "." ++ name ++ "`. This rule simplifies to just `" ++ name ++ "`. This follows the convention of centering modules around a type." ]
        }
        range
        (Fix.removeRange
            { start = range.start
            , end =
                { row = range.start.row

                -- Add 1 to the column to remove the dot.
                , column = range.start.column + String.length name + 1
                }
            }
            :: additionalFixes
        )


collectImportedTypes : Dict ModuleName (Set String) -> Node Import -> Dict String ImportedType -> Dict String ImportedType
collectImportedTypes exposedTypes (Node _ importNode) set =
    case importNode.exposingList of
        Just (Node _ exposing_) ->
            let
                (Node _ moduleName) =
                    importNode.moduleName
            in
            case exposing_ of
                Exposing.All _ ->
                    case Dict.get moduleName exposedTypes of
                        Nothing ->
                            set

                        Just typesInImportedModule ->
                            Set.foldl (\name acc -> collectType moduleName name acc) set typesInImportedModule

                Exposing.Explicit nodes ->
                    List.foldl
                        (\(Node _ topLevelExposeNode) acc ->
                            case topLevelExposeNode of
                                Exposing.FunctionExpose _ ->
                                    set

                                Exposing.InfixExpose _ ->
                                    set

                                Exposing.TypeOrAliasExpose name ->
                                    collectType moduleName name acc

                                Exposing.TypeExpose { name } ->
                                    collectType moduleName name acc
                        )
                        set
                        nodes

        Nothing ->
            set


collectType : ModuleName -> String -> Dict String ImportedType -> Dict String ImportedType
collectType moduleName name dict =
    case Dict.get name dict of
        Nothing ->
            Dict.insert name (FromSingleModule moduleName) dict

        Just (FromSingleModule previousModuleName) ->
            if previousModuleName == moduleName then
                dict

            else
                Dict.insert name FromMultipleModules dict

        Just FromMultipleModules ->
            dict


importFix : ModuleName -> String -> List (Node Import) -> List Fix
importFix moduleName name imports =
    let
        matchingImport : Maybe (Node Import)
        matchingImport =
            find (\(Node _ import_) -> Node.value import_.moduleName == moduleName) imports
    in
    case matchingImport of
        Just (Node importRange importNode) ->
            case importNode.exposingList of
                Just (Node _ (Exposing.All _)) ->
                    []

                Just (Node _ (Exposing.Explicit ((Node firstRange _) :: _))) ->
                    [ Fix.insertAt firstRange.start (name ++ ", ") ]

                _ ->
                    [ Fix.insertAt importRange.end (" exposing (" ++ name ++ ")") ]

        Nothing ->
            -- If no matching imports, assume it is part of the default imports.
            []


elmCorePrelude : List (Node Import)
elmCorePrelude =
    let
        explicit : List Exposing.TopLevelExpose -> Maybe Exposing
        explicit exposed =
            exposed
                |> List.map (Node Range.emptyRange)
                |> Exposing.Explicit
                |> Just
    in
    -- These are the default imports implicitly added by the Elm compiler
    -- https://package.elm-lang.org/packages/elm/core/latest
    [ createFakeImport
        { moduleName = [ "Basics" ]
        , moduleAlias = Nothing
        , exposingList = Just <| Exposing.All Range.emptyRange
        }
    , createFakeImport
        { moduleName = [ "List" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "List", open = Nothing }
                , Exposing.InfixExpose "::"
                ]
        }
    , createFakeImport
        { moduleName = [ "Maybe" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Maybe", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "Result" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Result", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "String" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "String", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Char" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Char", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Tuple" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Debug" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Platform" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Program", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Cmd" ]
        , moduleAlias = Just "Cmd"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Cmd", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Sub" ]
        , moduleAlias = Just "Sub"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Sub", open = Nothing }
                ]
        }
    ]


createFakeImport : { moduleName : ModuleName, exposingList : Maybe Exposing, moduleAlias : Maybe String } -> Node Import
createFakeImport { moduleName, moduleAlias, exposingList } =
    Node Range.emptyRange
        { moduleName = Node Range.emptyRange moduleName
        , moduleAlias = moduleAlias |> Maybe.map (List.singleton >> Node Range.emptyRange)
        , exposingList = exposingList |> Maybe.map (Node Range.emptyRange)
        }


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest
