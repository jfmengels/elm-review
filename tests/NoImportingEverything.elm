module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern, QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbids importing everything from a module.

🔧 Running with `--fix` will automatically fix all the reported errors.

When you import everything from a module, it becomes harder to know where a function
or a type comes from. The official guide even
[recommends against importing everything](https://guide.elm-lang.org/webapps/modules.html#using-modules).

    config =
        [ NoImportingEverything.rule []
        ]

Teams often have an agreement on the list of imports from which it is okay to expose everything, so
you can configure a list of exceptions.

    config =
        [ NoImportingEverything.rule [ "Html", "Some.Module" ]
        ]


## Fail

    import A exposing (..)
    import A as B exposing (..)


## Success

    import A as B exposing (B(..), C, d)

    -- If configured with `[ "Html" ]`
    import Html exposing (..)


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoImportingEverything
```

-}
rule : List String -> Rule
rule exceptions =
    Rule.newProjectRuleSchema "NoImportingEverything" initialContext
        |> Rule.withDirectDependenciesProjectVisitor (\dependencies _ -> ( [], dependenciesVisitor dependencies ))
        |> Rule.withModuleVisitor (moduleVisitor exceptions)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    Dict ModuleName (Dict String String)


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , importsExposingAll : Dict ModuleName ImportExposingAll
    , exposedTypes : ExposedTypes
    , constructorToType : Dict ModuleName (Dict String String)
    , localConstructorToType : Dict String String
    }


type ExposedTypes
    = ExposesAll
    | ExposesConstructorsOf (Set String)


type alias ImportExposingAll =
    { node : Node Import
    , exposingRange : Range
    , values : Set String
    }


initialContext : ProjectContext
initialContext =
    Dict.empty


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable constructorToType ->
            { lookupTable = lookupTable
            , importsExposingAll = Dict.empty
            , exposedTypes = ExposesAll
            , constructorToType = constructorToType
            , localConstructorToType = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleContext ->
            if Dict.isEmpty moduleContext.localConstructorToType then
                Dict.empty

            else
                Dict.singleton moduleName moduleContext.localConstructorToType
        )
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts =
    Dict.union


dependenciesVisitor : Dict String Dependency -> ProjectContext
dependenciesVisitor dependencies =
    Dict.foldl
        (\_ dep acc -> List.foldl findConstructorsInModule acc (Dependency.modules dep))
        Dict.empty
        dependencies


findConstructorsInModule : Elm.Docs.Module -> Dict ModuleName (Dict String String) -> Dict ModuleName (Dict String String)
findConstructorsInModule mod dict =
    let
        newConstructors : Dict String String
        newConstructors =
            List.foldl
                (\union acc ->
                    List.foldl
                        (\( constructorName, _ ) subAcc ->
                            Dict.insert constructorName union.name subAcc
                        )
                        acc
                        union.tags
                )
                Dict.empty
                mod.unions
    in
    if Dict.isEmpty newConstructors then
        dict

    else
        Dict.insert (String.split "." mod.name) newConstructors dict


moduleVisitor :
    List String
    -> Rule.ModuleRuleSchema schemaState ModuleContext
    -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor exceptions schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))
        |> Rule.withFinalModuleEvaluation finalEvaluation


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List empty, ModuleContext )
moduleDefinitionVisitor (Node _ module_) context =
    case Module.exposingList module_ of
        Exposing.All _ ->
            ( [], { context | exposedTypes = ExposesAll } )

        Exposing.Explicit list ->
            let
                constructors : Set String
                constructors =
                    List.foldl
                        (\(Node _ item) acc ->
                            case item of
                                Exposing.TypeExpose { name } ->
                                    Set.insert name acc

                                _ ->
                                    acc
                        )
                        Set.empty
                        list
            in
            ( [], { context | exposedTypes = ExposesConstructorsOf constructors } )


exceptionsToSet : List String -> Set ModuleName
exceptionsToSet exceptions =
    List.foldl
        (\moduleName acc -> Set.insert (String.split "." moduleName) acc)
        Set.empty
        exceptions


importVisitor : Set ModuleName -> Node Import -> ModuleContext -> ( List (Error nothing), ModuleContext )
importVisitor exceptions node context =
    let
        moduleName : ModuleName
        moduleName =
            node
                |> Node.value
                |> .moduleName
                |> Node.value
    in
    if Set.member moduleName exceptions then
        ( [], context )

    else
        case (Node.value node).exposingList of
            Just (Node _ (Exposing.All allRange)) ->
                ( []
                , { context
                    | importsExposingAll =
                        Dict.insert moduleName
                            { node = node
                            , exposingRange = allRange
                            , values = Set.empty
                            }
                            context.importsExposingAll
                  }
                )

            _ ->
                ( [], context )


declarationVisitor : Node Declaration -> ModuleContext -> ( List empty, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            ( []
            , (case signature of
                Just (Node _ { typeAnnotation }) ->
                    context
                        |> visitTypeAnnotation [ typeAnnotation ]

                Nothing ->
                    context
              )
                |> visitFunctionArgumentPatterns (Node.value declaration).arguments
            )

        Declaration.CustomTypeDeclaration type_ ->
            let
                typeName : String
                typeName =
                    Node.value type_.name

                isConstructorsExposed_ : Bool
                isConstructorsExposed_ =
                    isConstructorsExposed typeName context

                newContext : ModuleContext
                newContext =
                    List.foldl
                        (\(Node _ constructor) ctx ->
                            visitTypeAnnotation constructor.arguments
                                (if isConstructorsExposed_ then
                                    { ctx | localConstructorToType = Dict.insert (Node.value constructor.name) typeName ctx.localConstructorToType }

                                 else
                                    ctx
                                )
                        )
                        context
                        type_.constructors
            in
            ( [], newContext )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( [], visitTypeAnnotation [ typeAnnotation ] context )

        _ ->
            ( [], context )


visitTypeAnnotation : List (Node TypeAnnotation) -> ModuleContext -> ModuleContext
visitTypeAnnotation typeAnnotations context =
    case typeAnnotations of
        [] ->
            context

        typeAnnotation :: rest ->
            case Node.value typeAnnotation of
                TypeAnnotation.Typed (Node range ( [], name )) subTypes ->
                    let
                        newContext : ModuleContext
                        newContext =
                            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                                Just moduleName ->
                                    useImportedType moduleName name context

                                Nothing ->
                                    context
                    in
                    visitTypeAnnotation (subTypes ++ rest) newContext

                TypeAnnotation.Typed _ subTypes ->
                    visitTypeAnnotation (subTypes ++ rest) context

                TypeAnnotation.Tupled subTypes ->
                    visitTypeAnnotation (subTypes ++ rest) context

                TypeAnnotation.Record fields ->
                    visitTypeAnnotation (mapAndAppend (Node.value >> Tuple.second) fields rest) context

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    visitTypeAnnotation (mapAndAppend (Node.value >> Tuple.second) fields rest) context

                TypeAnnotation.FunctionTypeAnnotation fn arg ->
                    visitTypeAnnotation (fn :: arg :: rest) context

                _ ->
                    visitTypeAnnotation rest context


mapAndAppend : (a -> b) -> List a -> List b -> List b
mapAndAppend fn list initial =
    List.foldl (\x acc -> fn x :: acc) initial list


constructorsInPattern : ModuleNameLookupTable -> List (Node Pattern) -> Set ( ModuleName, String ) -> Set ( ModuleName, String )
constructorsInPattern lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                Pattern.NamedPattern qualifiedNameRef patterns ->
                    constructorsInPattern
                        lookupTable
                        (patterns ++ restOfNodes)
                        (addNamedPattern lookupTable node qualifiedNameRef acc)

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


addNamedPattern : ModuleNameLookupTable -> Node a -> QualifiedNameRef -> Set ( ModuleName, String ) -> Set ( ModuleName, String )
addNamedPattern lookupTable node qualifiedNameRef acc =
    if List.isEmpty qualifiedNameRef.moduleName then
        case ModuleNameLookupTable.fullModuleNameFor lookupTable node of
            Just realModuleName ->
                Set.insert ( realModuleName, qualifiedNameRef.name ) acc

            Nothing ->
                acc

    else
        acc


isConstructorsExposed : String -> { context | exposedTypes : ExposedTypes } -> Bool
isConstructorsExposed name context =
    case context.exposedTypes of
        ExposesAll ->
            True

        ExposesConstructorsOf set ->
            Set.member name set


expressionVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            useImportedValue name (Node.range node) context

        Expression.RecordUpdateExpression (Node nameRange name) _ ->
            useImportedValue name nameRange context

        Expression.LambdaExpression { args } ->
            visitFunctionArgumentPatterns args context

        Expression.LetExpression { declarations } ->
            visitLetExpression declarations context

        Expression.CaseExpression case_ ->
            let
                newImportsExposingAll : Dict ModuleName ImportExposingAll
                newImportsExposingAll =
                    List.foldl
                        (\( casePattern, _ ) importsExposingAll ->
                            Set.foldl
                                (\( moduleName, constructorName ) subImportsExposingAll ->
                                    useImportedTypeConstructor moduleName constructorName context.constructorToType subImportsExposingAll
                                )
                                importsExposingAll
                                (constructorsInPattern context.lookupTable [ casePattern ] Set.empty)
                        )
                        context.importsExposingAll
                        case_.cases
            in
            { context | importsExposingAll = newImportsExposingAll }

        Expression.OperatorApplication op _ _ _ ->
            useImportedValue ("(" ++ op ++ ")") (Node.range node) context

        Expression.PrefixOperator op ->
            useImportedValue ("(" ++ op ++ ")") (Node.range node) context

        _ ->
            context


visitLetExpression : List (Node Expression.LetDeclaration) -> ModuleContext -> ModuleContext
visitLetExpression declarations context =
    List.foldl
        (\(Node _ letDeclaration) ctx ->
            case letDeclaration of
                Expression.LetFunction { signature, declaration } ->
                    (case signature of
                        Just (Node _ { typeAnnotation }) ->
                            ctx
                                |> visitTypeAnnotation [ typeAnnotation ]

                        Nothing ->
                            ctx
                    )
                        |> visitFunctionArgumentPatterns (Node.value declaration).arguments

                Expression.LetDestructuring pattern _ ->
                    visitFunctionArgumentPatterns [ pattern ] ctx
        )
        context
        declarations


visitFunctionArgumentPatterns : List (Node Pattern) -> ModuleContext -> ModuleContext
visitFunctionArgumentPatterns patterns context =
    List.foldl
        (\pattern importsExposingAll ->
            Set.foldl
                (\( moduleName, constructorName ) subImportsExposingAll ->
                    useImportedTypeConstructor moduleName constructorName context.constructorToType subImportsExposingAll
                )
                importsExposingAll
                (constructorsInPattern context.lookupTable [ pattern ] Set.empty)
        )
        context.importsExposingAll
        patterns
        |> (\importsExposingAll -> { context | importsExposingAll = importsExposingAll })


useImportedValue : String -> Range -> ModuleContext -> ModuleContext
useImportedValue name range context =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
        Nothing ->
            context

        Just moduleName ->
            case Dict.get moduleName context.importsExposingAll of
                Nothing ->
                    context

                Just importExposingAll ->
                    let
                        insertionName : String
                        insertionName =
                            case Dict.get moduleName context.constructorToType |> Maybe.andThen (Dict.get name) of
                                Just typeName ->
                                    typeName ++ "(..)"

                                Nothing ->
                                    name

                        importsExposingAll : Dict ModuleName ImportExposingAll
                        importsExposingAll =
                            Dict.insert
                                moduleName
                                { importExposingAll | values = Set.insert insertionName importExposingAll.values }
                                context.importsExposingAll
                    in
                    { context | importsExposingAll = importsExposingAll }


useImportedTypeConstructor : ModuleName -> String -> Dict ModuleName (Dict String String) -> Dict ModuleName ImportExposingAll -> Dict ModuleName ImportExposingAll
useImportedTypeConstructor moduleName constructorName constructorToType importsExposingAll =
    case Dict.get moduleName importsExposingAll of
        Nothing ->
            importsExposingAll

        Just importExposingAll ->
            case Dict.get moduleName constructorToType |> Maybe.andThen (Dict.get constructorName) of
                Just typeName ->
                    Dict.insert
                        moduleName
                        { importExposingAll | values = Set.insert (typeName ++ "(..)") importExposingAll.values }
                        importsExposingAll

                Nothing ->
                    importsExposingAll


useImportedType : ModuleName -> String -> ModuleContext -> ModuleContext
useImportedType moduleName typeName context =
    case Dict.get moduleName context.importsExposingAll of
        Nothing ->
            context

        Just importExposingAll ->
            let
                importsExposingAll : Dict ModuleName ImportExposingAll
                importsExposingAll =
                    Dict.insert
                        moduleName
                        { importExposingAll | values = Set.insert typeName importExposingAll.values }
                        context.importsExposingAll
            in
            { context | importsExposingAll = importsExposingAll }


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    Dict.foldr
        (\_ value valueList -> importError value :: valueList)
        []
        context.importsExposingAll


importError : ImportExposingAll -> Error {}
importError ({ exposingRange } as importExposingAll) =
    Rule.errorWithFix
        { message = "Prefer listing what you wish to import and/or using qualified imports"
        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
        }
        { start = { row = exposingRange.start.row, column = exposingRange.start.column - 1 }
        , end = { row = exposingRange.end.row, column = exposingRange.end.column + 1 }
        }
        [ exposingFix importExposingAll ]


exposingFix : ImportExposingAll -> Fix
exposingFix { node, exposingRange, values } =
    if Set.isEmpty values then
        removeExposingFix node

    else
        replaceExposingFix values exposingRange


removeExposingFix : Node Import -> Fix
removeExposingFix (Node { end } import_) =
    let
        startRange : Range
        startRange =
            case import_.moduleAlias of
                Just (Node aliasRange _) ->
                    aliasRange

                Nothing ->
                    Node.range import_.moduleName
    in
    Fix.replaceRangeBy { start = startRange.end, end = end } ""


replaceExposingFix : Set String -> Range -> Fix
replaceExposingFix values range =
    values
        |> Set.foldr
            (\value acc ->
                if Set.member (value ++ "(..)") values then
                    acc

                else
                    value :: acc
            )
            []
        |> String.join ", "
        |> Fix.replaceRangeBy range
