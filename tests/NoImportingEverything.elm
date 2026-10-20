module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern, QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoImportingEverything" initialContext
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , constructorToType : Dict ModuleName (Dict String String)
    , importsExposingAll : Dict ModuleName ImportExposingAll
    }


type alias ImportExposingAll =
    { node : Node Import
    , exposingRange : Range
    , values : Set String
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable constructorToType _ ->
            { lookupTable = lookupTable
            , constructorToType = constructorToType
            , importsExposingAll = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withConstructorToType { implicitImports = False, dependencies = True }


exceptionsToSet : List String -> Set ModuleName
exceptionsToSet exceptions =
    List.foldl
        (\moduleName acc -> Set.insert (String.split "." moduleName) acc)
        Set.empty
        exceptions


importVisitor : Set ModuleName -> Node Import -> Context -> ( List (Error nothing), Context )
importVisitor exceptions ((Node _ import_) as node) context =
    let
        (Node _ moduleName) =
            import_.moduleName
    in
    if Set.member moduleName exceptions then
        ( [], context )

    else
        case import_.exposingList of
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


declarationVisitor : Node Declaration -> Context -> ( List empty, Context )
declarationVisitor (Node _ decl) context =
    case decl of
        Declaration.FunctionDeclaration { signature, declaration } ->
            ( []
            , (case signature of
                Just (Node _ { typeAnnotation }) ->
                    visitTypeAnnotation [ typeAnnotation ] context

                Nothing ->
                    context
              )
                |> visitFunctionArgumentPatterns (Node.value declaration).arguments
            )

        Declaration.CustomTypeDeclaration type_ ->
            let
                newContext : Context
                newContext =
                    List.foldl
                        (\(Node _ constructor) ctx ->
                            visitTypeAnnotation constructor.arguments ctx
                        )
                        context
                        type_.constructors
            in
            ( [], newContext )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( [], visitTypeAnnotation [ typeAnnotation ] context )

        _ ->
            ( [], context )


visitTypeAnnotation : List (Node TypeAnnotation) -> Context -> Context
visitTypeAnnotation typeAnnotations context =
    case typeAnnotations of
        [] ->
            context

        (Node _ typeAnnotation) :: rest ->
            case typeAnnotation of
                TypeAnnotation.Typed (Node range ( [], name )) subTypes ->
                    let
                        newContext : Context
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
                    visitTypeAnnotation (mapAndAppend (\(Node _ ( _, value )) -> value) fields rest) context

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    visitTypeAnnotation (mapAndAppend (\(Node _ ( _, value )) -> value) fields rest) context

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

        (Node range pattern) :: restOfNodes ->
            case pattern of
                Pattern.NamedPattern qualifiedNameRef patterns ->
                    constructorsInPattern
                        lookupTable
                        (patterns ++ restOfNodes)
                        (addNamedPattern lookupTable range qualifiedNameRef acc)

                Pattern.TuplePattern patterns ->
                    constructorsInPattern lookupTable (patterns ++ restOfNodes) acc

                Pattern.UnConsPattern left right ->
                    constructorsInPattern lookupTable (left :: right :: restOfNodes) acc

                Pattern.ListPattern patterns ->
                    constructorsInPattern lookupTable (patterns ++ restOfNodes) acc

                Pattern.AsPattern subPattern _ ->
                    constructorsInPattern lookupTable (subPattern :: restOfNodes) acc

                Pattern.ParenthesizedPattern subPattern ->
                    constructorsInPattern lookupTable (subPattern :: restOfNodes) acc

                _ ->
                    constructorsInPattern lookupTable restOfNodes acc


addNamedPattern : ModuleNameLookupTable -> Range -> QualifiedNameRef -> Set ( ModuleName, String ) -> Set ( ModuleName, String )
addNamedPattern lookupTable range qualifiedNameRef acc =
    if List.isEmpty qualifiedNameRef.moduleName then
        case ModuleNameLookupTable.fullModuleNameAt lookupTable range of
            Just realModuleName ->
                Set.insert ( realModuleName, qualifiedNameRef.name ) acc

            Nothing ->
                acc

    else
        acc


expressionVisitor : Node Expression -> Context -> Context
expressionVisitor (Node range expr) context =
    case expr of
        Expression.FunctionOrValue [] name ->
            useImportedValue name range context

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
            useImportedValue ("(" ++ op ++ ")") range context

        Expression.PrefixOperator op ->
            useImportedValue ("(" ++ op ++ ")") range context

        _ ->
            context


visitLetExpression : List (Node Expression.LetDeclaration) -> Context -> Context
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


visitFunctionArgumentPatterns : List (Node Pattern) -> Context -> Context
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


useImportedValue : String -> Range -> Context -> Context
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


useImportedType : ModuleName -> String -> Context -> Context
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


finalEvaluation : Context -> List (Error {})
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
