module NoMissingTypeAnnotationInLetIn exposing (rule)

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
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import ElmCorePrelude
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import TypeInference exposing (inferType)
import TypeInference.Type as Type exposing (Type)
import TypeInference.TypeByNameLookup as TypeByNameLookup exposing (TypeByNameLookup)


{-| Reports `let in` declarations that do not have a type annotation.

Type annotations help you understand what happens in the code, and it will help the compiler give better error messages.

    config =
        [ NoMissingTypeAnnotationInLetIn.rule
        ]

This rule does not report top-level declarations without a type annotation inside a `let in`.
For that, enable [`NoMissingTypeAnnotation`](./NoMissingTypeAnnotation).


## Fail

    a : number
    a =
        let
            -- Missing annotation
            b =
                2
        in
        b


## Success

    -- Top-level annotation is not necessary, but good to have!
    a : number
    a =
        let
            b : number
            b =
                2
        in
        b


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoMissingTypeAnnotationInLetIn
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoMissingTypeAnnotationInLetIn" initialProjectContext
        |> TypeInference.addProjectVisitors
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor (\import_ context -> ( [], importVisitor (Node.value import_) context ))
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor


type alias ProjectContext =
    { typeInference : TypeInference.ProjectContext
    }


type alias ModuleContext =
    { moduleNameLookupTable : ModuleNameLookupTable
    , typeByNameLookup : TypeByNameLookup
    , typeInference : TypeInference.ModuleContext
    , importedDict : Dict ModuleName Imported
    , declaredTypes : Set String
    }


type Imported
    = Imported { alias : Maybe String, exposed : ExposedTypesFromModule }


type ExposedTypesFromModule
    = Everything
    | Only (Set String)


initialProjectContext : ProjectContext
initialProjectContext =
    { typeInference = TypeInference.initialProjectContext
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            let
                initialContext : ModuleContext
                initialContext =
                    { moduleNameLookupTable = lookupTable
                    , typeByNameLookup = TypeByNameLookup.empty
                    , typeInference = TypeInference.fromProjectToModule projectContext
                    , importedDict = Dict.empty
                    , declaredTypes = Set.empty
                    }
            in
            List.foldl importVisitor initialContext ElmCorePrelude.elmCorePrelude
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { typeInference =
                TypeInference.fromModuleToProject
                    (Rule.moduleNameFromMetadata metadata)
                    moduleContext.typeInference
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { typeInference = TypeInference.foldProjectContexts newContext.typeInference previousContext.typeInference
    }



-- IMPORT VISITOR


importVisitor : Import -> ModuleContext -> ModuleContext
importVisitor import_ context =
    { context
        | importedDict =
            Dict.insert
                (Node.value import_.moduleName)
                (Imported
                    { alias = Maybe.andThen (Node.value >> List.head) import_.moduleAlias
                    , exposed =
                        case Maybe.map Node.value import_.exposingList of
                            Just (Exposing.All _) ->
                                Everything

                            Just (Exposing.Explicit topLevelExpose) ->
                                Only (collectNamesOfExposedTypes topLevelExpose)

                            Nothing ->
                                Only Set.empty
                    }
                )
                context.importedDict
    }


collectNamesOfExposedTypes : List (Node Exposing.TopLevelExpose) -> Set String
collectNamesOfExposedTypes topLevelExposed =
    List.filterMap namesOfExposedType topLevelExposed
        |> Set.fromList


namesOfExposedType : Node Exposing.TopLevelExpose -> Maybe String
namesOfExposedType topLevelExposed =
    case Node.value topLevelExposed of
        Exposing.TypeOrAliasExpose name ->
            Just name

        Exposing.TypeExpose exposedType ->
            Just exposedType.name

        Exposing.InfixExpose _ ->
            Nothing

        Exposing.FunctionExpose _ ->
            Nothing



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    ( []
    , { context
        | declaredTypes =
            List.filterMap declarationVisitor nodes
                |> Set.fromList
      }
    )


declarationVisitor : Node Declaration -> Maybe String
declarationVisitor node =
    case Node.value node of
        Declaration.AliasDeclaration typeAlias ->
            Just (Node.value typeAlias.name)

        Declaration.CustomTypeDeclaration customType ->
            Just (Node.value customType.name)

        Declaration.FunctionDeclaration _ ->
            Nothing

        Declaration.PortDeclaration _ ->
            Nothing

        Declaration.InfixDeclaration _ ->
            Nothing

        Declaration.Destructuring _ _ ->
            Nothing



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor expression context =
    case Node.value expression of
        Expression.LetExpression { declarations } ->
            ( List.filterMap
                (\declaration ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            reportFunctionWithoutSignature context function

                        _ ->
                            Nothing
                )
                declarations
            , context
            )

        _ ->
            ( [], context )


reportFunctionWithoutSignature : ModuleContext -> Expression.Function -> Maybe (Error {})
reportFunctionWithoutSignature context function =
    case function.signature of
        Just _ ->
            Nothing

        Nothing ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration

                maybeType : Maybe String
                maybeType =
                    if List.isEmpty declaration.arguments then
                        inferType context declaration.expression
                            |> Maybe.map (updateAliases context)
                            |> Maybe.andThen Type.toMetadataType
                            |> Maybe.map typeAsString

                    else
                        Nothing
            in
            Rule.errorWithFix
                { message = "Missing type annotation for `" ++ Node.value declaration.name ++ "`"
                , details =
                    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
                    ]
                }
                (Node.range declaration.name)
                (createFix declaration.name maybeType)
                |> Just


createFix : Node String -> Maybe String -> List Fix
createFix functionNameNode maybeInferredType =
    case maybeInferredType of
        Nothing ->
            []

        Just inferredType ->
            let
                functionName : String
                functionName =
                    Node.value functionNameNode

                position : { row : Int, column : Int }
                position =
                    (Node.range functionNameNode).start
            in
            [ Fix.insertAt position (functionName ++ " : " ++ inferredType ++ "\n" ++ String.repeat (position.column - 1) " ") ]


typeAsString : Elm.Type.Type -> String
typeAsString type_ =
    (typeAsStringWithParensMaybe <| Debug.log "type" type_).value
        |> Debug.log "typeAsString"


typeAsStringWithParensMaybe : Elm.Type.Type -> { value : String, mayNeedParens : Bool }
typeAsStringWithParensMaybe type_ =
    case type_ of
        Elm.Type.Var string ->
            { value = string
            , mayNeedParens = False
            }

        Elm.Type.Lambda input output ->
            { value =
                case input of
                    Elm.Type.Lambda _ _ ->
                        typeAsStringWrappedInParens input ++ " -> " ++ typeAsString output

                    _ ->
                        typeAsString input ++ " -> " ++ typeAsString output
            , mayNeedParens = True
            }

        Elm.Type.Tuple types ->
            { value =
                if List.isEmpty types then
                    "()"

                else
                    "( " ++ String.join ", " (List.map typeAsString types) ++ " )"
            , mayNeedParens = False
            }

        Elm.Type.Type string types ->
            { value = String.join " " (string :: List.map typeAsStringWrappedInParens types)
            , mayNeedParens = not (List.isEmpty types)
            }

        Elm.Type.Record fields maybeExtensibleValue ->
            let
                extensibleValueAsString : String
                extensibleValueAsString =
                    case maybeExtensibleValue of
                        Just extensibleValue ->
                            extensibleValue ++ " | "

                        Nothing ->
                            ""
            in
            { value =
                if List.isEmpty fields then
                    "{}"

                else
                    "{ " ++ extensibleValueAsString ++ String.join ", " (List.map recordFieldAsString fields) ++ " }"
            , mayNeedParens = False
            }


typeAsStringWrappedInParens : Elm.Type.Type -> String
typeAsStringWrappedInParens type_ =
    let
        { value, mayNeedParens } =
            typeAsStringWithParensMaybe type_
    in
    if mayNeedParens then
        "(" ++ value ++ ")"

    else
        value


recordFieldAsString : ( String, Elm.Type.Type ) -> String
recordFieldAsString ( fieldName, fieldType ) =
    fieldName ++ " : " ++ typeAsString fieldType


typeAnnotationToElmType : Node TypeAnnotation -> Elm.Type.Type
typeAnnotationToElmType node =
    case Node.value node of
        TypeAnnotation.GenericType var ->
            Elm.Type.Var var

        TypeAnnotation.Typed (Node _ ( moduleName, name )) nodes ->
            Elm.Type.Type (String.join "." (moduleName ++ [ name ])) (List.map typeAnnotationToElmType nodes)

        TypeAnnotation.Unit ->
            Elm.Type.Tuple []

        TypeAnnotation.Tupled nodes ->
            Elm.Type.Tuple (List.map typeAnnotationToElmType nodes)

        TypeAnnotation.Record recordDefinition ->
            Elm.Type.Record
                (List.map
                    (Node.value >> (\( fieldName, fieldType ) -> ( Node.value fieldName, typeAnnotationToElmType fieldType )))
                    recordDefinition
                )
                Nothing

        TypeAnnotation.GenericRecord genericVar recordDefinition ->
            Elm.Type.Record
                (List.map
                    (Node.value >> (\( fieldName, fieldType ) -> ( Node.value fieldName, typeAnnotationToElmType fieldType )))
                    (Node.value recordDefinition)
                )
                (Just (Node.value genericVar))

        TypeAnnotation.FunctionTypeAnnotation input output ->
            Elm.Type.Lambda (typeAnnotationToElmType input) (typeAnnotationToElmType output)


updateAliases : ModuleContext -> Type -> Type
updateAliases context type_ =
    case type_ of
        Type.Type moduleName name types ->
            let
                moduleNameToUse : ModuleName
                moduleNameToUse =
                    case Dict.get moduleName context.importedDict of
                        Just (Imported { alias, exposed }) ->
                            if not (Set.member name context.declaredTypes) && isTypeImportedSomehow exposed name then
                                []

                            else
                                case alias of
                                    Just alias_ ->
                                        [ alias_ ]

                                    Nothing ->
                                        moduleName

                        Nothing ->
                            moduleName
            in
            Type.Type moduleNameToUse name (List.map (updateAliases context) types)

        Type.Unknown ->
            type_

        Type.Generic _ ->
            type_

        Type.Function input output ->
            Type.Function (updateAliases context input) (updateAliases context output)

        Type.Tuple types ->
            Type.Tuple (List.map (updateAliases context) types)

        Type.Record record ->
            Type.Record { record | fields = List.map (Tuple.mapSecond (updateAliases context)) record.fields }


isTypeImportedSomehow : ExposedTypesFromModule -> String -> Bool
isTypeImportedSomehow exposed name =
    case exposed of
        Everything ->
            True

        Only importedTypes ->
            Set.member name importedTypes
