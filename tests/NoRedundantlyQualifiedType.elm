module NoRedundantlyQualifiedType exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Function, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (RecordField, TypeAnnotation(..))
import Elm.Type
import Review.Fix as Fix exposing (Fix)
import Review.Resources.ImportBinding as ImportBinding
import Review.Resources.Scope as Scope
import Review.Resources.ScopeProvider as ScopeProvider exposing (ScopeProvider)
import Review.Rule as Rule exposing (Rule)


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
    Rule.newModuleRuleSchemaUsingContextCreator "NoRedundantlyQualifiedType" initialContext
        |> Rule.withDeclarationEnterVisitor (\node context -> ( declarationVisitor node context, context ))
        |> Rule.withLetDeclarationEnterVisitor (\_ node context -> ( letDeclarationEnterVisitor node context, context ))
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopeProvider : ScopeProvider
    , moduleDocs : Dict ModuleName Elm.Docs.Module
    , imports : List (Node Import)
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\scopeProvider ast moduleDocs () ->
            { scopeProvider = scopeProvider
            , moduleDocs = moduleDocs
            , imports = ast.imports
            }
        )
        |> Rule.withScopes
        |> Rule.withFullAst
        |> Rule.withImportedModuleDocs { implicitImports = True, dependencies = True }


declarationVisitor : Node Declaration -> Context -> List (Rule.Error {})
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


letDeclarationEnterVisitor : Node Expression.LetDeclaration -> Context -> List (Rule.Error {})
letDeclarationEnterVisitor (Node _ letDeclaration) context =
    case letDeclaration of
        LetFunction function ->
            doFunction context function

        LetDestructuring _ _ ->
            []


doFunction : Context -> Function -> List (Rule.Error {})
doFunction context function =
    case function.signature of
        Just (Node _ signature) ->
            doTypeAnnotation context signature.typeAnnotation

        Nothing ->
            []


doTypeAnnotation : Context -> Node TypeAnnotation -> List (Rule.Error {})
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


doRecordDefinition : List (Node RecordField) -> Context -> List (Rule.Error {})
doRecordDefinition recordDefinition context =
    List.concatMap
        (\(Node _ ( _, typeAnnotation )) ->
            doTypeAnnotation context typeAnnotation
        )
        recordDefinition


doConstructor : Context -> Node ( ModuleName, String ) -> List (Rule.Error {})
doConstructor context (Node range constructor) =
    case constructor of
        ( [ qualifier ], name ) ->
            if
                (qualifier /= name)
                    || (ScopeProvider.typeBinding context.scopeProvider name /= Nothing)
            then
                []

            else
                reportErrorBasedOnImportedTypes
                    context
                    (ScopeProvider.moduleNameForType context.scopeProvider [ qualifier ] name)
                    range
                    name

        _ ->
            []


reportErrorBasedOnImportedTypes : Context -> ModuleName -> Range -> String -> List (Rule.Error {})
reportErrorBasedOnImportedTypes context moduleName range name =
    case ScopeProvider.importTypeBinding context.scopeProvider name of
        Nothing ->
            -- No type with this name has been imported
            -- We have to check whether introducing one would create a conflict
            if wouldIntroduceValueConflict context moduleName name then
                []

            else
                [ reportError name range (importFix moduleName name context.imports) ]

        Just binding ->
            -- A type with this name is already imported
            if moduleName == ImportBinding.moduleName binding then
                -- and it's the one we're looking for. No need to change the import.
                [ reportError name range [] ]

            else
                -- and it's for a different module. Importing it would create a compiler error
                -- so we don't want to report an error
                []


wouldIntroduceValueConflict : Context -> ModuleName -> String -> Bool
wouldIntroduceValueConflict context moduleName name =
    -- There is no local value that would shadow an imported constructor
    (Scope.valueBinding (ScopeProvider.topLevel context.scopeProvider) name == Nothing)
        -- Another import already imports a constructor with this name
        -- Importing the constructor would introduce an ambiguity compiler error.
        && (ScopeProvider.importValueBinding context.scopeProvider name /= Nothing)
        -- The type introduces a constructor (slower to check than the conditions above)
        && introducesTypeConstructor context moduleName name


introducesTypeConstructor : Context -> ModuleName -> String -> Bool
introducesTypeConstructor context moduleName name =
    case Dict.get moduleName context.moduleDocs of
        Just { aliases } ->
            case find (\alias -> alias.name == name) aliases of
                Just alias ->
                    case alias.tipe of
                        Elm.Type.Record _ Nothing ->
                            True

                        _ ->
                            False

                Nothing ->
                    False

        Nothing ->
            False


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
