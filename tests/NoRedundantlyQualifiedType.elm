module NoRedundantlyQualifiedType exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Function, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (RecordField, TypeAnnotation(..))
import Review.Fix as Fix exposing (Fix)
import Review.Resources.ImportBinding as ImportBinding
import Review.Resources.ScopeProvider as ScopeProvider exposing (ScopeProvider)
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoRedundantlyQualifiedType" initialContext
        |> Rule.withDeclarationEnterVisitor (\node context -> ( declarationVisitor node context, context ))
        |> Rule.withLetDeclarationEnterVisitor (\_ node context -> ( letDeclarationEnterVisitor node context, context ))
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopeProvider : ScopeProvider
    , imports : List (Node Import)
    , typesDefinedInModule : Set String
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\scopeProvider ast () ->
            { scopeProvider = scopeProvider
            , imports = ast.imports
            , typesDefinedInModule = collectTypesDefinedInModule ast.declarations
            }
        )
        |> Rule.withScopes
        |> Rule.withFullAst


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
            if qualifier /= name || Set.member name context.typesDefinedInModule then
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
            [ reportError name range (importFix moduleName name context.imports) ]

        Just binding ->
            if moduleName == ImportBinding.moduleName binding then
                [ reportError name range [] ]

            else
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
