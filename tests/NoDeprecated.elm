module NoDeprecated exposing
    ( rule
    , Configuration, defaults, dependencies, withExceptionsForElements
    )

{-|

@docs rule

This rule is useful to stop the spread of the usage of deprecated values and types.

This rule is recommended to be used with `elm-review`'s suppression system (see `elm-review suppress --help`).
That way, current uses of deprecated elements won't be reported, but the rule will report new usages, in practice
allowing you to stop the bleed.

An additional benefit is that the suppressed errors will make it easy to have an overview of the number of times
deprecated elements are used and where they are located. Looking at the error reports (using `elm-review --unsuppress`
for instance) will give you the more precise problems and locations.


## Recommendations

I recommend making it extra explicit when deprecating elements in your application code, for instance by renaming
deprecated elements to include "deprecated" in their name, or in their module name for modules.

That way, it will be very explicit for you and your teammates when you're using something that is deprecated, even in
Git diffs.

For packages, renaming something is a breaking chance so that is not a viable option (if it is, remove the function and
release a new major version). Instead, what you can do is to start a line in your module/value/type's documentation
with `@deprecated`. There is no official nor conventional approach around deprecation in the Elm community, but this may
be a good start. But definitely pitch in the discussion around making a standard!
(I'll put a link here soon. If I haven't, please remind me!)

For both application and packages, when you deprecate something, I highly recommend documenting (in the most appropriate
location) why it is deprecate but especially what alternatives should be used or explored. It can be frustrating to
learn that something is deprecated without an explanation or any guidance what to use instead.

@docs Configuration, defaults, dependencies, withExceptionsForElements


## Fail

    import DeprecatedModule

    a =
        DeprecatedModule.view "..."

    b =
        Button.view_DEPRECATED "Click me!" OnClick


## When (not) to enable this rule

If you do not have deprecated elements in your project, this rule won't be useful.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoDeprecated
```

-}

{-
   TODO Report when using deprecated module aliases
   TODO Add an exception for the rule itself
   TODO Report when using exceptions that could not be found
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports usages of deprecated values and types.

    config =
        [ NoDeprecated.rule NoDeprecated.defaults
        ]

-}
rule : Configuration -> Rule
rule configuration =
    case createElementPredicate configuration of
        Ok elementPredicate ->
            let
                stableConfiguration : StableConfiguration
                stableConfiguration =
                    userConfigurationToStableConfiguration configuration elementPredicate
            in
            Rule.newProjectRuleSchema "NoDeprecated" initialProjectContext
                |> Rule.withDependenciesProjectVisitor (dependenciesVisitor stableConfiguration)
                |> Rule.withModuleVisitor (moduleVisitor stableConfiguration)
                |> Rule.withModuleContextUsingContextCreator
                    { fromProjectToModule = fromProjectToModule stableConfiguration
                    , fromModuleToProject = fromModuleToProject
                    , foldProjectContexts = foldProjectContexts
                    }
                |> Rule.withContextFromImportedModules
                |> Rule.fromProjectRuleSchema

        Err faultyNames ->
            Rule.configurationError "NoDeprecated"
                { message = "Invalid exceptions provided in the configuration"
                , details =
                    [ "The names provided to the withExceptionsForElements function should look like 'Some.Module.value' or 'MyModule.Type', which wasn't the case for the following types:"
                    , faultyNames
                        |> List.map (\str -> " - " ++ str)
                        |> String.join "\n"
                    ]
                }


initialProjectContext : ProjectContext
initialProjectContext =
    { deprecatedModules = []
    , deprecatedElements = []
    }


type alias ProjectContext =
    { deprecatedModules : List ( ModuleName, DeprecationReason )
    , deprecatedElements : List ( ModuleName, String )
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , currentModuleName : ModuleName
    , deprecatedModules : Dict ModuleName DeprecationReason
    , deprecatedElements : Set ( ModuleName, String )
    , isModuleDeprecated : Bool
    , localDeprecatedElements : List ( ModuleName, String )
    }


type DeprecationReason
    = DeprecatedModule
    | DeprecatedDependency


fromProjectToModule : StableConfiguration -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule (StableConfiguration configuration) =
    Rule.initContextCreator
        (\metadata lookupTable projectContext ->
            let
                moduleName : ModuleName
                moduleName =
                    Rule.moduleNameFromMetadata metadata
            in
            { lookupTable = lookupTable
            , currentModuleName = moduleName
            , deprecatedModules = Dict.fromList projectContext.deprecatedModules
            , deprecatedElements = Set.fromList projectContext.deprecatedElements
            , isModuleDeprecated = configuration.moduleNamePredicate moduleName
            , localDeprecatedElements = []
            }
        )
        |> Rule.withMetadata
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { deprecatedModules =
                if moduleContext.isModuleDeprecated then
                    [ ( Rule.moduleNameFromMetadata metadata, DeprecatedModule ) ]

                else
                    []
            , deprecatedElements = moduleContext.localDeprecatedElements
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { deprecatedModules = newContext.deprecatedModules ++ previousContext.deprecatedModules
    , deprecatedElements = newContext.deprecatedElements ++ previousContext.deprecatedElements
    }


moduleVisitor : StableConfiguration -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor configuration schema =
    schema
        |> Rule.withCommentsVisitor (\comments context -> ( [], commentsVisitor configuration comments context ))
        |> Rule.withDeclarationListVisitor (\nodes context -> ( [], declarationListVisitor configuration nodes context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( declarationVisitor configuration node context, context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( expressionVisitor configuration node context, context ))


{-| Configuration for the rule.

Create one using [`defaults`](#defaults), then change it using functions like [`dependencies`](#dependencies) and
[`withExceptionsForElements`](#withExceptionsForElements).

-}
type Configuration
    = Configuration
        { moduleNamePredicate : ModuleName -> Bool
        , documentationPredicate : String -> Bool
        , elementPredicate : ModuleName -> String -> Bool
        , exceptionsForElements : List String
        , recordFieldPredicate : String -> Bool
        , parameterPredicate : String -> Bool
        , deprecatedDependencies : List String
        }


type StableConfiguration
    = StableConfiguration
        { moduleNamePredicate : ModuleName -> Bool
        , documentationPredicate : String -> Bool
        , elementPredicate : ModuleName -> String -> Bool
        , recordFieldPredicate : String -> Bool
        , parameterPredicate : String -> Bool
        , deprecatedDependencies : List String
        }


userConfigurationToStableConfiguration : Configuration -> (ModuleName -> String -> Bool) -> StableConfiguration
userConfigurationToStableConfiguration (Configuration configuration) elementPredicate =
    StableConfiguration
        { moduleNamePredicate = configuration.moduleNamePredicate
        , documentationPredicate = configuration.documentationPredicate
        , elementPredicate = elementPredicate
        , recordFieldPredicate = configuration.recordFieldPredicate
        , parameterPredicate = configuration.parameterPredicate
        , deprecatedDependencies = configuration.deprecatedDependencies
        }


createElementPredicate : Configuration -> Result (List String) (ModuleName -> String -> Bool)
createElementPredicate (Configuration configuration) =
    if List.isEmpty configuration.exceptionsForElements then
        Ok
            (\moduleName name ->
                configuration.elementPredicate moduleName name
            )

    else
        case parseNames configuration.exceptionsForElements of
            Ok exceptionsForElements ->
                Ok
                    (\moduleName name ->
                        configuration.elementPredicate moduleName name
                            && not (Set.member ( moduleName, name ) exceptionsForElements)
                    )

            Err faultyNames ->
                Err faultyNames


parseNames : List String -> Result (List String) (Set ( ModuleName, String ))
parseNames strings =
    let
        parsedNames : List (Result String ( ModuleName, String ))
        parsedNames =
            List.map isValidName strings

        invalidNames : List String
        invalidNames =
            List.filterMap
                (\result ->
                    case result of
                        Err typeName ->
                            Just typeName

                        Ok _ ->
                            Nothing
                )
                parsedNames
    in
    if List.isEmpty invalidNames then
        parsedNames
            |> List.filterMap Result.toMaybe
            |> Set.fromList
            |> Ok

    else
        Err invalidNames


isValidName : String -> Result String ( ModuleName, String )
isValidName name =
    case List.reverse <| String.split "." name of
        functionName :: moduleName :: restOfModuleName ->
            Ok ( List.reverse (moduleName :: restOfModuleName), functionName )

        _ ->
            Err name


{-| Default configuration.

By default are considered as deprecated:

  - Values / types / modules that contain "deprecated" (case insensitive) in their name.
  - Values / types / modules whose documentation comment has a line starting with "@deprecated" or (for better visibility) "\*\*@deprecated"
  - Values / types from modules that are considered as deprecated

Configure this further using functions like [`dependencies`](#dependencies) and
[`withExceptionsForElements`](#withExceptionsForElements).

-}
defaults : Configuration
defaults =
    let
        containsDeprecated : String -> Bool
        containsDeprecated name =
            name
                |> String.toLower
                |> String.contains "deprecated"

        documentationPredicate : String -> Bool
        documentationPredicate doc =
            doc
                |> String.dropLeft 3
                |> String.lines
                |> List.any
                    (\rawLine ->
                        let
                            line : String
                            line =
                                String.trimLeft rawLine
                        in
                        String.startsWith "@deprecated" line
                            || String.startsWith "**@deprecated" line
                    )
    in
    Configuration
        { moduleNamePredicate = \moduleName -> containsDeprecated (String.join "." moduleName)
        , documentationPredicate = documentationPredicate
        , elementPredicate = \_ name -> containsDeprecated name
        , exceptionsForElements = []
        , recordFieldPredicate = containsDeprecated
        , parameterPredicate = containsDeprecated
        , deprecatedDependencies = []
        }


{-| Mark one or more dependencies as deprecated.

    config =
        [ NoDeprecated.defaults
            |> NoDeprecated.dependencies [ "jfmengels/some-deprecated-dependency" ]
            |> NoDeprecated.rule
        ]

Every usage of something defined in that dependency in the project's code wil be reported.

-}
dependencies : List String -> Configuration -> Configuration
dependencies dependencyNames (Configuration configuration) =
    Configuration { configuration | deprecatedDependencies = configuration.deprecatedDependencies ++ dependencyNames }


{-| Add exceptions for the reporting elements. This can for instance be used for values and that
contain "deprecated" in their name without actually being deprecated.

    config =
        [ NoDeprecated.defaults
            |> NoDeprecated.withExceptionsForElements [ "SomeModule.listOfDeprecatedFunctions" ]
            |> NoDeprecated.rule
        ]

-}
withExceptionsForElements : List String -> Configuration -> Configuration
withExceptionsForElements exceptionsForElements (Configuration configuration) =
    Configuration { configuration | exceptionsForElements = exceptionsForElements ++ configuration.exceptionsForElements }


dependenciesVisitor : StableConfiguration -> Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ( List (Rule.Error global), ProjectContext )
dependenciesVisitor (StableConfiguration configuration) dict projectContext =
    let
        newContext : ProjectContext
        newContext =
            Dict.foldl
                (\packageName dependency acc ->
                    let
                        modules : List Elm.Docs.Module
                        modules =
                            Review.Project.Dependency.modules dependency
                    in
                    if List.member packageName configuration.deprecatedDependencies then
                        { acc
                            | deprecatedModules =
                                List.map
                                    (\{ name } -> ( String.split "." name, DeprecatedDependency ))
                                    modules
                                    ++ acc.deprecatedModules
                        }

                    else
                        List.foldl
                            (registerDeprecatedThings (StableConfiguration configuration))
                            acc
                            modules
                )
                projectContext
                dict

        unknownDependenciesErrors : List (Rule.Error global)
        unknownDependenciesErrors =
            configuration.deprecatedDependencies
                |> List.filter (\name -> not (Dict.member name dict))
                |> List.map
                    (\name ->
                        Rule.globalError
                            { message = "Could not find package `" ++ name ++ "`"
                            , details =
                                [ "You marked this package as deprecated, but I can't find it in your dependencies."
                                , "It could be a typo, or maybe you've successfully removed it from your project?"
                                ]
                            }
                    )
    in
    ( unknownDependenciesErrors, newContext )


registerDeprecatedThings : StableConfiguration -> Elm.Docs.Module -> ProjectContext -> ProjectContext
registerDeprecatedThings (StableConfiguration configuration) module_ acc =
    let
        moduleName : ModuleName
        moduleName =
            String.split "." module_.name
    in
    if configuration.documentationPredicate module_.comment then
        { deprecatedModules = ( moduleName, DeprecatedModule ) :: acc.deprecatedModules
        , deprecatedElements = acc.deprecatedElements
        }

    else
        let
            commentIndicatesDeprecation : { a | comment : String } -> Bool
            commentIndicatesDeprecation { comment } =
                configuration.documentationPredicate comment

            deprecatedAliases : List Elm.Docs.Alias
            deprecatedAliases =
                module_.aliases
                    |> List.filter commentIndicatesDeprecation

            deprecatedUnions : List Elm.Docs.Union
            deprecatedUnions =
                module_.unions
                    |> List.filter commentIndicatesDeprecation

            newValues : List ( ModuleName, String )
            newValues =
                List.concat
                    [ module_.values
                        |> List.filter commentIndicatesDeprecation
                        |> List.map (\value -> ( moduleName, value.name ))
                    , deprecatedUnions
                        |> List.map (\{ name } -> ( moduleName, name ))
                    , deprecatedUnions
                        |> List.concatMap .tags
                        |> List.map (\( name, _ ) -> ( moduleName, name ))
                    , deprecatedAliases
                        |> List.map (\{ name } -> ( moduleName, name ))
                    ]
        in
        { deprecatedModules = acc.deprecatedModules
        , deprecatedElements = newValues ++ acc.deprecatedElements
        }


commentsVisitor : StableConfiguration -> List (Node String) -> ModuleContext -> ModuleContext
commentsVisitor (StableConfiguration configuration) comments moduleContext =
    if moduleContext.isModuleDeprecated then
        moduleContext

    else
        case find (\(Node _ comment) -> String.startsWith "{-|" comment) comments of
            Just (Node _ moduleDocumentation) ->
                { moduleContext | isModuleDeprecated = configuration.documentationPredicate moduleDocumentation }

            Nothing ->
                moduleContext


declarationListVisitor : StableConfiguration -> List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor configuration nodes context =
    if context.isModuleDeprecated then
        context

    else
        List.foldl (registerDeclaration configuration) context nodes


registerDeclaration : StableConfiguration -> Node Declaration -> ModuleContext -> ModuleContext
registerDeclaration configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            registerFunctionDeclaration configuration declaration context

        Declaration.AliasDeclaration type_ ->
            registerAliasDeclaration configuration type_ context

        Declaration.CustomTypeDeclaration type_ ->
            registerCustomTypeDeclaration configuration type_ context

        _ ->
            context


registerFunctionDeclaration : StableConfiguration -> Expression.Function -> ModuleContext -> ModuleContext
registerFunctionDeclaration (StableConfiguration configuration) declaration context =
    let
        name : String
        name =
            declaration.declaration |> Node.value |> .name |> Node.value
    in
    if
        configuration.elementPredicate context.currentModuleName name
            || checkDocumentation configuration.documentationPredicate declaration.documentation
    then
        registerElement name context

    else
        context


registerAliasDeclaration : StableConfiguration -> Elm.Syntax.TypeAlias.TypeAlias -> ModuleContext -> ModuleContext
registerAliasDeclaration (StableConfiguration configuration) type_ context =
    let
        name : String
        name =
            Node.value type_.name
    in
    if
        configuration.elementPredicate context.currentModuleName name
            || checkDocumentation configuration.documentationPredicate type_.documentation
    then
        registerElement name context

    else
        context


registerCustomTypeDeclaration : StableConfiguration -> Elm.Syntax.Type.Type -> ModuleContext -> ModuleContext
registerCustomTypeDeclaration (StableConfiguration configuration) type_ context =
    let
        name : String
        name =
            Node.value type_.name

        register : ModuleContext -> ModuleContext
        register ctx =
            List.foldl
                (\(Node _ constructor) -> registerElement (Node.value constructor.name))
                (registerElement name ctx)
                type_.constructors
    in
    if
        configuration.elementPredicate context.currentModuleName name
            || checkDocumentation configuration.documentationPredicate type_.documentation
    then
        register context

    else
        List.foldl
            (\(Node _ constructor) ctx ->
                if configuration.elementPredicate ctx.currentModuleName (Node.value constructor.name) then
                    registerElement (Node.value constructor.name) ctx

                else
                    ctx
            )
            context
            type_.constructors


checkDocumentation : (String -> Bool) -> Maybe (Node String) -> Bool
checkDocumentation documentationPredicate documentationNode =
    case documentationNode of
        Just (Node _ str) ->
            documentationPredicate str

        Nothing ->
            False


registerElement : String -> ModuleContext -> ModuleContext
registerElement name context =
    { context
        | deprecatedElements = Set.insert ( [], name ) context.deprecatedElements
        , localDeprecatedElements = ( context.currentModuleName, name ) :: context.localDeprecatedElements
    }


declarationVisitor : StableConfiguration -> Node Declaration -> ModuleContext -> List (Rule.Error {})
declarationVisitor configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            let
                signatureErrors : List (Rule.Error {})
                signatureErrors =
                    case declaration.signature of
                        Just signature ->
                            reportTypes
                                context
                                [ (Node.value signature).typeAnnotation ]
                                []

                        Nothing ->
                            []

                destructuringErrors : List (Rule.Error {})
                destructuringErrors =
                    reportPatterns
                        configuration
                        context
                        (declaration.declaration |> Node.value |> .arguments)
                        []
            in
            destructuringErrors ++ signatureErrors

        Declaration.CustomTypeDeclaration type_ ->
            reportTypes
                context
                (List.concatMap (\(Node _ { arguments }) -> arguments) type_.constructors)
                []

        Declaration.AliasDeclaration type_ ->
            reportTypes
                context
                [ type_.typeAnnotation ]
                []

        Declaration.PortDeclaration signature ->
            reportTypes
                context
                [ signature.typeAnnotation ]
                []

        _ ->
            []


reportLetDeclaration : StableConfiguration -> ModuleContext -> Node Expression.LetDeclaration -> List (Rule.Error {})
reportLetDeclaration configuration context letDeclaration =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            let
                signatureErrors : List (Rule.Error {})
                signatureErrors =
                    case function.signature of
                        Just signature ->
                            reportTypes
                                context
                                [ (Node.value signature).typeAnnotation ]
                                []

                        Nothing ->
                            []

                destructuringErrors : List (Rule.Error {})
                destructuringErrors =
                    reportPatterns
                        configuration
                        context
                        (function.declaration |> Node.value |> .arguments)
                        []
            in
            destructuringErrors ++ signatureErrors

        Expression.LetDestructuring pattern _ ->
            reportPatterns
                configuration
                context
                [ pattern ]
                []


reportTypes : ModuleContext -> List (Node TypeAnnotation) -> List (Rule.Error {}) -> List (Rule.Error {})
reportTypes context nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.Typed (Node range ( _, name )) args ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportElementAsMaybe context range name of
                                Just err ->
                                    err :: acc

                                Nothing ->
                                    acc
                    in
                    reportTypes
                        context
                        (args ++ restOfNodes)
                        newAcc

                TypeAnnotation.Tupled nodesToLookAt ->
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.Record recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (\(Node _ ( _, fieldValue )) -> fieldValue) recordDefinition
                    in
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.GenericRecord _ recordDefinition ->
                    let
                        nodesToLookAt : List (Node TypeAnnotation)
                        nodesToLookAt =
                            List.map (\(Node _ ( _, fieldValue )) -> fieldValue) (Node.value recordDefinition)
                    in
                    reportTypes context (nodesToLookAt ++ restOfNodes) acc

                TypeAnnotation.FunctionTypeAnnotation left right ->
                    reportTypes context (left :: right :: restOfNodes) acc

                _ ->
                    reportTypes context restOfNodes acc


reportPatterns : StableConfiguration -> ModuleContext -> List (Node Pattern) -> List (Rule.Error {}) -> List (Rule.Error {})
reportPatterns configuration context nodes acc =
    case nodes of
        [] ->
            acc

        pattern :: restOfNodes ->
            case Node.value pattern of
                Pattern.ParenthesizedPattern subPattern ->
                    reportPatterns
                        configuration
                        context
                        (subPattern :: restOfNodes)
                        acc

                Pattern.TuplePattern subPatterns ->
                    reportPatterns configuration context (subPatterns ++ restOfNodes) acc

                Pattern.RecordPattern fields ->
                    reportPatterns configuration
                        context
                        restOfNodes
                        (List.filterMap (reportField configuration) fields ++ acc)

                Pattern.UnConsPattern left right ->
                    reportPatterns configuration context (left :: right :: restOfNodes) acc

                Pattern.ListPattern subPatterns ->
                    reportPatterns configuration context (subPatterns ++ restOfNodes) acc

                Pattern.VarPattern name ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportParameter configuration (Node.range pattern) name of
                                Just err ->
                                    err :: acc

                                Nothing ->
                                    acc
                    in
                    reportPatterns
                        configuration
                        context
                        restOfNodes
                        newAcc

                Pattern.NamedPattern qualifiedNameRef subPatterns ->
                    let
                        errors : List (Rule.Error {})
                        errors =
                            reportElementAsList
                                context
                                (Node.range pattern)
                                (\() -> rangeForNamedPattern pattern qualifiedNameRef)
                                qualifiedNameRef.name
                    in
                    reportPatterns
                        configuration
                        context
                        (subPatterns ++ restOfNodes)
                        (errors ++ acc)

                Pattern.AsPattern subPattern name ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case reportParameter configuration (Node.range name) (Node.value name) of
                                Just err ->
                                    err :: acc

                                Nothing ->
                                    acc
                    in
                    reportPatterns configuration context (subPattern :: restOfNodes) newAcc

                _ ->
                    reportPatterns configuration context restOfNodes acc


rangeForNamedPattern : Node a -> Pattern.QualifiedNameRef -> Range
rangeForNamedPattern (Node parentRange _) { moduleName, name } =
    let
        lengthForName : Int
        lengthForName =
            if List.isEmpty moduleName then
                String.length name

            else
                (String.join "." moduleName ++ "." ++ name)
                    |> String.length

        patternStart : Range.Location
        patternStart =
            parentRange.start
    in
    { start = patternStart
    , end = { row = patternStart.row, column = patternStart.column + lengthForName }
    }


reportField : StableConfiguration -> Node String -> Maybe (Rule.Error {})
reportField (StableConfiguration configuration) field =
    if configuration.recordFieldPredicate (Node.value field) then
        Just (error Field (Node.range field))

    else
        Nothing


expressionVisitor : StableConfiguration -> Node Expression -> ModuleContext -> List (Rule.Error {})
expressionVisitor configuration (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            reportElementAsList
                context
                nodeRange
                (always nodeRange)
                name

        Expression.LetExpression letBlock ->
            List.concatMap
                (reportLetDeclaration configuration context)
                letBlock.declarations

        Expression.CaseExpression { cases } ->
            reportPatterns
                configuration
                context
                (List.map Tuple.first cases)
                []

        Expression.RecordUpdateExpression (Node range name) _ ->
            reportElementAsList
                context
                range
                (always range)
                name

        Expression.RecordAccess _ field ->
            case reportField configuration field of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        Expression.RecordAccessFunction fieldName ->
            case reportField configuration (Node nodeRange fieldName) of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        _ ->
            []


reportElementAsList : ModuleContext -> Range -> (() -> Range) -> String -> List (Rule.Error {})
reportElementAsList context rangeForLookupTable rangeForReport name =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable rangeForLookupTable of
        Just moduleName ->
            case Dict.get moduleName context.deprecatedModules of
                Just DeprecatedModule ->
                    [ error Module (rangeForReport ()) ]

                Just DeprecatedDependency ->
                    [ error Dependency (rangeForReport ()) ]

                Nothing ->
                    if Set.member ( moduleName, name ) context.deprecatedElements then
                        [ error Element (rangeForReport ()) ]

                    else
                        []

        Nothing ->
            []


reportElementAsMaybe : ModuleContext -> Range -> String -> Maybe (Rule.Error {})
reportElementAsMaybe context range name =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
        Just moduleName ->
            case Dict.get moduleName context.deprecatedModules of
                Just DeprecatedModule ->
                    Just (error Module range)

                Just DeprecatedDependency ->
                    Just (error Dependency range)

                Nothing ->
                    if Set.member ( moduleName, name ) context.deprecatedElements then
                        Just (error Element range)

                    else
                        Nothing

        Nothing ->
            Nothing


reportParameter : StableConfiguration -> Range -> String -> Maybe (Rule.Error {})
reportParameter (StableConfiguration configuration) range name =
    if configuration.parameterPredicate name then
        Just (error Parameter range)

    else
        Nothing


type Origin
    = Element
    | Module
    | Dependency
    | Field
    | Parameter


error : Origin -> Range -> Rule.Error {}
error origin range =
    let
        details : List String
        details =
            case origin of
                Element ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    , "Please check its documentation to know the alternative solutions."
                    ]

                Module ->
                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                    , "Please check its documentation to know the alternative solutions."
                    ]

                Dependency ->
                    [ "The dependency where this element is defined was marked as deprecated and should not be used anymore."
                    , "Please check its documentation or your review configuration to know the alternative solutions."
                    ]

                Field ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    , "Please check its documentation to know the alternative solutions."
                    ]

                Parameter ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    ]
    in
    Rule.error
        { message = "Found new usage of deprecated element"
        , details = details
        }
        range


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ] == Just 6

-}
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
