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


## Fail

    import DeprecatedModule

    a =
        DeprecatedModule.view "..."

    b =
        Button.view_DEPRECATED "Click me!" OnClick


## Tagging recommendations

I recommend making it extra explicit when deprecating elements in your application code, for instance by renaming
them to include "deprecated" in their name, or in their module name for modules. That way, it will be very clear for you and your teammates when you're using something that is deprecated, even in
Git diffs.

I recommend also including `@deprecated` in the deprecated element's documentation, as that will both consider the
element as deprecated, and allow this rule to pick up the reasons and recommendations and present them in the error's details.
This is the recommended way to deprecate something from an Elm package.

If you include a single `@deprecated` at the beginning of a line (potentially between `*` for Markdown styling),
then this rule will pick the text until the end of a line.

    {-| Does X.

    **@deprecated** Not performant. Use Y instead.

    Bla bla.

    -}
    value =
        1

Here, "Not performant. Use Y instead." will be picked up.

Alternatively, you can have start and end `@deprecated` tags, and anything between two will be presented to the user.

    {-| Does X.

    **@deprecated** Not performant. Use Y instead.

    Bla bla.

    **/@deprecated**

    -}
    value =
        1

Here, "Not performant. Use Y instead." and "Bla bla" will be picked up. (the `/` before the `@` is optional).

There is no official nor conventional approach around deprecation in the Elm community, but this may
be a good start. But definitely pitch in the discussion around making a standard!
(I'll put a link here soon. If I haven't, please remind me!)

For both application and packages, when you deprecate something, I highly recommend documenting (in the most appropriate
location) **why it is deprecated** but especially **what alternatives should be used** or explored. It can be frustrating to
learn that something is deprecated without an explanation or any guidance on what to use instead.

It is absolutely fine to suppress **more** things. While it's normal to want to suppress as few problems as possible,
identifying technical debt is important, and that is exactly what this rule can help with. Your efforts on reducing the
number of deprecated usages should not be a hard goal, and it should be done in parallel of identifying technical debt.


## Tackling reported issues

As mentioned before, this rule is recommended to be used with `elm-review`'s suppression system. One of its benefits,
is that the number of usages will be tallied per file in the `<review>/suppressed/NoDeprecated.json` file. You can look
at it, and decide to tackle one file over another based on how many problems are in the file and how you prefer tackling
these issues.

While tackling issues file by file might work for some cases, sometimes it is nicer to organize work based on how often
a deprecated function/type is used. For instance, you might want to look at the functions that are used only once or twice,
or look at the functions that are the most widespread in your codebase.

To get this point of view, you can run this rule as an insight rule:

```bash
elm-review --report=json --extract --rules NoDeprecated | jq -r '.extracts.NoDeprecated'
```

which will yield a result like the following:

```json
{
  "Some.Deprecated.Module": {
    "total": 28,
    "isModuleDeprecated": true,
    "usages": {
      "someFunction": 20,
      "someType": 8
    }
  },
  "Some.Module": {
    "total": 1,
    "isModuleDeprecated": false,
    "usages": {
      "someDeprecatedFunction": 1
    }
  }
}
```


## Configure

@docs Configuration, defaults, dependencies, withExceptionsForElements


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
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Encode as Encode
import Regex exposing (Regex)
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
rule (Configuration { exceptionsForElements, deprecatedDependencies }) =
    case parseExceptions exceptionsForElements of
        Ok exceptions ->
            Rule.newProjectRuleSchema "NoDeprecated" initialProjectContext
                |> Rule.withDirectDependenciesProjectVisitor (dependenciesVisitor deprecatedDependencies)
                |> Rule.withModuleVisitor (moduleVisitor exceptions)
                |> Rule.withModuleContextUsingContextCreator
                    { fromProjectToModule = fromProjectToModule
                    , fromModuleToProject = fromModuleToProject
                    , foldProjectContexts = foldProjectContexts
                    }
                |> Rule.withContextFromImportedModules
                |> Rule.withDataExtractor dataExtractor
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


type alias ProjectContext =
    { deprecatedModules : Dict ModuleName DeprecationReason
    , deprecatedElements : Dict ( ModuleName, String ) (Maybe String)
    , usages : Dict ( ModuleName, String ) Int
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { deprecatedModules = Dict.empty
    , deprecatedElements = Dict.empty
    , usages = Dict.empty
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , currentModuleName : ModuleName
    , deprecatedModules : Dict ModuleName DeprecationReason
    , deprecatedElements : Dict ( ModuleName, String ) (Maybe String)
    , isModuleDeprecated : Status
    , localDeprecatedElements : Dict ( ModuleName, String ) (Maybe String)
    , usages : List DeprecatedElementUsage
    }


type DeprecationReason
    = DeprecatedModule (Maybe String)
    | DeprecatedDependency ()


deprecatedDependency : DeprecationReason
deprecatedDependency =
    DeprecatedDependency ()


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\moduleName lookupTable projectContext ->
            { lookupTable = lookupTable
            , currentModuleName = moduleName
            , deprecatedModules = projectContext.deprecatedModules
            , deprecatedElements = projectContext.deprecatedElements
            , isModuleDeprecated = moduleNamePredicate moduleName
            , localDeprecatedElements = Dict.empty
            , usages = []
            }
        )
        |> Rule.withModuleName
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\currentModuleName moduleContext ->
            { deprecatedModules =
                case moduleContext.isModuleDeprecated of
                    Deprecated message ->
                        Dict.singleton currentModuleName (DeprecatedModule message)

                    NotDeprecated () ->
                        Dict.empty
            , deprecatedElements = moduleContext.localDeprecatedElements
            , usages =
                List.foldl
                    (\{ moduleName, name } acc ->
                        let
                            key : ( ModuleName, String )
                            key =
                                ( moduleName, name )

                            count : Int
                            count =
                                Dict.get key acc |> Maybe.withDefault 0
                        in
                        Dict.insert key (count + 1) acc
                    )
                    Dict.empty
                    moduleContext.usages
            }
        )
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { deprecatedModules = Dict.union newContext.deprecatedModules previousContext.deprecatedModules
    , deprecatedElements = Dict.union newContext.deprecatedElements previousContext.deprecatedElements
    , usages =
        Dict.foldl
            (\key countForNew acc ->
                let
                    count : Int
                    count =
                        Dict.get key previousContext.usages |> Maybe.withDefault 0
                in
                Dict.insert key (countForNew + count) acc
            )
            previousContext.usages
            newContext.usages
    }


moduleVisitor : Exceptions -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor configuration schema =
    schema
        |> Rule.withModuleDocumentationVisitor (\moduleDocumentation context -> ( [], moduleDocumentationVisitor moduleDocumentation context ))
        |> Rule.withDeclarationListVisitor (\nodes context -> ( [], declarationListVisitor configuration nodes context ))
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor


{-| Configuration for the rule.

Create one using [`defaults`](#defaults), then change it using functions like [`dependencies`](#dependencies) and
[`withExceptionsForElements`](#withExceptionsForElements).

-}
type Configuration
    = Configuration
        { exceptionsForElements : List String
        , deprecatedDependencies : List String
        }


type Exceptions
    = Exceptions (Set ( ModuleName, String ))


type Status
    = Deprecated (Maybe String)
    | NotDeprecated ()


deprecatedPlain : Status
deprecatedPlain =
    Deprecated Nothing


notDeprecated : Status
notDeprecated =
    NotDeprecated ()


orElse : (() -> Status) -> Status -> Status
orElse fn status =
    case status of
        Deprecated _ ->
            status

        NotDeprecated () ->
            fn ()


parseExceptions : List String -> Result (List String) Exceptions
parseExceptions exceptionsForElements =
    let
        parsedNames : List (Result String ( ModuleName, String ))
        parsedNames =
            List.map isValidName exceptionsForElements

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
            |> Exceptions
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

  - Values / parameters / types / modules that contain "deprecated" (case insensitive) in their name.
  - Values / types / modules whose documentation comment has a line starting with "@deprecated" or (for better visibility) "\*\*@deprecated"
  - Values / types from modules that are considered as deprecated

Configure this further using functions like [`dependencies`](#dependencies) and
[`withExceptionsForElements`](#withExceptionsForElements).

-}
defaults : Configuration
defaults =
    Configuration
        { exceptionsForElements = []
        , deprecatedDependencies = []
        }


containsDeprecated : String -> Status
containsDeprecated name =
    if String.contains "deprecated" (String.toLower name) then
        deprecatedPlain

    else
        notDeprecated


moduleNamePredicate : List String -> Status
moduleNamePredicate moduleName =
    containsDeprecated (String.join "." moduleName)


elementPredicate : Exceptions -> ModuleName -> String -> Status
elementPredicate (Exceptions exceptions) moduleName name =
    if
        String.contains "deprecated" (String.toLower name)
            && not (Set.member ( moduleName, name ) exceptions)
    then
        deprecatedPlain

    else
        notDeprecated


documentationPredicate : String -> Status
documentationPredicate doc =
    case Regex.findAtMost 1 deprecationStartRegex (String.slice 3 -3 doc) of
        match :: _ ->
            let
                offset : Int
                offset =
                    match.index + String.length match.match + 3
            in
            case Regex.findAtMost 1 deprecationEndRegex (String.slice offset -3 doc) of
                endMatch :: _ ->
                    doc
                        |> String.slice offset (offset + endMatch.index)
                        |> Just
                        |> Deprecated

                _ ->
                    case match.submatches of
                        _ :: subMatch :: _ ->
                            Deprecated subMatch

                        _ ->
                            deprecatedPlain

        [] ->
            if
                doc
                    |> String.slice 3 -3
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
            then
                deprecatedPlain

            else
                notDeprecated


deprecationStartRegex : Regex
deprecationStartRegex =
    "^(\\s*\\**@deprecated\\**)(.*)$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = True }
        |> Maybe.withDefault Regex.never


deprecationEndRegex : Regex
deprecationEndRegex =
    "@deprecated"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


trimEndRegex : Regex
trimEndRegex =
    "(\\s|[/*])*$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


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
    Configuration { configuration | exceptionsForElements = configuration.exceptionsForElements ++ exceptionsForElements }


type alias DeprecatedElementUsage =
    { moduleName : ModuleName
    , name : String
    , origin : Origin
    , range : Range
    , deprecationMessage : Maybe String
    }


dependenciesVisitor : List String -> Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ( List (Rule.Error global), ProjectContext )
dependenciesVisitor deprecatedDependencies dict projectContext =
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
                    if List.member packageName deprecatedDependencies then
                        { acc
                            | deprecatedModules =
                                List.foldl
                                    (\{ name } subAcc -> Dict.insert (String.split "." name) deprecatedDependency subAcc)
                                    acc.deprecatedModules
                                    modules
                        }

                    else
                        List.foldl registerDeprecatedThings acc modules
                )
                projectContext
                dict

        unknownDependenciesErrors : List (Rule.Error global)
        unknownDependenciesErrors =
            deprecatedDependencies
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


registerDeprecatedThings : Elm.Docs.Module -> ProjectContext -> ProjectContext
registerDeprecatedThings module_ context =
    let
        moduleName : ModuleName
        moduleName =
            String.split "." module_.name
    in
    case documentationPredicate module_.comment of
        Deprecated message ->
            { deprecatedModules = Dict.insert moduleName (DeprecatedModule message) context.deprecatedModules
            , deprecatedElements = context.deprecatedElements
            , usages = context.usages
            }

        NotDeprecated () ->
            let
                addDeprecatedValues : Dict ( ModuleName, String ) (Maybe String) -> Dict ( ModuleName, String ) (Maybe String)
                addDeprecatedValues acc =
                    List.foldl
                        (\element subAcc ->
                            case documentationPredicate element.comment of
                                Deprecated message ->
                                    Dict.insert ( moduleName, element.name ) message subAcc

                                NotDeprecated () ->
                                    subAcc
                        )
                        acc
                        module_.values

                addDeprecatedAliases : Dict ( ModuleName, String ) (Maybe String) -> Dict ( ModuleName, String ) (Maybe String)
                addDeprecatedAliases acc =
                    List.foldl
                        (\element subAcc ->
                            case documentationPredicate element.comment of
                                Deprecated message ->
                                    Dict.insert ( moduleName, element.name ) message subAcc

                                NotDeprecated () ->
                                    subAcc
                        )
                        acc
                        module_.aliases

                addDeprecatedUnions : Dict ( ModuleName, String ) (Maybe String) -> Dict ( ModuleName, String ) (Maybe String)
                addDeprecatedUnions acc =
                    List.foldl
                        (\element subAcc ->
                            case documentationPredicate element.comment of
                                Deprecated message ->
                                    List.foldl
                                        (\( name, _ ) subSubAcc ->
                                            Dict.insert ( moduleName, name ) message subSubAcc
                                        )
                                        (Dict.insert ( moduleName, element.name ) message subAcc)
                                        element.tags

                                NotDeprecated () ->
                                    subAcc
                        )
                        acc
                        module_.unions

                deprecatedElements : Dict ( ModuleName, String ) (Maybe String)
                deprecatedElements =
                    context.deprecatedElements
                        |> addDeprecatedValues
                        |> addDeprecatedAliases
                        |> addDeprecatedUnions
            in
            { deprecatedModules = context.deprecatedModules
            , deprecatedElements = deprecatedElements
            , usages = context.usages
            }


moduleDocumentationVisitor : Maybe (Node String) -> ModuleContext -> ModuleContext
moduleDocumentationVisitor maybeModuleDocumentation moduleContext =
    case maybeModuleDocumentation of
        Just (Node _ moduleDocumentation) ->
            { moduleContext
                | isModuleDeprecated =
                    documentationPredicate moduleDocumentation
                        |> orElse (\() -> moduleContext.isModuleDeprecated)
            }

        Nothing ->
            moduleContext


declarationListVisitor : Exceptions -> List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor configuration nodes context =
    List.foldl (registerDeclaration configuration) context nodes


registerDeclaration : Exceptions -> Node Declaration -> ModuleContext -> ModuleContext
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


registerFunctionDeclaration : Exceptions -> Expression.Function -> ModuleContext -> ModuleContext
registerFunctionDeclaration exceptions declaration context =
    let
        name : String
        name =
            declaration.declaration |> Node.value |> .name |> Node.value
    in
    registerElement
        (checkDocumentation declaration.documentation
            |> orElse (\() -> elementPredicate exceptions context.currentModuleName name)
        )
        name
        context


registerAliasDeclaration : Exceptions -> Elm.Syntax.TypeAlias.TypeAlias -> ModuleContext -> ModuleContext
registerAliasDeclaration exceptions type_ context =
    let
        name : String
        name =
            Node.value type_.name
    in
    registerElement
        (checkDocumentation type_.documentation
            |> orElse (\() -> elementPredicate exceptions context.currentModuleName name)
        )
        name
        context


registerCustomTypeDeclaration : Exceptions -> Elm.Syntax.Type.Type -> ModuleContext -> ModuleContext
registerCustomTypeDeclaration exceptions type_ context =
    let
        name : String
        name =
            Node.value type_.name
    in
    case
        checkDocumentation type_.documentation
            |> orElse (\() -> elementPredicate exceptions context.currentModuleName name)
    of
        (Deprecated _) as status ->
            List.foldl
                (\(Node _ constructor) -> registerElement status (Node.value constructor.name))
                (registerElement status name context)
                type_.constructors

        NotDeprecated () ->
            List.foldl
                (\(Node _ constructor) ctx ->
                    registerElement
                        (elementPredicate exceptions ctx.currentModuleName (Node.value constructor.name))
                        (Node.value constructor.name)
                        ctx
                )
                context
                type_.constructors


checkDocumentation : Maybe (Node String) -> Status
checkDocumentation documentationNode =
    case documentationNode of
        Just (Node _ str) ->
            documentationPredicate str

        Nothing ->
            notDeprecated


registerElement : Status -> String -> ModuleContext -> ModuleContext
registerElement status name context =
    case status of
        NotDeprecated () ->
            context

        Deprecated message ->
            let
                key : ( ModuleName, String )
                key =
                    ( context.currentModuleName, name )
            in
            { context
                | deprecatedElements = Dict.insert key message context.deprecatedElements
                , localDeprecatedElements = Dict.insert key message context.localDeprecatedElements
            }


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor node context =
    let
        usages : List DeprecatedElementUsage
        usages =
            declarationVisitorHelp node context
    in
    ( List.map error usages, { context | usages = usages ++ context.usages } )


declarationVisitorHelp : Node Declaration -> ModuleContext -> List DeprecatedElementUsage
declarationVisitorHelp node context =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            let
                signatureErrors : List DeprecatedElementUsage
                signatureErrors =
                    case declaration.signature of
                        Just (Node _ { typeAnnotation }) ->
                            reportTypes context [ typeAnnotation ] []

                        Nothing ->
                            []
            in
            reportPatterns
                context
                (Node.value declaration.declaration).arguments
                signatureErrors

        Declaration.CustomTypeDeclaration type_ ->
            reportTypes
                context
                (List.concatMap (\(Node _ { arguments }) -> arguments) type_.constructors)
                []

        Declaration.AliasDeclaration { typeAnnotation } ->
            reportTypes context [ typeAnnotation ] []

        Declaration.PortDeclaration { typeAnnotation } ->
            reportTypes context [ typeAnnotation ] []

        _ ->
            []


reportLetDeclarations : ModuleContext -> List (Node Expression.LetDeclaration) -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportLetDeclarations context letDeclarations acc =
    case letDeclarations of
        [] ->
            acc

        letDeclaration :: rest ->
            reportLetDeclarations
                context
                rest
                (reportLetDeclaration context letDeclaration acc)


reportLetDeclaration : ModuleContext -> Node Expression.LetDeclaration -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportLetDeclaration context (Node _ letDeclaration) acc =
    case letDeclaration of
        Expression.LetFunction function ->
            let
                signatureErrors : List DeprecatedElementUsage
                signatureErrors =
                    case function.signature of
                        Just signature ->
                            reportTypes
                                context
                                [ (Node.value signature).typeAnnotation ]
                                acc

                        Nothing ->
                            acc
            in
            reportPatterns
                context
                (function.declaration |> Node.value |> .arguments)
                signatureErrors

        Expression.LetDestructuring pattern _ ->
            reportPatterns
                context
                [ pattern ]
                acc


reportTypes : ModuleContext -> List (Node TypeAnnotation) -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportTypes context nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.Typed (Node range ( _, name )) args ->
                    reportTypes
                        context
                        (args ++ restOfNodes)
                        (maybeCons (reportElementAsMaybe context range name) acc)

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


reportPatterns : ModuleContext -> List (Node Pattern) -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportPatterns context nodes acc =
    case nodes of
        [] ->
            acc

        pattern :: restOfNodes ->
            case Node.value pattern of
                Pattern.ParenthesizedPattern subPattern ->
                    reportPatterns
                        context
                        (subPattern :: restOfNodes)
                        acc

                Pattern.TuplePattern subPatterns ->
                    reportPatterns context (subPatterns ++ restOfNodes) acc

                Pattern.RecordPattern fields ->
                    reportPatterns
                        context
                        restOfNodes
                        (List.filterMap (reportField context.lookupTable context.currentModuleName) fields ++ acc)

                Pattern.UnConsPattern left right ->
                    reportPatterns context (left :: right :: restOfNodes) acc

                Pattern.ListPattern subPatterns ->
                    reportPatterns context (subPatterns ++ restOfNodes) acc

                Pattern.VarPattern name ->
                    reportPatterns
                        context
                        restOfNodes
                        (maybeCons (reportParameter context.currentModuleName (Node.range pattern) name) acc)

                Pattern.NamedPattern qualifiedNameRef subPatterns ->
                    let
                        errors : List DeprecatedElementUsage
                        errors =
                            reportElementAsList
                                context
                                (Node.range pattern)
                                (\() -> rangeForNamedPattern pattern qualifiedNameRef)
                                qualifiedNameRef.name
                                acc
                    in
                    reportPatterns
                        context
                        (subPatterns ++ restOfNodes)
                        errors

                Pattern.AsPattern subPattern name ->
                    reportPatterns
                        context
                        (subPattern :: restOfNodes)
                        (maybeCons (reportParameter context.currentModuleName (Node.range name) (Node.value name)) acc)

                _ ->
                    reportPatterns context restOfNodes acc


rangeForNamedPattern : Node a -> Pattern.QualifiedNameRef -> Range
rangeForNamedPattern (Node { start } _) { moduleName, name } =
    let
        lengthForName : Int
        lengthForName =
            if List.isEmpty moduleName then
                String.length name

            else
                (String.join "." moduleName ++ "." ++ name)
                    |> String.length
    in
    { start = start
    , end = { row = start.row, column = start.column + lengthForName }
    }


reportField : ModuleNameLookupTable -> ModuleName -> Node String -> Maybe DeprecatedElementUsage
reportField lookupTable currentModuleName field =
    case containsDeprecated (Node.value field) of
        Deprecated message ->
            let
                moduleName : ModuleName
                moduleName =
                    ModuleNameLookupTable.fullModuleNameFor lookupTable field
                        |> Maybe.withDefault currentModuleName
            in
            Just (usageOfDeprecatedElement moduleName (Node.value field) Field (Node.range field) message)

        NotDeprecated () ->
            Nothing


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    let
        usages : List DeprecatedElementUsage
        usages =
            expressionVisitorHelp node context
    in
    ( List.map error usages, { context | usages = usages ++ context.usages } )


expressionVisitorHelp : Node Expression -> ModuleContext -> List DeprecatedElementUsage
expressionVisitorHelp (Node nodeRange node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            reportElementAsList
                context
                nodeRange
                (always nodeRange)
                name
                []

        Expression.LetExpression letBlock ->
            reportLetDeclarations context letBlock.declarations []

        Expression.CaseExpression { cases } ->
            reportPatterns
                context
                (List.map Tuple.first cases)
                []

        Expression.RecordUpdateExpression (Node range name) _ ->
            reportElementAsList
                context
                range
                (always range)
                name
                []

        Expression.RecordAccess _ field ->
            case reportField context.lookupTable context.currentModuleName field of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        Expression.RecordAccessFunction fieldName ->
            case reportField context.lookupTable context.currentModuleName (Node nodeRange fieldName) of
                Just err ->
                    [ err ]

                Nothing ->
                    []

        _ ->
            []


reportElementAsList : ModuleContext -> Range -> (() -> Range) -> String -> List DeprecatedElementUsage -> List DeprecatedElementUsage
reportElementAsList context rangeForLookupTable rangeForReport name acc =
    case ModuleNameLookupTable.fullModuleNameAt context.lookupTable rangeForLookupTable of
        Just moduleName ->
            case Dict.get ( moduleName, name ) context.deprecatedElements of
                Just message ->
                    usageOfDeprecatedElement moduleName name Element (rangeForReport ()) message :: acc

                Nothing ->
                    case Dict.get moduleName context.deprecatedModules of
                        Just (DeprecatedModule message) ->
                            usageOfDeprecatedElement moduleName name Module (rangeForReport ()) message :: acc

                        Just (DeprecatedDependency ()) ->
                            usageOfDeprecatedElement moduleName name Dependency (rangeForReport ()) Nothing :: acc

                        Nothing ->
                            acc

        Nothing ->
            acc


reportElementAsMaybe : ModuleContext -> Range -> String -> Maybe DeprecatedElementUsage
reportElementAsMaybe context range name =
    case ModuleNameLookupTable.fullModuleNameAt context.lookupTable range of
        Just moduleName ->
            case Dict.get moduleName context.deprecatedModules of
                Just (DeprecatedModule message) ->
                    Just (usageOfDeprecatedElement moduleName name Module range message)

                Just (DeprecatedDependency ()) ->
                    Just (usageOfDeprecatedElement moduleName name Dependency range Nothing)

                Nothing ->
                    Dict.get ( moduleName, name ) context.deprecatedElements
                        |> Maybe.map (\message -> usageOfDeprecatedElement moduleName name Element range message)

        Nothing ->
            Nothing


reportParameter : ModuleName -> Range -> String -> Maybe DeprecatedElementUsage
reportParameter currentModuleName range name =
    case containsDeprecated name of
        Deprecated message ->
            Just (usageOfDeprecatedElement currentModuleName name Parameter range message)

        NotDeprecated () ->
            Nothing


type Origin
    = Element
    | Module
    | Dependency
    | Field
    | Parameter


usageOfDeprecatedElement : ModuleName -> String -> Origin -> Range -> Maybe String -> DeprecatedElementUsage
usageOfDeprecatedElement =
    DeprecatedElementUsage


error : DeprecatedElementUsage -> Rule.Error {}
error { origin, range, deprecationMessage } =
    let
        deprecation : String
        deprecation =
            case deprecationMessage of
                Just message ->
                    "Deprecation: " ++ (message |> String.trim |> Regex.replace trimEndRegex (always "") |> String.trim)

                Nothing ->
                    "Please check its documentation to know the alternative solutions."

        details : List String
        details =
            case origin of
                Element ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    , deprecation
                    ]

                Module ->
                    [ "The module where this element is defined was marked as deprecated and should not be used anymore."
                    , deprecation
                    ]

                Dependency ->
                    [ "The dependency where this element is defined was marked as deprecated and should not be used anymore."
                    , case deprecationMessage of
                        Just message ->
                            "Deprecation: " ++ String.trimLeft message

                        Nothing ->
                            "Please check its documentation or your review configuration to know the alternative solutions."
                    ]

                Field ->
                    [ "This element was marked as deprecated and should not be used anymore."
                    , deprecation
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


dataExtractor : ProjectContext -> Encode.Value
dataExtractor projectContext =
    let
        deprecatedModules : Set String
        deprecatedModules =
            Dict.foldl
                (\key _ acc ->
                    Set.insert (String.join "." key) acc
                )
                Set.empty
                projectContext.deprecatedModules
    in
    projectContext.usages
        |> Dict.foldl
            (\( moduleName, name ) count acc ->
                Dict.update
                    (String.join "." moduleName)
                    (Maybe.withDefault Dict.empty >> Dict.insert name count >> Just)
                    acc
            )
            Dict.empty
        |> Dict.toList
        |> List.map (\( moduleName, dict ) -> ( moduleName, encodeCountDict (Set.member moduleName deprecatedModules) dict ))
        |> List.sortBy (\( _, ( _, count ) ) -> -count)
        |> List.map (\( moduleName, ( dict, _ ) ) -> ( moduleName, dict ))
        |> Encode.object


encodeCountDict : Bool -> Dict String Int -> ( Encode.Value, Int )
encodeCountDict isModuleDeprecated dict =
    let
        ( fields, totalCount ) =
            Dict.foldl
                (\name count ( accList, accCount ) ->
                    ( ( name, count ) :: accList
                    , accCount + count
                    )
                )
                ( [], 0 )
                dict
    in
    ( Encode.object
        [ ( "total", Encode.int totalCount )
        , ( "isModuleDeprecated", Encode.bool isModuleDeprecated )
        , ( "usages"
          , Encode.object
                (fields |> List.sortBy (Tuple.second >> negate) |> List.map (Tuple.mapSecond Encode.int))
          )
        ]
    , totalCount
    )


maybeCons : Maybe a -> List a -> List a
maybeCons maybe list =
    case maybe of
        Just a ->
            a :: list

        Nothing ->
            list
