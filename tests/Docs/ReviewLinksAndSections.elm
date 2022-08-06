module Docs.ReviewLinksAndSections exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Docs.Utils.Link as Link
import Docs.Utils.Slug as Slug
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Version
import Regex exposing (Regex)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports problems with links and sections in Elm projects.

    config =
        [ Docs.ReviewLinksAndSections.rule
        ]


## Fail

Links to missing modules or sections are reported.

    {-| Link to [missing module](Unknown-Module).
    -}
    a =
        1

    {-| Link to [missing section](#unknown).
    -}
    a =
        1

In packages, links that would appear in the public documentation and that link to sections not part of the public documentation are reported.

    module Exposed exposing (a)

    import Internal

    {-| Link to [internal details](Internal#section).
    -}
    a =
        1

Sections that would have the same generated id are reported,
so that links don't inadvertently point to the wrong location.

    module A exposing (element, section)

    {-|


    # Section

    The above conflicts with the id generated
    for the `section` value.

    -}

    element =
        1

    section =
        1


## Success

    module Exposed exposing (a, b)

    import Internal

    {-| Link to [exposed b](#b).
    -}
    a =
        1

    b =
        2


## When (not) to enable this rule

For packages, this rule will be useful to prevent having dead links in the package documentation.

For applications, this rule will be useful if you have the habit of writing documentation the way you do in Elm packages,
and want to prevent it from going out of date.

This rule will not be useful if your project is an application and no-one in the team has the habit of writing
package-like documentation.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.ReviewLinksAndSections
```


## Thanks

Thanks to @lue-bird for helping out with this rule.

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.ReviewLinksAndSections" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { fileLinksAndSections : List FileLinksAndSections
    , packageNameAndVersion : Maybe { name : String, version : String }
    , exposedModules : Set ModuleName
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { fileLinksAndSections = []
    , packageNameAndVersion = Nothing
    , exposedModules = Set.empty
    }


type alias FileLinksAndSections =
    { moduleName : ModuleName
    , fileKey : FileKey
    , sections : List Section
    , links : List MaybeExposedLink
    }


type FileKey
    = ModuleKey Rule.ModuleKey
    | ReadmeKey Rule.ReadmeKey


type alias ModuleContext =
    { isModuleExposed : Bool
    , exposedElements : Set String
    , moduleName : ModuleName
    , commentSections : List SectionWithRange
    , sections : List Section
    , links : List MaybeExposedLink
    }


type alias Section =
    { slug : String
    , isExposed : Bool
    }


type MaybeExposedLink
    = MaybeExposedLink MaybeExposedLinkData


type alias MaybeExposedLinkData =
    { link : Link.Link
    , linkRange : Range
    , isExposed : Bool
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\moduleName projectContext ->
            { isModuleExposed = Set.member moduleName projectContext.exposedModules
            , exposedElements = Set.empty
            , moduleName = moduleName
            , commentSections = []
            , sections = []
            , links = []
            }
        )
        |> Rule.withModuleName


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleContext ->
            { fileLinksAndSections =
                [ { moduleName = moduleContext.moduleName
                  , fileKey = ModuleKey moduleKey
                  , sections = moduleContext.sections
                  , links = moduleContext.links
                  }
                ]
            , packageNameAndVersion = Nothing
            , exposedModules = Set.empty
            }
        )
        |> Rule.withModuleKey


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { fileLinksAndSections = List.append newContext.fileLinksAndSections previousContext.fileLinksAndSections
    , packageNameAndVersion = previousContext.packageNameAndVersion
    , exposedModules = previousContext.exposedModules
    }


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withModuleDocumentationVisitor moduleDocumentationVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson projectContext =
    case Maybe.map .project maybeElmJson of
        Just (Elm.Project.Package { name, version, exposed }) ->
            ( []
            , { projectContext
                | packageNameAndVersion = Just { name = Elm.Package.toString name, version = Elm.Version.toString version }
                , exposedModules = listExposedModules exposed
              }
            )

        _ ->
            ( [], projectContext )


listExposedModules : Elm.Project.Exposed -> Set ModuleName
listExposedModules exposed =
    let
        exposedModules : List ModuleName
        exposedModules =
            exposedModulesFromPackageAsList exposed
                |> List.map (Elm.Module.toString >> String.split ".")
    in
    Set.fromList ([] :: exposedModules)


exposedModulesFromPackageAsList : Elm.Project.Exposed -> List Elm.Module.Name
exposedModulesFromPackageAsList exposed =
    case exposed of
        Elm.Project.ExposedList list ->
            list

        Elm.Project.ExposedDict list ->
            List.concatMap Tuple.second list



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
readmeVisitor maybeReadmeInfo projectContext =
    case maybeReadmeInfo of
        Just { readmeKey, content } ->
            let
                isReadmeExposed : Bool
                isReadmeExposed =
                    Set.member [] projectContext.exposedModules

                sectionsAndLinks : { titleSections : List SectionWithRange, links : List MaybeExposedLink }
                sectionsAndLinks =
                    findSectionsAndLinks
                        []
                        isReadmeExposed
                        { content = content
                        , startRow = 1
                        }
            in
            ( duplicateSectionErrors Set.empty sectionsAndLinks.titleSections
                |> List.map (Rule.errorForReadme readmeKey duplicateSectionErrorDetails)
            , { fileLinksAndSections =
                    { moduleName = []
                    , fileKey = ReadmeKey readmeKey
                    , sections = List.map removeRangeFromSection sectionsAndLinks.titleSections
                    , links = sectionsAndLinks.links
                    }
                        :: projectContext.fileLinksAndSections
              , packageNameAndVersion = projectContext.packageNameAndVersion
              , exposedModules = projectContext.exposedModules
              }
            )

        Nothing ->
            ( [], projectContext )



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor node context =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            -- We'll keep `exposedElements` empty, which will make `declarationListVisitor` fill it with the known
            -- declarations.
            ( [], context )

        Exposing.Explicit exposed ->
            ( [], { context | exposedElements = Set.fromList (List.map exposedName exposed) } )


exposedName : Node Exposing.TopLevelExpose -> String
exposedName node =
    case Node.value node of
        Exposing.InfixExpose string ->
            string

        Exposing.FunctionExpose string ->
            string

        Exposing.TypeOrAliasExpose string ->
            string

        Exposing.TypeExpose exposedType ->
            exposedType.name



-- MODULE DOCUMENTATION VISITOR


moduleDocumentationVisitor : Maybe (Node String) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
moduleDocumentationVisitor moduleDocumentation context =
    let
        sectionsAndLinks : { titleSections : List SectionWithRange, links : List MaybeExposedLink }
        sectionsAndLinks =
            case moduleDocumentation of
                Just (Node range content) ->
                    findSectionsAndLinks
                        context.moduleName
                        context.isModuleExposed
                        { content = content, startRow = range.start.row }

                Nothing ->
                    { titleSections = [], links = [] }
    in
    ( []
    , { isModuleExposed = context.isModuleExposed
      , exposedElements = context.exposedElements
      , moduleName = context.moduleName
      , commentSections = sectionsAndLinks.titleSections
      , sections =
            List.append
                (List.map removeRangeFromSection sectionsAndLinks.titleSections)
                context.sections
      , links = List.append sectionsAndLinks.links context.links
      }
    )



-- DECLARATION VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor declarations context =
    let
        exposedElements : Set String
        exposedElements =
            if Set.isEmpty context.exposedElements then
                Set.fromList (List.filterMap nameOfDeclaration declarations)

            else
                context.exposedElements

        knownSections : List { slug : String, isExposed : Bool }
        knownSections =
            List.append
                (List.map (\slug -> { slug = slug, isExposed = True }) (Set.toList exposedElements))
                context.sections

        sectionsAndLinks : List { titleSections : List SectionWithRange, links : List MaybeExposedLink }
        sectionsAndLinks =
            List.map
                (findSectionsAndLinksForDeclaration
                    context.moduleName
                    (if context.isModuleExposed then
                        exposedElements

                     else
                        Set.empty
                    )
                )
                declarations

        titleSections : List SectionWithRange
        titleSections =
            List.concatMap .titleSections sectionsAndLinks
    in
    ( duplicateSectionErrors exposedElements (List.append titleSections context.commentSections)
        |> List.map (Rule.error duplicateSectionErrorDetails)
    , { isModuleExposed = context.isModuleExposed
      , exposedElements = exposedElements
      , moduleName = context.moduleName
      , commentSections = context.commentSections
      , sections = List.append (List.map removeRangeFromSection titleSections) knownSections
      , links = List.append (List.concatMap .links sectionsAndLinks) context.links
      }
    )


duplicateSectionErrors : Set String -> List SectionWithRange -> List Range
duplicateSectionErrors exposedElements sections =
    List.foldl
        (\{ slug, range } { errors, knownSections } ->
            if Set.member slug knownSections then
                { errors = range :: errors
                , knownSections = knownSections
                }

            else
                { errors = errors
                , knownSections = Set.insert slug knownSections
                }
        )
        { errors = [], knownSections = exposedElements }
        sections
        |> .errors


extractSlugsFromHeadings : { content : String, startRow : Int } -> List (Node String)
extractSlugsFromHeadings doc =
    doc.content
        |> String.lines
        |> List.indexedMap
            (\lineNumber line ->
                Regex.find specialsToHash line
                    |> List.concatMap .submatches
                    |> List.filterMap identity
                    |> List.map
                        (\slug ->
                            Node
                                { start = { row = lineNumber + doc.startRow, column = 1 }
                                , end = { row = lineNumber + doc.startRow, column = String.length line + 1 }
                                }
                                (Slug.toSlug slug)
                        )
            )
        |> List.concat


specialsToHash : Regex
specialsToHash =
    "^#{1,6}\\s+(.*)$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


nameOfDeclaration : Node Declaration -> Maybe String
nameOfDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            declaration
                |> Node.value
                |> .name
                |> Node.value
                |> Just

        Declaration.AliasDeclaration { name } ->
            Just (Node.value name)

        Declaration.CustomTypeDeclaration { name } ->
            Just (Node.value name)

        Declaration.PortDeclaration { name } ->
            Just (Node.value name)

        Declaration.InfixDeclaration { operator } ->
            Just (Node.value operator)

        Declaration.Destructuring _ _ ->
            Nothing


docOfDeclaration : Declaration -> Maybe (Node Documentation)
docOfDeclaration declaration =
    case declaration of
        Declaration.FunctionDeclaration { documentation } ->
            documentation

        Declaration.AliasDeclaration { documentation } ->
            documentation

        Declaration.CustomTypeDeclaration { documentation } ->
            documentation

        Declaration.PortDeclaration _ ->
            Nothing

        Declaration.InfixDeclaration _ ->
            Nothing

        Declaration.Destructuring _ _ ->
            Nothing


findSectionsAndLinksForDeclaration : ModuleName -> Set String -> Node Declaration -> { titleSections : List SectionWithRange, links : List MaybeExposedLink }
findSectionsAndLinksForDeclaration currentModuleName exposedElements declaration =
    case docOfDeclaration (Node.value declaration) of
        Just doc ->
            let
                name : String
                name =
                    nameOfDeclaration declaration
                        |> Maybe.withDefault ""

                isExposed : Bool
                isExposed =
                    Set.member name exposedElements
            in
            findSectionsAndLinks
                currentModuleName
                isExposed
                { content = Node.value doc, startRow = (Node.range doc).start.row }

        Nothing ->
            { titleSections = [], links = [] }


type alias SectionWithRange =
    { slug : String
    , range : Range
    , isExposed : Bool
    }


removeRangeFromSection : SectionWithRange -> Section
removeRangeFromSection { slug, isExposed } =
    { slug = slug
    , isExposed = isExposed
    }


findSectionsAndLinks : ModuleName -> Bool -> { content : String, startRow : Int } -> { titleSections : List SectionWithRange, links : List MaybeExposedLink }
findSectionsAndLinks currentModuleName isExposed doc =
    let
        titleSections : List SectionWithRange
        titleSections =
            extractSlugsFromHeadings doc
                |> List.map
                    (\slug ->
                        { slug = Node.value slug
                        , range = Node.range slug
                        , isExposed = isExposed
                        }
                    )

        links : List MaybeExposedLink
        links =
            Link.findLinks (doc.startRow - 1) currentModuleName doc.content
                |> List.map
                    (\link ->
                        MaybeExposedLink
                            { link = Node.value link
                            , linkRange = Node.range link
                            , isExposed = isExposed
                            }
                    )
    in
    { titleSections = titleSections
    , links = links
    }



-- FINAL EVALUATION


finalEvaluation : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluation projectContext =
    let
        sectionsPerModule : Dict ModuleName (List Section)
        sectionsPerModule =
            projectContext.fileLinksAndSections
                |> List.map (\module_ -> ( module_.moduleName, module_.sections ))
                |> Dict.fromList
    in
    List.concatMap (errorsForFile projectContext sectionsPerModule) projectContext.fileLinksAndSections


errorsForFile : ProjectContext -> Dict ModuleName (List Section) -> FileLinksAndSections -> List (Rule.Error scope)
errorsForFile projectContext sectionsPerModule fileLinksAndSections =
    List.filterMap
        (errorForFile projectContext sectionsPerModule fileLinksAndSections)
        fileLinksAndSections.links


errorForFile : ProjectContext -> Dict ModuleName (List Section) -> FileLinksAndSections -> MaybeExposedLink -> Maybe (Rule.Error scope)
errorForFile projectContext sectionsPerModule fileLinksAndSections (MaybeExposedLink maybeExposedLink) =
    case maybeExposedLink.link.file of
        Link.ModuleTarget moduleName ->
            reportErrorForModule projectContext sectionsPerModule fileLinksAndSections maybeExposedLink moduleName

        Link.ReadmeTarget ->
            reportErrorForReadme sectionsPerModule fileLinksAndSections.fileKey maybeExposedLink

        Link.PackagesTarget packageTarget ->
            reportErrorsForPackagesTarget projectContext sectionsPerModule fileLinksAndSections maybeExposedLink packageTarget

        Link.External target ->
            reportErrorsForExternalTarget (projectContext.packageNameAndVersion == Nothing) fileLinksAndSections.fileKey maybeExposedLink.linkRange target


reportErrorsForPackagesTarget : ProjectContext -> Dict ModuleName (List Section) -> FileLinksAndSections -> MaybeExposedLinkData -> { name : String, version : String, subTarget : Link.SubTarget } -> Maybe (Rule.Error scope)
reportErrorsForPackagesTarget projectContext sectionsPerModule fileLinksAndSections maybeExposedLink { name, version, subTarget } =
    case projectContext.packageNameAndVersion of
        Just currentPackage ->
            if name == currentPackage.name && (version == "latest" || version == currentPackage.version) then
                reportErrorForCurrentPackageSubTarget projectContext sectionsPerModule fileLinksAndSections maybeExposedLink subTarget

            else
                Nothing

        Nothing ->
            Nothing


reportErrorForCurrentPackageSubTarget : ProjectContext -> Dict ModuleName (List Section) -> FileLinksAndSections -> MaybeExposedLinkData -> Link.SubTarget -> Maybe (Rule.Error scope)
reportErrorForCurrentPackageSubTarget projectContext sectionsPerModule fileLinksAndSections maybeExposedLink subTarget =
    case subTarget of
        Link.ModuleSubTarget moduleName ->
            reportErrorForModule projectContext sectionsPerModule fileLinksAndSections maybeExposedLink moduleName

        Link.ReadmeSubTarget ->
            reportErrorForReadme sectionsPerModule fileLinksAndSections.fileKey maybeExposedLink


reportErrorForModule : ProjectContext -> Dict ModuleName (List Section) -> FileLinksAndSections -> MaybeExposedLinkData -> ModuleName -> Maybe (Rule.Error scope)
reportErrorForModule projectContext sectionsPerModule fileLinksAndSections maybeExposedLink moduleName =
    case Dict.get moduleName sectionsPerModule of
        Just existingSections ->
            if Set.member fileLinksAndSections.moduleName projectContext.exposedModules && not (Set.member moduleName projectContext.exposedModules) then
                Just (reportLinkToNonExposedModule fileLinksAndSections.fileKey maybeExposedLink.linkRange)

            else
                reportIfMissingSection fileLinksAndSections.fileKey existingSections maybeExposedLink

        Nothing ->
            Just (reportUnknownModule fileLinksAndSections.fileKey moduleName maybeExposedLink.linkRange)


reportErrorForReadme : Dict (List comparable) (List Section) -> FileKey -> MaybeExposedLinkData -> Maybe (Rule.Error scope)
reportErrorForReadme sectionsPerModule fileKey maybeExposedLink =
    case Dict.get [] sectionsPerModule of
        Just existingSections ->
            reportIfMissingSection fileKey existingSections maybeExposedLink

        Nothing ->
            Just (reportLinkToMissingReadme fileKey maybeExposedLink.linkRange)


reportErrorsForExternalTarget : Bool -> FileKey -> Range -> String -> Maybe (Rule.Error scope)
reportErrorsForExternalTarget isApplication fileKey linkRange target =
    if isApplication || String.contains "://" target then
        Nothing

    else
        Just (reportLinkToExternalResourceWithoutProtocol fileKey linkRange)


reportIfMissingSection : FileKey -> List Section -> MaybeExposedLinkData -> Maybe (Rule.Error scope)
reportIfMissingSection fileKey existingSectionsForTargetFile { isExposed, linkRange, link } =
    case link.slug of
        Just "" ->
            Just (reportLinkWithEmptySlug fileKey linkRange)

        Just slug ->
            case find (\section -> section.slug == slug) existingSectionsForTargetFile of
                Just section ->
                    if isExposed && not section.isExposed then
                        Just (reportLinkToNonExposedSection fileKey linkRange)

                    else
                        Nothing

                Nothing ->
                    Just (reportLink fileKey linkRange)

        Nothing ->
            Nothing


reportLink : FileKey -> Range -> Rule.Error scope
reportLink fileKey range =
    reportForFile fileKey
        { message = "Link points to a non-existing section or element"
        , details = [ "This is a dead link." ]
        }
        range


reportLinkToNonExposedModule : FileKey -> Range -> Rule.Error scope
reportLinkToNonExposedModule fileKey range =
    reportForFile fileKey
        { message = "Link in public documentation points to non-exposed module"
        , details = [ "Users will not be able to follow the link." ]
        }
        range


reportLinkToNonExposedSection : FileKey -> Range -> Rule.Error scope
reportLinkToNonExposedSection fileKey range =
    reportForFile fileKey
        { message = "Link in public documentation points to non-exposed section"
        , details = [ "Users will not be able to follow the link." ]
        }
        range


reportLinkWithEmptySlug : FileKey -> Range -> Rule.Error scope
reportLinkWithEmptySlug fileKey range =
    reportForFile fileKey
        { message = "Link to empty section is unnecessary"
        , details = [ "Links to # not followed by an id don't provide any value to the user. I suggest to either strip the # or remove the link." ]
        }
        range


reportUnknownModule : FileKey -> ModuleName -> Range -> Rule.Error scope
reportUnknownModule fileKey moduleName range =
    reportForFile fileKey
        { message = "Link points to non-existing module " ++ String.join "." moduleName
        , details = [ "This is a dead link." ]
        }
        range


reportLinkToMissingReadme : FileKey -> Range -> Rule.Error scope
reportLinkToMissingReadme fileKey range =
    reportForFile fileKey
        { message = "Link points to missing README"
        , details = [ "elm-review only looks for a 'README.md' located next to your 'elm.json'. Maybe it's positioned elsewhere or named differently?" ]
        }
        range


reportLinkToExternalResourceWithoutProtocol : FileKey -> Range -> Rule.Error scope
reportLinkToExternalResourceWithoutProtocol fileKey range =
    reportForFile fileKey
        { message = "Link to unknown resource without a protocol"
        , details =
            [ "I have trouble figuring out what kind of resource is linked here."
            , "If it should link to a module, then they should be in the form 'Some-Module-Name'."
            , "If it's a link to an external resource, they should start with a protocol, like `https://www.fruits.com`, otherwise the link will point to an unknown resource on package.elm-lang.org."
            ]
        }
        range


duplicateSectionErrorDetails : { message : String, details : List String }
duplicateSectionErrorDetails =
    { message = "Duplicate section"
    , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
    }


reportForFile : FileKey -> { message : String, details : List String } -> Range -> Rule.Error scope
reportForFile fileKey =
    case fileKey of
        ModuleKey moduleKey ->
            Rule.errorForModule moduleKey

        ReadmeKey readmeKey ->
            Rule.errorForReadme readmeKey


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
