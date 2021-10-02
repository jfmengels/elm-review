module Docs.ReviewAtDocs exposing (rule)

{-|

@docs rule

-}

import Dict
import Docs.Utils.ExposedFromProject as ExposedFromProject
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Parser exposing ((|.), (|=), Parser)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)



-- TODO Report @docs Thing(..) like in:
--   https://github.com/Holmusk/swagger-decoder/blob/1.0.0/src/Swagger/Types.elm
--   https://package.elm-lang.org/packages/Holmusk/swagger-decoder/latest/Swagger-Types#Scheme
-- TODO Report https://github.com/elm/package.elm-lang.org/issues/311
-- TODO Report https://github.com/elm/package.elm-lang.org/issues/216
-- TODO Report @docs in README?


{-| Reports problems with the usages of `@docs`.

    config =
        [ Docs.ReviewAtDocs.rule
        ]

The aim of this rule is to report problems for documentation in packages that the Elm compiler doesn't report but that
break documentation, and to replicate the same checks for applications so that you can write documentation without
worrying about them getting stale.

The rule will report issues with malformed `@docs` directives that will cause the documentation to not be displayed properly once published.

  - `@docs` on the first line

```elm
{-|

@docs a

-}
```

  - Indented `@docs`

```elm
{-|

    @docs a

-}
```

Once there are no more issues of malformed `@docs`, the rule will report about:

  - Missing `@docs` for exposed elements

  - `@docs` for non-exposed or missing elements

  - Duplicate `@docs` references

  - Usage of `@docs` outside of the module documentation

If a module does not have _any_ usage of `@docs`, then the rule will not report anything, as the rule will assume the
module is not meant to be documented at this moment in time. An exception is made for exposed modules of a package.


## When (not) to enable this rule

This rule will not be useful if your project is an application and no-one in the team has the habit of writing
package-like documentation.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.ReviewAtDocs
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Docs.ReviewAtDocs" initialContext
        |> Rule.withElmJsonModuleVisitor elmJsonVisitor
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withDeclarationListVisitor (\nodes context -> ( declarationListVisitor nodes context, context ))
        |> Rule.fromModuleRuleSchema


type alias Context =
    { exposedModulesFromProject : Set String
    , moduleIsExposed : Bool
    , exposedFromModule : Exposing
    , hasMalformedDocs : Bool
    , docsReferences : List (Node String)
    }


initialContext : Context
initialContext =
    { exposedModulesFromProject = Set.empty
    , moduleIsExposed = False
    , exposedFromModule = Exposing.All Range.emptyRange
    , hasMalformedDocs = False
    , docsReferences = []
    }



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe Elm.Project.Project -> Context -> Context
elmJsonVisitor maybeProject context =
    let
        exposedModules : Set String
        exposedModules =
            case maybeProject of
                Just project ->
                    ExposedFromProject.exposedModules project

                _ ->
                    Set.empty
    in
    { context | exposedModulesFromProject = exposedModules }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor node context =
    ( []
    , { context
        | exposedFromModule = Module.exposingList (Node.value node)
        , moduleIsExposed = Set.member (Module.moduleName (Node.value node) |> String.join ".") context.exposedModulesFromProject
      }
    )



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> Context -> ( List (Rule.Error {}), Context )
commentsVisitor nodes context =
    case find (Node.value >> String.startsWith "{-|") nodes of
        Just (Node range comment) ->
            case String.lines comment of
                firstLine :: restOfLines ->
                    let
                        ( linesThatStartWithAtDocs, linesThatDontStartWithAtDocs ) =
                            restOfLines
                                |> List.indexedMap (\index line -> ( index + range.start.row + 1, line ))
                                |> List.partition (Tuple.second >> String.startsWith "@docs ")

                        misformedDocsErrors : List (Rule.Error {})
                        misformedDocsErrors =
                            List.append
                                (reportDocsOnFirstLine range.start.row firstLine)
                                (List.concatMap reportIndentedDocs linesThatDontStartWithAtDocs)
                    in
                    ( misformedDocsErrors
                    , { context
                        | docsReferences = List.concatMap collectDocStatements linesThatStartWithAtDocs
                        , hasMalformedDocs = not (List.isEmpty misformedDocsErrors)
                      }
                    )

                [] ->
                    ( [], context )

        Nothing ->
            ( [], context )


reportDocsOnFirstLine : Int -> String -> List (Rule.Error {})
reportDocsOnFirstLine lineNumber line =
    Parser.run (Parser.succeed identity |. Parser.keyword "{-|" |= docsWithSpacesParser lineNumber) line
        |> Result.map
            (\range ->
                [ Rule.error
                    { message = "Found @docs on the first line"
                    , details = [ "Using @docs on the first line will make for a broken documentation once published. Please move it to the beginning of the next line." ]
                    }
                    range
                ]
            )
        |> Result.withDefault []


reportIndentedDocs : ( Int, String ) -> List (Rule.Error {})
reportIndentedDocs ( lineNumber, line ) =
    Parser.run (docsWithSpacesParser lineNumber) line
        |> Result.map
            (\range ->
                [ Rule.error
                    { message = "Found indented @docs"
                    , details = [ "@docs need to be at the beginning of a line, otherwise they can lead to broken documentation once published. on the first line will make for a broken documentation once published. Please remove the leading spaces" ]
                    }
                    range
                ]
            )
        |> Result.withDefault []


docsWithSpacesParser : Int -> Parser Range
docsWithSpacesParser row =
    Parser.succeed
        (\startColumn endColumn ->
            { start = { row = row, column = startColumn }, end = { row = row, column = endColumn } }
        )
        |. Parser.spaces
        |= Parser.getCol
        |. Parser.keyword "@docs"
        |= Parser.getCol


collectDocStatements : ( Int, String ) -> List (Node String)
collectDocStatements ( lineNumber, string ) =
    Parser.run (docElementsParser lineNumber) string
        |> Result.withDefault []


docElementsParser : Int -> Parser (List (Node String))
docElementsParser startRow =
    Parser.succeed identity
        |. Parser.keyword "@docs"
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ","
            , end = ""
            , spaces = Parser.spaces
            , item = docsItemParser startRow
            , trailing = Parser.Forbidden
            }


docsItemParser : Int -> Parser (Node String)
docsItemParser row =
    Parser.succeed
        (\startColumn name endColumn ->
            Node
                { start = { row = row, column = startColumn }
                , end = { row = row, column = endColumn }
                }
                name
        )
        |= Parser.getCol
        |= Parser.variable
            { start = Char.isAlpha
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.empty
            }
        |= Parser.getCol



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> Context -> List (Rule.Error {})
declarationListVisitor nodes context =
    if context.hasMalformedDocs || (List.isEmpty context.docsReferences && not context.moduleIsExposed) then
        List.concatMap errorsForDocsInDeclarationDoc nodes

    else
        let
            exposedNodes : List (Node String)
            exposedNodes =
                case context.exposedFromModule of
                    Exposing.All _ ->
                        List.filterMap declarationName nodes

                    Exposing.Explicit explicit ->
                        List.map topLevelExposeName explicit

            exposed : Set String
            exposed =
                Set.fromList (List.map Node.value exposedNodes)

            ( duplicateDocErrors, referencedElements ) =
                errorsForDuplicateDocs context.docsReferences
        in
        List.concat
            [ errorsForDocsForNonExposedElements exposed context.docsReferences
            , errorsForExposedElementsWithoutADocsReference referencedElements exposedNodes
            , List.concatMap errorsForDocsInDeclarationDoc nodes
            , duplicateDocErrors
            ]


errorsForDocsForNonExposedElements : Set String -> List (Node String) -> List (Rule.Error {})
errorsForDocsForNonExposedElements exposed docsReferences =
    docsReferences
        |> List.filter (\(Node _ name) -> not (Set.member name exposed))
        |> List.map
            (\(Node range name) ->
                Rule.error
                    { message = "Found @docs reference for non-exposed `" ++ name ++ "`"
                    , details =
                        [ "I couldn't find this element among the module's exposed elements. Maybe you removed or renamed it recently."
                        , "Please remove the @docs reference or update the reference to the new name."
                        ]
                    }
                    range
            )


errorsForExposedElementsWithoutADocsReference : Set String -> List (Node String) -> List (Rule.Error {})
errorsForExposedElementsWithoutADocsReference allDocsReferences exposedNodes =
    exposedNodes
        |> List.filter (\(Node _ name) -> not (Set.member name allDocsReferences))
        |> List.map
            (\(Node range name) ->
                Rule.error
                    { message = "Missing @docs reference for exposed `" ++ name ++ "`"
                    , details =
                        [ "There is no @docs reference for this element. Maybe you exposed or renamed it recently."
                        , "Please add a @docs reference to it the module documentation (the one at the top of the module) like this:"
                        , """{-|
@docs """ ++ name ++ """
-}"""
                        ]
                    }
                    range
            )


errorsForDocsInDeclarationDoc : Node Declaration -> List (Rule.Error {})
errorsForDocsInDeclarationDoc node =
    case docForDeclaration node of
        Just ( declarationType, Node docRange docContent ) ->
            indexedConcatMap
                (\lineNumber lineContent ->
                    lineContent
                        |> Parser.run (docsWithSpacesParser (lineNumber + docRange.start.row))
                        |> Result.map
                            (\range ->
                                [ Rule.error
                                    { message = "Found usage of @docs in a " ++ declarationType ++ " documentation"
                                    , details = [ "@docs can only be used in the module's documentation. You should remove this @docs and move it there." ]
                                    }
                                    range
                                ]
                            )
                        |> Result.withDefault []
                )
                (String.lines docContent)

        Nothing ->
            []


docForDeclaration : Node Declaration -> Maybe ( String, Node String )
docForDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            Maybe.map (Tuple.pair "function") function.documentation

        Declaration.AliasDeclaration typeAlias ->
            Maybe.map (Tuple.pair "type") typeAlias.documentation

        Declaration.CustomTypeDeclaration customType ->
            Maybe.map (Tuple.pair "type") customType.documentation

        Declaration.PortDeclaration _ ->
            -- TODO Support port declaration in elm-syntax v8
            Nothing

        Declaration.InfixDeclaration _ ->
            Nothing

        Declaration.Destructuring _ _ ->
            Nothing


errorsForDuplicateDocs : List (Node String) -> ( List (Rule.Error {}), Set String )
errorsForDuplicateDocs docsReferences =
    List.foldl
        (\(Node range name) ( errors, previouslyFoundNames ) ->
            case Dict.get name previouslyFoundNames of
                Just lineNumber ->
                    ( Rule.error
                        { message = "Found duplicate @docs reference for `element`"
                        , details = [ "An element should only be referenced once, but I found a previous reference to it on line " ++ String.fromInt lineNumber ++ ". Please remove one of them." ]
                        }
                        range
                        :: errors
                    , previouslyFoundNames
                    )

                Nothing ->
                    ( errors, Dict.insert name range.start.row previouslyFoundNames )
        )
        ( [], Dict.empty )
        docsReferences
        |> Tuple.mapSecond (Dict.keys >> Set.fromList)


declarationName : Node Declaration -> Maybe (Node String)
declarationName node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            function.declaration |> Node.value |> .name |> Just

        Declaration.AliasDeclaration typeAlias ->
            Just typeAlias.name

        Declaration.CustomTypeDeclaration type_ ->
            Just type_.name

        Declaration.PortDeclaration signature ->
            Just signature.name

        Declaration.InfixDeclaration { operator } ->
            Just operator

        Declaration.Destructuring _ _ ->
            Nothing


topLevelExposeName : Node Exposing.TopLevelExpose -> Node String
topLevelExposeName (Node range topLevelExpose) =
    case topLevelExpose of
        Exposing.InfixExpose name ->
            Node range name

        Exposing.FunctionExpose name ->
            Node range name

        Exposing.TypeOrAliasExpose name ->
            Node range name

        Exposing.TypeExpose { name } ->
            Node range name


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


indexedConcatMap : (Int -> a -> List b) -> List a -> List b
indexedConcatMap function list =
    List.foldl
        (\a ( index, acc ) -> ( index + 1, List.append (function index a) acc ))
        ( 0, [] )
        list
        |> Tuple.second
