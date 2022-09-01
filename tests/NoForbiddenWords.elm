module NoForbiddenWords exposing (rule)

{-|

@docs rule

-}

import Elm.Project as Project exposing (Project)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Regex exposing (Regex)
import Review.Rule as Rule exposing (Rule)


{-| Forbid certain words in Elm comments, README and elm.json (package summary only).

    config : List Rule
    config =
        [ NoForbiddenWords.rule [ "TODO", "- [ ]" ]
        ]


## Failure Examples

Based on the configured words `TODO` and `- [ ]` the following examples would fail:

    -- TODO: Finish writing this function



Multi-line comments `{- ... -}` and documentation `{-| ... -}` also work:



    {- Actions
       - [ ] Documentation
       - [ ] Tests
    -}

-}
rule : List String -> Rule
rule words =
    Rule.newProjectRuleSchema "NoForbiddenWords" ()
        |> Rule.withElmJsonProjectVisitor (elmJsonVisitor words)
        |> Rule.withReadmeProjectVisitor (readmeVisitor words)
        |> Rule.withModuleVisitor (moduleVisitor words)
        |> Rule.withModuleContext
            { fromModuleToProject = \_ _ () -> ()
            , fromProjectToModule = \_ _ () -> ()
            , foldProjectContexts = \() () -> ()
            }
        |> Rule.fromProjectRuleSchema



--- ELM.JSON


elmJsonVisitor : List String -> Maybe { elmJsonKey : Rule.ElmJsonKey, project : Project } -> () -> ( List (Rule.Error scope), () )
elmJsonVisitor words maybeElmJson () =
    case maybeElmJson of
        Nothing ->
            ( [], () )

        Just elmJson ->
            ( checkElmJson words elmJson, () )


checkElmJson : List String -> { elmJsonKey : Rule.ElmJsonKey, project : Project } -> List (Rule.Error scope)
checkElmJson words { elmJsonKey, project } =
    case project of
        Project.Application _ ->
            []

        Project.Package info ->
            fastConcatMap (checkElmJsonSummary elmJsonKey info.summary) words


checkElmJsonSummary : Rule.ElmJsonKey -> String -> String -> List (Rule.Error scope)
checkElmJsonSummary elmJsonKey summary word =
    summary
        |> stringNode
        |> ranges word
        |> List.map (elmJsonSummaryError elmJsonKey word)


elmJsonSummaryError : Rule.ElmJsonKey -> String -> Range -> Rule.Error scope
elmJsonSummaryError elmJsonKey word rangeInSummary =
    rawElmJsonSummaryError word rangeInSummary
        |> Rule.errorForElmJson elmJsonKey


rawElmJsonSummaryError : String -> Range -> String -> { message : String, details : List String, range : Range }
rawElmJsonSummaryError word rangeInSummary rawElmJson =
    { message = "`" ++ word ++ "` is not allowed in elm.json summary."
    , details =
        [ "You should review your elm.json and make sure the forbidden word has been removed before publishing your code."
        ]
    , range = rawElmJsonSummaryRange rangeInSummary rawElmJson
    }


rawElmJsonSummaryRange : Range -> String -> Range
rawElmJsonSummaryRange rangeInSummary rawElmJson =
    rawElmJson
        |> jsonFieldLocation "summary"
        |> Maybe.map (rangeAddLocation rangeInSummary)
        |> Maybe.withDefault startRange



--- README


readmeVisitor : List String -> Maybe { readmeKey : Rule.ReadmeKey, content : String } -> () -> ( List (Rule.Error scope), () )
readmeVisitor words maybeReadme () =
    case maybeReadme of
        Nothing ->
            ( [], () )

        Just readme ->
            ( fastConcatMap (checkForbiddenReadmeWord readme) words
            , ()
            )


checkForbiddenReadmeWord : { readmeKey : Rule.ReadmeKey, content : String } -> String -> List (Rule.Error scope)
checkForbiddenReadmeWord { readmeKey, content } word =
    content
        |> stringNode
        |> ranges word
        |> List.map (forbiddenReadmeWordError readmeKey word)


forbiddenReadmeWordError : Rule.ReadmeKey -> String -> Range -> Rule.Error scope
forbiddenReadmeWordError readmeKey word range =
    Rule.errorForReadme readmeKey
        { message = "`" ++ word ++ "` is not allowed in README file."
        , details =
            [ "You should review this section and make sure the forbidden word has been removed before publishing your code."
            ]
        }
        range



--- MODULE


moduleVisitor :
    List String
    -> Rule.ModuleRuleSchema {} ()
    -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ()
moduleVisitor words schema =
    schema
        |> Rule.withSimpleCommentsVisitor (commentsVisitor words)
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor words)


commentsVisitor : List String -> List (Node String) -> List (Rule.Error {})
commentsVisitor words comments =
    fastConcatMap (commentVisitor words) comments


declarationVisitor : List String -> Node Declaration -> List (Rule.Error {})
declarationVisitor words (Node _ declaration) =
    case declaration of
        Declaration.FunctionDeclaration { documentation } ->
            documentation
                |> Maybe.map (commentVisitor words)
                |> Maybe.withDefault []

        Declaration.CustomTypeDeclaration { documentation } ->
            documentation
                |> Maybe.map (commentVisitor words)
                |> Maybe.withDefault []

        Declaration.AliasDeclaration { documentation } ->
            documentation
                |> Maybe.map (commentVisitor words)
                |> Maybe.withDefault []

        _ ->
            []


commentVisitor : List String -> Node String -> List (Rule.Error {})
commentVisitor words comment =
    fastConcatMap (checkForbiddenWord comment) words


checkForbiddenWord : Node String -> String -> List (Rule.Error {})
checkForbiddenWord comment word =
    ranges word comment
        |> List.map (forbiddenWordError word)



--- HELPERS


stringNode : String -> Node String
stringNode string =
    Node startRange string


startRange : Range
startRange =
    { start = { row = 1, column = 1 }
    , end = { row = 1, column = 1 }
    }


rangeAddLocation : Range -> Range.Location -> Range
rangeAddLocation range start =
    { start =
        { row = start.row + range.start.row - 1
        , column = start.column + range.start.column - 1
        }
    , end =
        { row = start.row + range.end.row - 1
        , column = start.column + range.end.column - 1
        }
    }


ranges : String -> Node String -> List Range
ranges needle (Node range haystack) =
    String.lines haystack
        |> List.indexedMap (rangesInLine needle range.start)
        |> fastConcat


rangesInLine : String -> Range.Location -> Int -> String -> List Range
rangesInLine needle start row line =
    String.indexes needle line
        |> List.map (rangeFromIndex needle start row)


rangeFromIndex : String -> Range.Location -> Int -> Int -> Range
rangeFromIndex needle start row index =
    case row of
        0 ->
            { start =
                { row = start.row
                , column = start.column + index
                }
            , end =
                { row = start.row
                , column = start.column + index + String.length needle
                }
            }

        _ ->
            { start =
                { row = start.row + row
                , column = index + 1
                }
            , end =
                { row = start.row + row
                , column = index + 1 + String.length needle
                }
            }


jsonFieldLocation : String -> String -> Maybe Range.Location
jsonFieldLocation fieldName rawJson =
    let
        regex : Regex
        regex =
            jsonFieldRegex fieldName
    in
    String.lines rawJson
        |> List.indexedMap (jsonFieldLocationsInLine regex)
        |> fastConcat
        |> List.head


jsonFieldLocationsInLine : Regex -> Int -> String -> List Range.Location
jsonFieldLocationsInLine regex row line =
    Regex.find regex line
        |> List.map (jsonFieldMatchLocation row)


jsonFieldMatchLocation : Int -> Regex.Match -> Range.Location
jsonFieldMatchLocation row { match, index } =
    case row of
        0 ->
            { row = 1
            , column = 1 + index + String.length match
            }

        _ ->
            { row = row + 1
            , column = index + 1 + String.length match
            }


jsonFieldRegex : String -> Regex
jsonFieldRegex fieldName =
    Regex.fromString ("\"" ++ fieldName ++ "\"\\s?:\\s?\"")
        |> Maybe.withDefault Regex.never


forbiddenWordError : String -> Range -> Rule.Error {}
forbiddenWordError word range =
    Rule.error
        { message = "`" ++ word ++ "` is not allowed in comments."
        , details =
            [ "You should review this comment and make sure the forbidden word has been removed before publishing your code."
            ]
        }
        range



--- List Performance


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn =
    let
        helper : a -> List b -> List b
        helper item acc =
            fn item ++ acc
    in
    List.foldr helper []
