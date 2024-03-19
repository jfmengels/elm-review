module NoUnknownCssClasses exposing
    ( rule
    , defaults, withCssFiles, withHardcodedKnownClasses
    )

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import NoInconsistentAliases exposing (Config)
import Parser exposing ((|.), (|=), Parser)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import String exposing (contains)


{-| Reports... REPLACEME

    config =
        [ NoUnknownCssClasses.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review/example --rules NoUnknownCssClasses
```

-}
rule : Configuration -> Rule
rule (Configuration configuration) =
    Rule.newProjectRuleSchema "NoUnknownCssClasses" (initialProjectContext configuration.knownClasses)
        |> Rule.withExtraFilesProjectVisitor configuration.cssFiles cssFilesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


type Configuration
    = Configuration
        { knownClasses : Set String
        , cssFiles : List String
        }


defaults : Configuration
defaults =
    Configuration
        { knownClasses = Set.empty
        , cssFiles = []
        }


cssFilesVisitor : List { file | content : String } -> ProjectContext -> ( List empty, ProjectContext )
cssFilesVisitor files context =
    ( [], { knownClasses = List.foldl (.content >> parseCssFile) context.knownClasses files } )


withHardcodedKnownClasses : List String -> Configuration -> Configuration
withHardcodedKnownClasses list (Configuration configuration) =
    Configuration { configuration | knownClasses = List.foldl Set.insert configuration.knownClasses list }


withCssFiles : List String -> Configuration -> Configuration
withCssFiles list (Configuration configuration) =
    Configuration { configuration | cssFiles = list ++ configuration.cssFiles }


type alias ProjectContext =
    { knownClasses : Set String
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , knownClasses : Set String
    }


moduleVisitor : Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionVisitor


initialProjectContext : Set String -> ProjectContext
initialProjectContext knownClasses =
    { knownClasses = knownClasses
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , knownClasses = projectContext.knownClasses
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { knownClasses = Set.empty
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    { knownClasses = previous.knownClasses
    }


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ name)) :: firstArg :: _) ->
            case ( ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange, name ) of
                ( Just [ "Html", "Attributes" ], "class" ) ->
                    case Node.value firstArg of
                        Expression.Literal str ->
                            ( unknownClasses
                                context.knownClasses
                                (Node.range firstArg)
                                str
                            , context
                            )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        Expression.OperatorApplication "|>" _ firstArg (Node fnRange (Expression.FunctionOrValue _ name)) ->
            case ( ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange, name ) of
                ( Just [ "Html", "Attributes" ], "class" ) ->
                    case Node.value firstArg of
                        Expression.Literal str ->
                            ( unknownClasses
                                context.knownClasses
                                (Node.range firstArg)
                                str
                            , context
                            )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ name)) firstArg ->
            case ( ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange, name ) of
                ( Just [ "Html", "Attributes" ], "class" ) ->
                    case Node.value firstArg of
                        Expression.Literal str ->
                            ( unknownClasses
                                context.knownClasses
                                (Node.range firstArg)
                                str
                            , context
                            )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )


reportError : Range -> String -> Rule.Error {}
reportError range name =
    Rule.error
        { message = "Unknown CSS class \"" ++ name ++ "\""
        , details = [ "I could not find this class in CSS files. Have you made a typo? Here are similarly-named classes: TODO" ]
        }
        range


unknownClasses : Set String -> Range -> String -> List (Rule.Error {})
unknownClasses knownClasses range str =
    let
        { row, column } =
            range.start
    in
    List.foldl
        (\class ( offset, errors ) ->
            let
                newErrors : List (Rule.Error {})
                newErrors =
                    if Set.member class knownClasses then
                        errors

                    else
                        reportError
                            { start = { row = row, column = column + offset }
                            , end = { row = row, column = column + offset + String.length class }
                            }
                            class
                            :: errors
            in
            ( offset + String.length class + 1, newErrors )
        )
        ( 1, [] )
        (String.split " " str)
        |> Tuple.second



---


parseCssFile : String -> Set String -> Set String
parseCssFile file knownClasses =
    case Parser.run cssParser file of
        Ok cssClasses ->
            Set.union cssClasses knownClasses

        Err _ ->
            -- Create an error?
            knownClasses


cssParser : Parser (Set String)
cssParser =
    Parser.loop Set.empty cssRule


cssRule : Set String -> Parser (Parser.Step (Set String) (Set String))
cssRule acc =
    Parser.oneOf
        [ Parser.succeed (\selector -> Parser.Loop (Set.insert selector acc))
            |. Parser.token "."
            |= (Parser.chompWhile Char.isAlphaNum
                    |> Parser.getChompedString
               )
        , Parser.end
            |> Parser.map (\_ -> Parser.Done acc)
        , Parser.succeed (\() -> Parser.Loop acc)
            |. Parser.token "{"
            |. Parser.chompUntil "}"
            |. Parser.token "}"
            |= Parser.spaces
        , Parser.succeed (\() -> Parser.Loop acc)
            |= Parser.chompIf (always True)
        ]
