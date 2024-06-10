module Css.NoUnknownCssClasses exposing
    ( withCssUsingFunctions
    , addKnownClasses, cssFiles, rule
    )

{-| Reports... REPLACEME

    config =
        [ NoUnknownCssClasses.rule
        ]


## Fail

    import Html
    import Html.Attributes

    a =
        Html.span
            [ Html.Attributes.class "unknown" ]
            [ Html.text "Some text" ]


## Success

```css
.red { color: red; }
.bold { font-weight: bold; }
```

    import Html
    import Html.Attributes

    a =
        Html.span
            [ Html.Attributes.class "red bold" ]
            [ Html.text "Some text" ]


## Class module

    module Class exposing (Class, batch, fromString, none, toAttr)

    {-| TODO
    -}

    import Html
    import Html.Attributes

    type Class
        = Class (List String)

    none : Class
    none =
        Class []

    fromString : String -> Class
    fromString class =
        Class [ class ]

    batch : List Class -> Class
    batch classes =
        classes
            |> List.concatMap (\(Class classes) -> classes)
            |> Class

    toAttr : Class -> Html.Attribute never
    toAttr (Class classes) =
        Html.Attributes.class (String.join " " classes)

A nice benefit of using this approach is that if you have an element that accepts arbitrary styling through arguments,
you can pass a `Class` instead of a much more permissive `Html.Attribute msg`.

    import Html
    import Html.Attributes
    import Html.Events

    viewThing : Html.Attribute msg -> String -> Html msg
    viewThing styling text =
        Html.div
            [ Html.id "thing"
            , styling
            ]
            [ Html.text text ]

    exampleView =
        viewThing
            -- ❌ This attribute was misused to become an event handler!
            (Html.Events.onClick ThisIsNotAStylingAttribute)
            "Some text"

You can make sure this doesn't happen using this more restrictive `Class` approach.

    import Class
    import Html

    viewThing : Class -> String -> Html msg
    viewThing styling text =
        Html.div
            [ Html.id "thing"
            , Class.toAttr styling
            ]
            [ Html.text text ]

    exampleView =
        viewThing
            (Class.fromString "red bold")
            "Some text"

TODO Indicate how to configure the rule to greenlight this `Class.fromString`


## Configuring your own class functions

@docs withCssUsingFunctions


## When (not) to enable this rule

This rule is useful in web applications that use plain CSS files for the styling.
Given sufficiently enough information through [`addKnownClasses`](addKnownClasses), it could be useful
for frameworks such as Tailwind.

To work correctly, this rule does need some work from you to avoid having expressions.

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review/example --rules NoUnknownCssClasses
```

-}

import Css.ClassFunction as ClassFunction exposing (CssArgument)
import Css.CssParser
import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Levenshtein
import RangeDict exposing (RangeDict)
import Regex exposing (Regex)
import Review.FilePattern exposing (FilePattern)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Configuration -> Rule
rule (Configuration configuration) =
    Rule.newProjectRuleSchema "Css.NoUnknownClasses" (initialProjectContext configuration.knownClasses)
        |> Rule.withExtraFilesProjectVisitor cssFilesVisitor configuration.cssFiles
        |> Rule.withModuleVisitor (moduleVisitor configuration.cssFunctions)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


type Configuration
    = Configuration
        { knownClasses : Set String
        , cssFiles : List FilePattern
        , cssFunctions : CssFunctions
        }


cssFiles : List FilePattern -> Configuration
cssFiles globs =
    Configuration
        { knownClasses = Set.empty
        , cssFiles = globs
        , cssFunctions = Dict.fromList ClassFunction.baseCssFunctions
        }


cssFilesVisitor : Dict String { fileKey : Rule.ExtraFileKey, content : String } -> ProjectContext -> ( List (Rule.Error scope), ProjectContext )
cssFilesVisitor files context =
    let
        ( errors, knownClasses ) =
            Dict.foldl parseCssFile ( [], context.knownClasses ) files
    in
    ( errors, { knownClasses = knownClasses } )


parseCssFile : String -> { fileKey : Rule.ExtraFileKey, content : String } -> ( List (Rule.Error scope), Set String ) -> ( List (Rule.Error scope), Set String )
parseCssFile filePath file ( errors, knownClasses ) =
    case Css.CssParser.parse file.content of
        Ok cssClasses ->
            ( errors, Set.union cssClasses knownClasses )

        Err _ ->
            ( Rule.errorForExtraFile file.fileKey
                { message = "Unable to parse CSS file `" ++ filePath ++ "`"
                , details = [ "Please check that this file is syntactically correct. It is possible that I'm mistaken as my CSS parser is still very naive. Contributions are welcome to solve the issue." ]
                }
                { start = { row = 1, column = 1 }, end = { row = 1, column = 100000 } }
                :: errors
            , knownClasses
            )


addKnownClasses : List String -> Configuration -> Configuration
addKnownClasses list (Configuration configuration) =
    Configuration { configuration | knownClasses = List.foldl Set.insert configuration.knownClasses list }


{-| By default, only `Html.Attributes.class`, `Html.Attributes.classList` and `Svg.Attributes.class` are used
to find class usages in the application.

You may have your own trusted functions (such as the `Class` module suggested above) that are allowed to take
`String` arguments to be considered as CSS classes. You can use this function along with the
[`Css.ClassFunction` module](#TODO) to explain to the rule which arguments are/contain what should be considered
as CSS classes.

TODO Move this documentation into the `ClassFunction` module

This example is for adding `Class.fromAttr`.

    import Css.ClassFunction
    import Css.NoUnknownClasses


    -- TODO Add missing imports
    config =
        [ Css.NoUnknownClasses.cssFiles whereYourCssFilesAre
            |> Css.NoUnknownClasses.withCssUsingFunctions cssUsingFunctions
            |> Css.NoUnknownClasses.rule
        ]

    cssUsingFunctions : Dict ( ModuleName, String ) (ClassFunction.Arguments -> List ClassFunction.CssArgument)
    cssUsingFunctions =
        Dict.fromList [ ( ( [ "Class" ], "fromString" ), classFromStringFunction ) ]

    classFromStringFunction : ClassFunction.Arguments -> List ClassFunction.CssArgument
    classFromStringFunction { firstArgument } =
        [ ClassFunction.fromLiteral firstArgument ]

Here is how `Html.Attributes.classList` is implemented (Reminder of an example usage: `Html.Attributes.classList [ ("some-class", booleanValue ) ]`):

    import Css.NoUnknownClasses
    import Css.ClassFunction
    -- TODO Add missing imports

    cssUsingFunctions : Dict ( ModuleName, String ) (ClassFunction.Arguments -> List ClassFunction.CssArgument)
    cssUsingFunctions =
        Dict.fromList
            [ ( ( [ "Html", "Attributes" ], "classList" ), \{ firstArgument } -> htmlAttributesClassList firstArgument )
            -- , ...
            ]
        ]


    htmlAttributesClassList : Node Expression -> List ClassFunction.CssArgument
    htmlAttributesClassList node =
        case Node.value node of
            Expression.ListExpr list ->
                List.map
                    (\element ->
                        case Node.value element of
                            Expression.TupledExpression [ first, _ ] ->
                                ClassFunction.fromLiteral first

                            _ ->
                                ClassFunction.Variable (Node.range element)
                    )
                    list

            _ ->
                [ ClassFunction.fromLiteral node ]

-}
withCssUsingFunctions :
    List ( String, ClassFunction.Arguments -> List CssArgument )
    -> Configuration
    -> Configuration
withCssUsingFunctions newFunctions (Configuration configuration) =
    Configuration { configuration | cssFunctions = List.foldl (\( key, fn ) acc -> Dict.insert key fn acc) configuration.cssFunctions newFunctions }


type alias ProjectContext =
    { knownClasses : Set String
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , knownClasses : Set String
    , functionOrValuesToIgnore : RangeDict ()
    }


moduleVisitor : CssFunctions -> Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor cssFunctions schema =
    schema
        |> Rule.withExpressionEnterVisitor (expressionVisitor cssFunctions)


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
            , functionOrValuesToIgnore = RangeDict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\_ ->
            { knownClasses = Set.empty
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    { knownClasses = previous.knownClasses
    }


expressionVisitor : CssFunctions -> Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor cssFunctions node context =
    case Node.value node of
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ name)) :: firstArg :: restOfArguments) ->
            reportClasses cssFunctions context fnRange name firstArg restOfArguments

        Expression.OperatorApplication "|>" _ firstArg (Node fnRange (Expression.FunctionOrValue _ name)) ->
            reportClasses cssFunctions context fnRange name firstArg []

        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ name)) firstArg ->
            reportClasses cssFunctions context fnRange name firstArg []

        Expression.FunctionOrValue _ name ->
            ( reportStrayCssFunction cssFunctions context (Node.range node) name
            , context
            )

        _ ->
            ( [], context )


reportStrayCssFunction : CssFunctions -> ModuleContext -> Range -> String -> List (Rule.Error {})
reportStrayCssFunction cssFunctions context range name =
    if RangeDict.member range context.functionOrValuesToIgnore then
        []

    else
        case
            ModuleNameLookupTable.moduleNameAt context.lookupTable range
                |> Maybe.andThen (\moduleName -> getCssFunction cssFunctions name moduleName)
        of
            Just _ ->
                [ Rule.error
                    { message = "Class using function is used without arguments"
                    , details = [ "Having the function used without arguments confuses me and will prevent me from figuring out whether the classes passed to this function will be known or unknown. Please pass in all the arguments at the location." ]
                    }
                    range
                ]

            Nothing ->
                []


getCssFunction : Dict String v -> String -> List String -> Maybe v
getCssFunction cssFunctions name moduleName =
    Dict.get (String.join "." moduleName ++ "." ++ name) cssFunctions


type alias CssFunctions =
    Dict String (ClassFunction.Arguments -> List CssArgument)


reportClasses : CssFunctions -> ModuleContext -> Range -> String -> Node Expression -> List (Node Expression) -> ( List (Rule.Error {}), ModuleContext )
reportClasses cssFunctions context fnRange name firstArgument restOfArguments =
    case
        ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
            |> Maybe.andThen (\moduleName -> getCssFunction cssFunctions name moduleName)
    of
        Just cssFunction ->
            ( errorsForCssFunction context.knownClasses cssFunction fnRange { lookupTable = context.lookupTable, firstArgument = firstArgument, restOfArguments = restOfArguments }
            , { context | functionOrValuesToIgnore = RangeDict.insert fnRange () context.functionOrValuesToIgnore }
            )

        Nothing ->
            ( [], context )


errorsForCssFunction :
    Set String
    -> (ClassFunction.Arguments -> List CssArgument)
    -> Range
    -> ClassFunction.Arguments
    -> List (Rule.Error {})
errorsForCssFunction knownClasses cssFunction fnRange target =
    cssFunction target
        |> List.concatMap
            (\arg ->
                case arg of
                    ClassFunction.Literal class ->
                        unknownClasses
                            knownClasses
                            (Node.range target.firstArgument)
                            class

                    ClassFunction.Variable range ->
                        [ Rule.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            }
                            range
                        ]

                    ClassFunction.UngraspableExpression range ->
                        [ Rule.error
                            { message = "Non-literal argument to CSS class function"
                            , details = [ "The argument given to this function is not a value that I could interpret. This makes it hard for me to figure out whether this was a known CSS class or not. Please transform this a string literal (\"my-class\")." ]
                            }
                            range
                        ]

                    ClassFunction.MissingArgument _ ->
                        [ Rule.error
                            { message = "Class using function is used without all of its CSS class arguments"
                            , details = [ "Having the function used without all of its arguments confuses me and will prevent me from figuring out whether the classes passed to this function will be known or unknown. Please pass in all the arguments at the location." ]
                            }
                            fnRange
                        ]
            )


reportError : Set String -> Range -> String -> Rule.Error {}
reportError knownClasses range name =
    Rule.error
        { message = "Unknown CSS class \"" ++ name ++ "\""
        , details =
            "I could not find this class in CSS files. Have you made a typo?"
                :: (if Set.isEmpty knownClasses then
                        []

                    else
                        [ String.join "\n"
                            ("Here are similarly-named classes:"
                                :: List.map (\str -> " - " ++ str) (similarClasses name knownClasses)
                            )
                        ]
                   )
        }
        range


similarClasses : String -> Set String -> List String
similarClasses targetClass knownClasses =
    let
        classes : { first : Maybe { class : String, distance : Int }, second : Maybe { class : String, distance : Int } }
        classes =
            Set.foldl
                (\class ({ first, second } as untouched) ->
                    let
                        distance : Int
                        distance =
                            computeDistance class targetClass
                    in
                    if isSmallerDistance distance first then
                        { first = Just { class = class, distance = distance }
                        , second = first
                        }

                    else if isSmallerDistance distance second then
                        { first = first
                        , second = Just { class = class, distance = distance }
                        }

                    else
                        untouched
                )
                { first = Nothing, second = Nothing }
                knownClasses
    in
    List.filterMap (Maybe.map .class) [ classes.first, classes.second ]


isSmallerDistance : Int -> Maybe { a | distance : Int } -> Bool
isSmallerDistance distance maybeElement =
    case maybeElement of
        Just element ->
            distance < element.distance

        Nothing ->
            True


computeDistance : String -> String -> Int
computeDistance a b =
    Basics.min
        (Levenshtein.distance a b)
        (Levenshtein.distance b a)


unknownClasses : Set String -> Range -> String -> List (Rule.Error {})
unknownClasses knownClasses range classesStr =
    unknownClassesHelp
        knownClasses
        range.start.row
        range.start.column
        (Regex.find classesFromLiteralRegex classesStr)
        []


classesFromLiteralRegex : Regex
classesFromLiteralRegex =
    Regex.fromString "([\\w-_]+)"
        |> Maybe.withDefault Regex.never


unknownClassesHelp : Set String -> Int -> Int -> List Regex.Match -> List (Rule.Error {}) -> List (Rule.Error {})
unknownClassesHelp knownClasses row column classes errors =
    case classes of
        [] ->
            errors

        class :: rest ->
            let
                newErrors : List (Rule.Error {})
                newErrors =
                    if Set.member class.match knownClasses then
                        errors

                    else
                        reportError
                            knownClasses
                            { start = { row = row, column = column + class.index + 1 }
                            , end = { row = row, column = column + class.index + String.length class.match + 1 }
                            }
                            class.match
                            :: errors
            in
            unknownClassesHelp
                knownClasses
                row
                column
                rest
                newErrors
