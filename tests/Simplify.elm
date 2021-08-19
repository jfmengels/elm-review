module Simplify exposing
    ( rule
    , Configuration, defaults, ignoreCaseOfForTypes
    )

{-| Reports when an expression can be simplified.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Simplify.rule Simplify.defaults
        ]

@docs rule
@docs Configuration, defaults, ignoreCaseOfForTypes


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules Simplify
```


## Simplifications

Below is the list of all kinds of simplifications this rule applies.


### Booleans

    x || True
    --> True

    x || False
    --> x

    x && True
    --> x

    x && False
    --> False

    not True
    --> False

    x == True
    --> x

    x /= False
    --> x

    not x == not y
    --> x == y

    anything == anything
    --> True

    anything /= anything
    --> False

    { r | a = 1 } == { r | a = 2 }
    --> False

    not (not x)
    --> x

    not >> not
    --> identity


### If expressions

    if True then x else y
    --> x

    if False then x else y
    --> y

    if condition then x else x
    --> x

    if condition then True else False
    --> condition

    if condition then False else True
    --> not condition


### Case expressions

    case condition of
        True -> x
        False -> y
    --> if condition then x else y

    case condition of
        False -> y
        True -> x
    --> if not condition then x else y

    case value of
        A _ -> x
        B -> x
    --> x


### Record updates

    { a | b = a.b }
    --> a

    { a | b = a.b, c = 1 }
    --> { a | c = 1 }


### Basics functions

    identity x
    --> x

    f >> identity
    --> f

    always x y
    --> x

    f >> always x
    --> always x


### Lambdas

    (\\() -> x) data
    --> x

    (\\() y -> x) data
    --> (\y -> x)

    (\\_ y -> x) data
    --> (\y -> x)


### Operators

    (++) a b
    --> a ++ b


### Numbers

    n + 0
    --> n

    n - 0
    --> n

    0 - n
    --> -n

    n * 1
    --> n

    n * 0
    --> 0

    n / 1
    --> n

    -(-n)
    --> n

    negate >> negate
    --> identity

    negate (negate x)
    --> x


### Strings

    "a" ++ ""
    --> "a"

    String.isEmpty ""
    --> True

    String.isEmpty "a"
    --> False

    String.concat []
    --> ""

    String.join str []
    --> ""

    String.join "" list
    --> String.concat list

    String.length "abc"
    --> 3

    String.repeat n ""
    --> ""

    String.repeat 0 str
    --> ""

    String.repeat 1 str
    --> str

    String.words ""
    --> []

    String.lines ""
    --> []

    String.reverse ""
    --> ""

    String.reverse <| String.reverse x
    --> x


### Maybe

    Maybe.map identity x
    --> x

    Maybe.map f Nothing
    --> Nothing

    Maybe.map f (Just x)
    --> Just (f x)

    MaybeThen f Nothing
    --> Nothing

    Maybe.andThen (always Nothing) x
    --> Nothing

    Maybe.andThen (\a -> Just b) x
    --> Maybe.map (\a -> b) x

    Maybe.andThen f (Just x)
    --> f x

    Maybe.withDefault x Nothing
    --> x

    Maybe.withDefault x (Just y)
    --> y


### Result

    Result.map identity x
    --> x.and

    Result.map f (Err x)
    --> Err x

    Result.map f (Ok x)
    --> Ok (f x)

    Result.andThen f (Err x)
    --> Err x

    Result.andThen f (Ok x)
    --> f x

    Result.andThen (\a -> Ok b) x
    --> Result.map (\a -> b) x

    Result.withDefault x (Err y)
    --> x

    Result.withDefault x (Ok y)
    --> y


### Lists

    a :: []
    --> [ a ]

    a :: [ b ]
    --> [ a, b ]

    [ a ] ++ list
    --> a :: list

    [] ++ list
    --> list

    [ a, b ] ++ [ c ]
    --> [ a, b, c ]

    [ a, b ] ++ [ c ]
    --> [ a, b, c ]

    List.concat []
    --> []

    List.concat [ [ a, b ], [ c ] ]
    --> [ a, b, c ]

    List.concat [ a, [ 1 ], [ 2 ] ]
    --> List.concat [ a, [ 1, 2 ] ]

    List.concatMap identity x
    --> List.concat list

    List.concatMap identity
    --> List.concat

    List.concatMap (\a -> a) list
    --> List.concat list

    List.concatMap fn [ x ]
    --> fn x

    List.concatMap (always []) list
    --> []

    List.map fn [] -- same for List.filter, List.filterMap, ...
    --> []

    List.map identity list
    --> list

    List.map identity
    --> identity

    List.filter (always True) list
    --> list

    List.filter (\a -> True) list
    --> list

    List.filter (always False) list
    --> []

    List.filter (always True)
    --> identity

    List.filter (always False)
    --> always []

    List.filterMap Just list
    --> list

    List.filterMap (\a -> Just a) list
    --> list

    List.filterMap Just
    --> identity

    List.filterMap (always Nothing) list
    --> []

    List.filterMap (always Nothing)
    --> (always [])

    List.isEmpty []
    --> True

    List.isEmpty [ a ]
    --> False

    List.isEmpty (x :: xs)
    --> False

    List.all fn []
    --> True

    List.all (always True) list
    --> True

    List.any fn []
    --> True

    List.any (always False) list
    --> True

    List.range 6 3
    --> []

    List.length [ a ]
    --> 1

    List.repeat 0 list
    --> []

    List.partition fn []
    --> ( [], [] )

    List.partition (always True) list
    --> ( list, [] )

    List.reverse []
    --> []

    List.reverse <| List.reverse x
    --> x


### Set

    Set.map fn Set.empty -- same for Set.filter, Set.remove...
    --> Set.empty

    Set.map identity list
    --> list

    Set.map identity
    --> identity

    Set.isEmpty Set.empty
    --> True

    Set.member x Set.empty
    --> False

    Set.fromList []
    --> Set.empty

    Set.toList Set.empty
    --> []

    Set.length Set.empty
    --> 0

    Set.intersect Set.empty set
    --> Set.empty

    Set.diff Set.empty set
    --> Set.empty

    Set.diff set Set.empty
    --> set

    Set.union set Set.empty
    --> set

    Set.insert x Set.empty
    --> Set.singleton x

    Set.partition fn Set.empty
    --> ( Set.empty, Set.empty )

    Set.partition (always True) set
    --> ( list, Set.empty )


### Dict

    Dict.isEmpty Dict.empty
    --> True

    Dict.fromList []
    --> Dict.empty

    Dict.toList Dict.empty
    --> []

    Dict.size Dict.empty
    --> 0


### Cmd / Sub

All of these also apply for `Sub`.

    Cmd.batch []
    --> Cmd.none

    Cmd.batch [ a ]
    --> a

    Cmd.batch [ a, Cmd.none, b ]
    --> Cmd.batch [ a, b ]

    Cmd.map identity cmd
    --> cmd

    Cmd.map fn Cmd.none
    --> Cmd.none

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Type
import Json.Decode as Decode
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.Normalize as Normalize


{-| Rule to simplify Elm code.
-}
rule : Configuration -> Rule
rule (Configuration config) =
    case parseTypeNames config.ignoreConstructors of
        Ok [] ->
            Rule.newModuleRuleSchemaUsingContextCreator "Simplify" initialModuleContext
                |> moduleVisitor Set.empty
                |> Rule.fromModuleRuleSchema

        Ok typeNamesList ->
            let
                typeNames : Set ( ModuleName, String )
                typeNames =
                    Set.fromList typeNamesList
            in
            Rule.newProjectRuleSchema "Simplify" initialContext
                |> Rule.withDependenciesProjectVisitor (dependenciesVisitor typeNames)
                |> Rule.withModuleVisitor (moduleVisitor typeNames)
                |> Rule.withModuleContextUsingContextCreator
                    { fromProjectToModule = fromProjectToModule
                    , fromModuleToProject = fromModuleToProject
                    , foldProjectContexts = foldProjectContexts
                    }
                |> Rule.withContextFromImportedModules
                |> Rule.withFinalProjectEvaluation (finalEvaluation config.ignoreConstructors)
                |> Rule.fromProjectRuleSchema

        Err invalidTypes ->
            Rule.configurationError "Simplify"
                { message = "Invalid type names: " ++ (invalidTypes |> List.map wrapInBackticks |> String.join ", ")
                , details =
                    [ "I expect valid type names to be passed to Simplify.ignoreCaseOfForTypes, that include the module name, like `Module.Name.TypeName`."
                    ]
                }


moduleVisitor : Set ( ModuleName, String ) -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor typeNames schema =
    schema
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> addDeclarationListVisitor typeNames


addDeclarationListVisitor : Set ( ModuleName, String ) -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
addDeclarationListVisitor typeNames schema =
    if Set.isEmpty typeNames then
        schema

    else
        Rule.withDeclarationListVisitor (declarationListVisitor typeNames) schema



-- CONFIGURATION


{-| Configuration for this rule. Create a new one with [`defaults`](#defaults) and use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) to alter it.
-}
type Configuration
    = Configuration
        { ignoreConstructors : List String
        }


{-| Default configuration for this rule. Use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) if you want to change the configuration.

    config =
        [ Simplify.defaults
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

-}
defaults : Configuration
defaults =
    Configuration { ignoreConstructors = [] }


{-| Ignore some reports about types used in case expressions.

This rule simplifies the following construct:

    module Module.Name exposing (..)

    type Type = A | B

    case value of
        A -> x
        B -> x
    --> x

In some cases, you may want to disable this simplification because you expect to change or add constructors to this custom type.
Keeping the case expression as it is will make the compiler remind you to update this code when you add new variants, which can be valuable.

Using the following configuration, case of expressions — where all variants of the `Type` custom type
from the `Module.Name` module appear — will not be simplified.

    config =
        [ Simplify.defaults
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

Note that if you use a wildcard, you will still get the simplification, since in this case the compiler will
not remind you anyway.

    case value of
        A -> x
        _ -> x
    --> x

I personally don't recommend to use this function too much, because this could be a sign of premature abstraction, and because
I think that often [You Aren't Gonna Need this code](https://jfmengels.net/safe-dead-code-removal/#yagni-you-arent-gonna-need-it).

Only use it for custom types that you think will change soon. When using it, I recommend not keeping it there too long.
Come back after a while to see if this exception is still worth having. Maybe add a comment with the date and an
explanation next to each exception?

-}
ignoreCaseOfForTypes : List String -> Configuration -> Configuration
ignoreCaseOfForTypes ignoreConstructors (Configuration config) =
    Configuration { config | ignoreConstructors = ignoreConstructors ++ config.ignoreConstructors }


parseTypeNames : List String -> Result (List String) (List ( ModuleName, String ))
parseTypeNames strings =
    let
        parsedTypeNames : List (Result String ( ModuleName, String ))
        parsedTypeNames =
            List.map isValidType strings

        invalidTypeNames : List String
        invalidTypeNames =
            List.filterMap
                (\result ->
                    case result of
                        Err typeName ->
                            Just typeName

                        Ok _ ->
                            Nothing
                )
                parsedTypeNames
    in
    if List.isEmpty invalidTypeNames then
        parsedTypeNames
            |> List.filterMap Result.toMaybe
            |> Ok

    else
        Err invalidTypeNames


isValidType : String -> Result String ( ModuleName, String )
isValidType typeAsString =
    case Decode.decodeString Elm.Type.decoder ("\"" ++ typeAsString ++ "\"") of
        Ok (Elm.Type.Type name _) ->
            case List.reverse <| String.split "." name of
                functionName :: moduleName :: restOfModuleName ->
                    Ok ( List.reverse (moduleName :: restOfModuleName), functionName )

                _ ->
                    Err typeAsString

        _ ->
            Err typeAsString



-- CONTEXT


type alias ProjectContext =
    { ignoredCustomTypes : List Constructor
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    , rangesToIgnore : List Range
    , rightSidesOfPlusPlus : List Range
    , ignoredCustomTypes : List Constructor
    , localIgnoredCustomTypes : List Constructor
    , constructorsToIgnore : Set ( ModuleName, String )
    }


type alias Constructor =
    { moduleName : ModuleName
    , name : String
    , constructors : List String
    }


initialContext : ProjectContext
initialContext =
    { ignoredCustomTypes = []
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { ignoredCustomTypes = moduleContext.localIgnoredCustomTypes
            }
        )


initialModuleContext : Rule.ContextCreator () ModuleContext
initialModuleContext =
    Rule.initContextCreator
        (\lookupTable metadata () ->
            { lookupTable = lookupTable
            , moduleName = Rule.moduleNameFromMetadata metadata
            , rangesToIgnore = []
            , rightSidesOfPlusPlus = []
            , localIgnoredCustomTypes = []
            , ignoredCustomTypes = []
            , constructorsToIgnore = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withMetadata


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable metadata projectContext ->
            { lookupTable = lookupTable
            , moduleName = Rule.moduleNameFromMetadata metadata
            , rangesToIgnore = []
            , rightSidesOfPlusPlus = []
            , localIgnoredCustomTypes = []
            , ignoredCustomTypes = projectContext.ignoredCustomTypes
            , constructorsToIgnore = buildConstructorsToIgnore projectContext.ignoredCustomTypes
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withMetadata


buildConstructorsToIgnore : List Constructor -> Set ( ModuleName, String )
buildConstructorsToIgnore constructors =
    constructors
        |> List.concatMap (\c -> List.map (Tuple.pair c.moduleName) c.constructors)
        |> Set.fromList


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { ignoredCustomTypes = newContext.ignoredCustomTypes ++ previousContext.ignoredCustomTypes
    }



-- DEPENDENCIES VISITOR


dependenciesVisitor : Set ( ModuleName, String ) -> Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor typeNames dict _ =
    ( []
    , { ignoredCustomTypes =
            dict
                |> Dict.values
                |> List.concatMap Dependency.modules
                |> List.concatMap
                    (\mod ->
                        let
                            moduleName : ModuleName
                            moduleName =
                                String.split "." mod.name
                        in
                        mod.unions
                            |> List.filter (\{ name } -> Set.member ( moduleName, name ) typeNames)
                            |> List.map
                                (\union ->
                                    { moduleName = moduleName
                                    , name = union.name
                                    , constructors = List.map Tuple.first union.tags
                                    }
                                )
                    )
      }
    )



-- FINAL EVALUATION


finalEvaluation : List String -> ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation ignoreConstructors projectContext =
    let
        list : List String
        list =
            projectContext.ignoredCustomTypes
                |> List.map (\type_ -> String.join "." type_.moduleName ++ "." ++ type_.name)
                |> Set.fromList
                |> Set.diff (Set.fromList ignoreConstructors)
                |> Set.toList
    in
    if List.isEmpty list then
        []

    else
        [ Rule.globalError
            { message = "Could not find type names: " ++ (String.join ", " <| List.map wrapInBackticks list)
            , details =
                [ "I expected to find these custom types in the code or dependencies, but I could not find them."
                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                , "If you find that these types have been moved or renamed, please update your configuration."
                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                ]
            }
        ]


wrapInBackticks : String -> String
wrapInBackticks s =
    "`" ++ s ++ "`"



-- DECLARATION LIST VISITOR


declarationListVisitor : Set ( ModuleName, String ) -> List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor constructorsToIgnore declarations context =
    let
        localIgnoredCustomTypes : List Constructor
        localIgnoredCustomTypes =
            List.filterMap (findCustomTypes constructorsToIgnore context) declarations
    in
    ( []
    , { context
        | localIgnoredCustomTypes = localIgnoredCustomTypes
        , ignoredCustomTypes = localIgnoredCustomTypes ++ context.ignoredCustomTypes
        , constructorsToIgnore = Set.union (buildConstructorsToIgnore localIgnoredCustomTypes) context.constructorsToIgnore
      }
    )


findCustomTypes : Set ( ModuleName, String ) -> ModuleContext -> Node Declaration -> Maybe Constructor
findCustomTypes constructorsToIgnore context node =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            if Set.member ( context.moduleName, Node.value name ) constructorsToIgnore then
                Just
                    { moduleName = context.moduleName
                    , name = Node.value name
                    , constructors = List.map (\constructor -> constructor |> Node.value |> .name |> Node.value) constructors
                    }

            else
                Nothing

        _ ->
            Nothing



-- DECLARATION VISITOR


declarationVisitor : Node a -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor _ context =
    ( [], { context | rangesToIgnore = [], rightSidesOfPlusPlus = [] } )



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node context =
    if List.member (Node.range node) context.rangesToIgnore then
        ( [], context )

    else
        let
            { errors, rangesToIgnore, rightSidesOfPlusPlus } =
                expressionVisitorHelp node context
        in
        ( errors
        , { context
            | rangesToIgnore = rangesToIgnore ++ context.rangesToIgnore
            , rightSidesOfPlusPlus =
                rightSidesOfPlusPlus ++ context.rightSidesOfPlusPlus
          }
        )


errorsAndRangesToIgnore : List (Error {}) -> List Range -> { errors : List (Error {}), rangesToIgnore : List Range, rightSidesOfPlusPlus : List Range }
errorsAndRangesToIgnore errors rangesToIgnore =
    { errors = errors
    , rangesToIgnore = rangesToIgnore
    , rightSidesOfPlusPlus = []
    }


onlyErrors : List (Error {}) -> { errors : List (Error {}), rangesToIgnore : List Range, rightSidesOfPlusPlus : List Range }
onlyErrors errors =
    { errors = errors
    , rangesToIgnore = []
    , rightSidesOfPlusPlus = []
    }


expressionVisitorHelp : Node Expression -> ModuleContext -> { errors : List (Error {}), rangesToIgnore : List Range, rightSidesOfPlusPlus : List Range }
expressionVisitorHelp node context =
    case Node.value node of
        --------------------
        -- FUNCTION CALLS --
        --------------------
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: restOfArguments) ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    onlyErrors
                        (checkFn
                            { lookupTable = context.lookupTable
                            , parentRange = Node.range node
                            , fnRange = fnRange
                            , firstArg = firstArg
                            , secondArg = List.head restOfArguments
                            , usingRightPizza = False
                            }
                        )

                _ ->
                    onlyErrors []

        -------------------
        -- IF EXPRESSION --
        -------------------
        Expression.IfBlock cond trueBranch falseBranch ->
            case getBoolean context.lookupTable cond of
                Just True ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            }
                            (targetIf node)
                            [ Fix.removeRange
                                { start = (Node.range node).start
                                , end = (Node.range trueBranch).start
                                }
                            , Fix.removeRange
                                { start = (Node.range trueBranch).end
                                , end = (Node.range node).end
                                }
                            ]
                        ]

                Just False ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            }
                            (targetIf node)
                            [ Fix.removeRange
                                { start = (Node.range node).start
                                , end = (Node.range falseBranch).start
                                }
                            ]
                        ]

                Nothing ->
                    case ( getBoolean context.lookupTable trueBranch, getBoolean context.lookupTable falseBranch ) of
                        ( Just True, Just False ) ->
                            onlyErrors
                                [ Rule.errorWithFix
                                    { message = "The if expression's value is the same as the condition"
                                    , details = [ "The expression can be replaced by the condition." ]
                                    }
                                    (targetIf node)
                                    [ Fix.removeRange
                                        { start = (Node.range node).start
                                        , end = (Node.range cond).start
                                        }
                                    , Fix.removeRange
                                        { start = (Node.range cond).end
                                        , end = (Node.range node).end
                                        }
                                    ]
                                ]

                        ( Just False, Just True ) ->
                            onlyErrors
                                [ Rule.errorWithFix
                                    { message = "The if expression's value is the inverse of the condition"
                                    , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                                    }
                                    (targetIf node)
                                    [ Fix.replaceRangeBy
                                        { start = (Node.range node).start
                                        , end = (Node.range cond).start
                                        }
                                        "not ("
                                    , Fix.replaceRangeBy
                                        { start = (Node.range cond).end
                                        , end = (Node.range node).end
                                        }
                                        ")"
                                    ]
                                ]

                        _ ->
                            case Normalize.compare context.lookupTable trueBranch falseBranch of
                                Normalize.ConfirmedEquality ->
                                    onlyErrors
                                        [ Rule.errorWithFix
                                            { message = "The values in both branches is the same."
                                            , details = [ "The expression can be replaced by the contents of either branch." ]
                                            }
                                            (targetIf node)
                                            [ Fix.removeRange
                                                { start = (Node.range node).start
                                                , end = (Node.range trueBranch).start
                                                }
                                            , Fix.removeRange
                                                { start = (Node.range trueBranch).end
                                                , end = (Node.range node).end
                                                }
                                            ]
                                        ]

                                Normalize.ConfirmedInequality ->
                                    onlyErrors []

                                Normalize.Unconfirmed ->
                                    onlyErrors []

        -------------------------------
        --  APPLIED LAMBDA FUNCTIONS --
        -------------------------------
        Expression.Application ((Node _ (Expression.ParenthesizedExpression (Node lambdaRange (Expression.LambdaExpression lambda)))) :: firstArgument :: _) ->
            case lambda.args of
                (Node unitRange Pattern.UnitPattern) :: otherPatterns ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            }
                            unitRange
                            (case otherPatterns of
                                [] ->
                                    [ Fix.removeRange { start = lambdaRange.start, end = (Node.range lambda.expression).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]

                                secondPattern :: _ ->
                                    [ Fix.removeRange { start = unitRange.start, end = (Node.range secondPattern).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]
                            )
                        ]

                (Node allRange Pattern.AllPattern) :: otherPatterns ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            }
                            allRange
                            (case otherPatterns of
                                [] ->
                                    [ Fix.removeRange { start = lambdaRange.start, end = (Node.range lambda.expression).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]

                                secondPattern :: _ ->
                                    [ Fix.removeRange { start = allRange.start, end = (Node.range secondPattern).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]
                            )
                        ]

                _ ->
                    onlyErrors []

        -----------------------------------
        -- FULLY APPLIED PREFIX OPERATOR --
        -----------------------------------
        Expression.Application [ Node.Node operatorRange (Expression.PrefixOperator operator), left, right ] ->
            onlyErrors
                [ Rule.errorWithFix
                    { message = "Use the infix form (a + b) over the prefix form ((+) a b)"
                    , details = [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used." ]
                    }
                    operatorRange
                    [ Fix.removeRange { start = operatorRange.start, end = (Node.range left).start }
                    , Fix.insertAt (Node.range right).start (operator ++ " ")
                    ]
                ]

        -------------------
        -- RECORD UPDATE --
        -------------------
        Expression.RecordUpdateExpression variable fields ->
            onlyErrors (removeRecordFields (Node.range node) variable fields)

        -------------
        -- CASE OF --
        -------------
        Expression.CaseExpression caseBlock ->
            onlyErrors (caseOfChecks context (Node.range node) caseBlock)

        ----------
        -- (<|) --
        ----------
        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ fnName)) firstArg ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    onlyErrors
                        (checkFn
                            { lookupTable = context.lookupTable
                            , parentRange = Node.range node
                            , fnRange = fnRange
                            , firstArg = firstArg
                            , secondArg = Nothing
                            , usingRightPizza = False
                            }
                        )

                _ ->
                    onlyErrors []

        Expression.OperatorApplication "<|" _ (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: []))) secondArgument ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    errorsAndRangesToIgnore
                        (checkFn
                            { lookupTable = context.lookupTable
                            , parentRange = Node.range node
                            , fnRange = fnRange
                            , firstArg = firstArg
                            , secondArg = Just secondArgument
                            , usingRightPizza = False
                            }
                        )
                        [ applicationRange ]

                _ ->
                    onlyErrors []

        ----------
        -- (|>) --
        ----------
        Expression.OperatorApplication "|>" _ firstArg (Node fnRange (Expression.FunctionOrValue _ fnName)) ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    onlyErrors
                        (checkFn
                            { lookupTable = context.lookupTable
                            , parentRange = Node.range node
                            , fnRange = fnRange
                            , firstArg = firstArg
                            , secondArg = Nothing
                            , usingRightPizza = True
                            }
                        )

                _ ->
                    onlyErrors []

        Expression.OperatorApplication "|>" _ secondArgument (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: []))) ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    errorsAndRangesToIgnore
                        (checkFn
                            { lookupTable = context.lookupTable
                            , parentRange = Node.range node
                            , fnRange = fnRange
                            , firstArg = firstArg
                            , secondArg = Just secondArgument
                            , usingRightPizza = True
                            }
                        )
                        [ applicationRange ]

                _ ->
                    onlyErrors []

        Expression.OperatorApplication ">>" _ left (Node _ (Expression.OperatorApplication ">>" _ right _)) ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = True
                    , parentRange = { start = (Node.range left).start, end = (Node.range right).end }
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication ">>" _ left right ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = True
                    , parentRange = Node.range node
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication "<<" _ (Node _ (Expression.OperatorApplication "<<" _ _ left)) right ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = False
                    , parentRange = { start = (Node.range left).start, end = (Node.range right).end }
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication "<<" _ left right ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = False
                    , parentRange = Node.range node
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication operator _ left right ->
            case Dict.get operator operatorChecks of
                Just checkFn ->
                    { errors =
                        checkFn
                            { lookupTable = context.lookupTable
                            , parentRange = Node.range node
                            , operator = operator
                            , left = left
                            , leftRange = Node.range left
                            , right = right
                            , rightRange = Node.range right
                            , isOnTheRightSideOfPlusPlus = List.member (Node.range node) context.rightSidesOfPlusPlus
                            }
                    , rangesToIgnore = []
                    , rightSidesOfPlusPlus =
                        if operator == "++" then
                            [ Node.range <| removeParens right ]

                        else
                            []
                    }

                Nothing ->
                    onlyErrors []

        Expression.Negation baseExpr ->
            case removeParens baseExpr of
                Node range (Expression.Negation _) ->
                    let
                        doubleNegationRange : Range
                        doubleNegationRange =
                            { start = (Node.range node).start
                            , end = { row = range.start.row, column = range.start.column + 1 }
                            }
                    in
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            }
                            doubleNegationRange
                            [ Fix.replaceRangeBy doubleNegationRange "(" ]
                        ]

                _ ->
                    onlyErrors []

        _ ->
            onlyErrors []


type alias CheckInfo =
    { lookupTable : ModuleNameLookupTable
    , parentRange : Range
    , fnRange : Range
    , firstArg : Node Expression
    , secondArg : Maybe (Node Expression)
    , usingRightPizza : Bool
    }


functionCallChecks : Dict ( ModuleName, String ) (CheckInfo -> List (Error {}))
functionCallChecks =
    Dict.fromList
        [ ( ( [ "Basics" ], "identity" ), basicsIdentityChecks )
        , ( ( [ "Basics" ], "always" ), basicsAlwaysChecks )
        , ( ( [ "Basics" ], "not" ), basicsNotChecks )
        , ( ( [ "Basics" ], "negate" ), basicsNegateChecks )
        , ( ( [ "Maybe" ], "map" ), maybeMapChecks )
        , ( ( [ "Maybe" ], "andThen" ), maybeAndThenChecks )
        , ( ( [ "Maybe" ], "withDefault" ), maybeWithDefaultChecks )
        , ( ( [ "Result" ], "map" ), resultMapChecks )
        , ( ( [ "Result" ], "andThen" ), resultAndThenChecks )
        , ( ( [ "Result" ], "withDefault" ), resultWithDefaultChecks )
        , ( ( [ "List" ], "map" ), collectionMapChecks listCollection )
        , ( ( [ "List" ], "filter" ), collectionFilterChecks listCollection )
        , reportEmptyListSecondArgument ( ( [ "List" ], "filterMap" ), listFilterMapChecks )
        , reportEmptyListFirstArgument ( ( [ "List" ], "concat" ), listConcatChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "concatMap" ), listConcatMapChecks )
        , ( ( [ "List" ], "all" ), listAllChecks )
        , ( ( [ "List" ], "any" ), listAnyChecks )
        , ( ( [ "List" ], "range" ), listRangeChecks )
        , ( ( [ "List" ], "length" ), collectionSizeChecks listCollection )
        , ( ( [ "List" ], "repeat" ), listRepeatChecks )
        , ( ( [ "List" ], "isEmpty" ), collectionIsEmptyChecks listCollection )
        , ( ( [ "List" ], "partition" ), collectionPartitionChecks listCollection )
        , ( ( [ "List" ], "reverse" ), listReverseChecks )
        , ( ( [ "Set" ], "map" ), collectionMapChecks setCollection )
        , ( ( [ "Set" ], "filter" ), collectionFilterChecks setCollection )
        , ( ( [ "Set" ], "remove" ), collectionRemoveChecks setCollection )
        , ( ( [ "Set" ], "isEmpty" ), collectionIsEmptyChecks setCollection )
        , ( ( [ "Set" ], "size" ), collectionSizeChecks setCollection )
        , ( ( [ "Set" ], "member" ), collectionMemberChecks setCollection )
        , ( ( [ "Set" ], "fromList" ), collectionFromListChecks setCollection )
        , ( ( [ "Set" ], "toList" ), collectionToListChecks setCollection )
        , ( ( [ "Set" ], "partition" ), collectionPartitionChecks setCollection )
        , ( ( [ "Set" ], "intersect" ), collectionIntersectChecks setCollection )
        , ( ( [ "Set" ], "diff" ), collectionDiffChecks setCollection )
        , ( ( [ "Set" ], "union" ), collectionUnionChecks setCollection )
        , ( ( [ "Set" ], "insert" ), collectionInsertChecks setCollection )
        , ( ( [ "Dict" ], "isEmpty" ), collectionIsEmptyChecks dictCollection )
        , ( ( [ "Dict" ], "fromList" ), collectionFromListChecks dictCollection )
        , ( ( [ "Dict" ], "toList" ), collectionToListChecks dictCollection )
        , ( ( [ "Dict" ], "size" ), collectionSizeChecks dictCollection )
        , ( ( [ "String" ], "isEmpty" ), stringIsEmptyChecks )
        , ( ( [ "String" ], "concat" ), stringConcatChecks )
        , ( ( [ "String" ], "join" ), stringJoinChecks )
        , ( ( [ "String" ], "length" ), stringLengthChecks )
        , ( ( [ "String" ], "repeat" ), stringRepeatChecks )
        , ( ( [ "String" ], "words" ), stringWordsChecks )
        , ( ( [ "String" ], "lines" ), stringLinesChecks )
        , ( ( [ "String" ], "reverse" ), stringReverseChecks )
        , ( ( [ "Platform", "Cmd" ], "batch" ), subAndCmdBatchChecks "Cmd" )
        , ( ( [ "Platform", "Cmd" ], "map" ), collectionMapChecks cmdCollection )
        , ( ( [ "Platform", "Sub" ], "batch" ), subAndCmdBatchChecks "Sub" )
        , ( ( [ "Platform", "Sub" ], "map" ), collectionMapChecks subCollection )
        ]


type alias OperatorCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , parentRange : Range
    , operator : String
    , left : Node Expression
    , leftRange : Range
    , right : Node Expression
    , rightRange : Range
    , isOnTheRightSideOfPlusPlus : Bool
    }


operatorChecks : Dict String (OperatorCheckInfo -> List (Error {}))
operatorChecks =
    Dict.fromList
        [ ( "+", plusChecks )
        , ( "-", minusChecks )
        , ( "*", multiplyChecks )
        , ( "/", divisionChecks )
        , ( "++", plusplusChecks )
        , ( "::", consChecks )
        , ( "||", orChecks )
        , ( "&&", andChecks )
        , ( "==", equalityChecks True )
        , ( "/=", equalityChecks False )
        , ( "<", comparisonChecks (<) )
        , ( ">", comparisonChecks (>) )
        , ( "<=", comparisonChecks (<=) )
        , ( ">=", comparisonChecks (>=) )
        ]


type alias CompositionCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , fromLeftToRight : Bool
    , parentRange : Range
    , left : Node Expression
    , leftRange : Range
    , right : Node Expression
    , rightRange : Range
    }


firstThatReportsError : List (a -> List (Error {})) -> a -> List (Error {})
firstThatReportsError remainingChecks data =
    case remainingChecks of
        [] ->
            []

        checkFn :: restOfFns ->
            case checkFn data of
                [] ->
                    firstThatReportsError restOfFns data

                errors ->
                    errors


compositionChecks : List (CompositionCheckInfo -> List (Error {}))
compositionChecks =
    [ identityCompositionCheck
    , notNotCompositionCheck
    , negateCompositionCheck
    , alwaysCompositionCheck
    , maybeMapCompositionChecks
    , resultMapCompositionChecks
    ]


removeAlongWithOtherFunctionCheck :
    { message : String, details : List String }
    -> (ModuleNameLookupTable -> Node Expression -> Maybe Range)
    -> CheckInfo
    -> List (Error {})
removeAlongWithOtherFunctionCheck errorMessage secondFunctionCheck checkInfo =
    case Node.value (removeParens checkInfo.firstArg) of
        Expression.Application (secondFn :: firstArgOfSecondCall :: _) ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    [ Rule.errorWithFix
                        errorMessage
                        (Range.combine [ checkInfo.fnRange, secondRange ])
                        [ removeFunctionFromFunctionCall checkInfo
                        , removeFunctionFromFunctionCall
                            { fnRange = Node.range secondFn
                            , firstArg = firstArgOfSecondCall
                            , usingRightPizza = False
                            }
                        ]
                    ]

                Nothing ->
                    []

        Expression.OperatorApplication "|>" _ firstArgOfSecondCall secondFn ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    [ Rule.errorWithFix
                        errorMessage
                        (Range.combine [ checkInfo.fnRange, secondRange ])
                        [ removeFunctionFromFunctionCall checkInfo
                        , removeFunctionFromFunctionCall
                            { fnRange = Node.range secondFn
                            , firstArg = firstArgOfSecondCall
                            , usingRightPizza = True
                            }
                        ]
                    ]

                Nothing ->
                    []

        Expression.OperatorApplication "<|" _ secondFn firstArgOfSecondCall ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    [ Rule.errorWithFix
                        errorMessage
                        (Range.combine [ checkInfo.fnRange, secondRange ])
                        [ removeFunctionFromFunctionCall checkInfo
                        , removeFunctionFromFunctionCall
                            { fnRange = Node.range secondFn
                            , firstArg = firstArgOfSecondCall
                            , usingRightPizza = False
                            }
                        ]
                    ]

                Nothing ->
                    []

        _ ->
            []


plusChecks : OperatorCheckInfo -> List (Error {})
plusChecks { leftRange, rightRange, left, right } =
    findMap
        (\( node, getRange ) ->
            if getUncomputedNumberValue node == Just 0 then
                Just
                    [ Rule.errorWithFix
                        { message = "Unnecessary addition with 0"
                        , details = [ "Adding 0 does not change the value of the number." ]
                        }
                        (getRange ())
                        [ Fix.removeRange (getRange ()) ]
                    ]

            else
                Nothing
        )
        [ ( right, \() -> { start = leftRange.end, end = rightRange.end } )
        , ( left, \() -> { start = leftRange.start, end = rightRange.start } )
        ]
        |> Maybe.withDefault []


minusChecks : OperatorCheckInfo -> List (Error {})
minusChecks { leftRange, rightRange, left, right } =
    if getUncomputedNumberValue right == Just 0 then
        let
            range : Range
            range =
                { start = leftRange.end, end = rightRange.end }
        in
        [ Rule.errorWithFix
            { message = "Unnecessary subtraction with 0"
            , details = [ "Subtracting 0 does not change the value of the number." ]
            }
            range
            [ Fix.removeRange range ]
        ]

    else if getUncomputedNumberValue left == Just 0 then
        let
            range : Range
            range =
                { start = leftRange.start, end = rightRange.start }
        in
        [ Rule.errorWithFix
            { message = "Unnecessary subtracting from 0"
            , details = [ "You can negate the expression on the right like `-n`." ]
            }
            range
            [ Fix.replaceRangeBy range "-" ]
        ]

    else
        []


multiplyChecks : OperatorCheckInfo -> List (Error {})
multiplyChecks { parentRange, leftRange, rightRange, left, right } =
    findMap
        (\( node, getRange ) ->
            case getUncomputedNumberValue node of
                Just value ->
                    if value == 1 then
                        Just
                            [ Rule.errorWithFix
                                { message = "Unnecessary multiplication by 1"
                                , details = [ "Multiplying by 1 does not change the value of the number." ]
                                }
                                (getRange ())
                                [ Fix.removeRange (getRange ()) ]
                            ]

                    else if value == 0 then
                        Just
                            [ Rule.errorWithFix
                                { message = "Multiplying by 0 equals 0"
                                , details = [ "You can replace this value by 0." ]
                                }
                                (getRange ())
                                [ Fix.replaceRangeBy parentRange "0" ]
                            ]

                    else
                        Nothing

                _ ->
                    Nothing
        )
        [ ( right, \() -> { start = leftRange.end, end = rightRange.end } )
        , ( left, \() -> { start = leftRange.start, end = rightRange.start } )
        ]
        |> Maybe.withDefault []


divisionChecks : OperatorCheckInfo -> List (Error {})
divisionChecks { leftRange, rightRange, right } =
    if getUncomputedNumberValue right == Just 1 then
        let
            range : Range
            range =
                { start = leftRange.end, end = rightRange.end }
        in
        [ Rule.errorWithFix
            { message = "Unnecessary division by 1"
            , details = [ "Dividing by 1 does not change the value of the number." ]
            }
            range
            [ Fix.removeRange range ]
        ]

    else
        []


find : (a -> Bool) -> List a -> Maybe a
find predicate nodes =
    case nodes of
        [] ->
            Nothing

        node :: rest ->
            if predicate node then
                Just node

            else
                find predicate rest


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap mapper nodes =
    case nodes of
        [] ->
            Nothing

        node :: rest ->
            case mapper node of
                Just value ->
                    Just value

                Nothing ->
                    findMap mapper rest


plusplusChecks : OperatorCheckInfo -> List (Error {})
plusplusChecks { parentRange, leftRange, rightRange, left, right, isOnTheRightSideOfPlusPlus } =
    case ( Node.value left, Node.value right ) of
        ( Expression.Literal "", _ ) ->
            [ errorForAddingEmptyStrings leftRange
                { start = leftRange.start
                , end = rightRange.start
                }
            ]

        ( _, Expression.Literal "" ) ->
            [ errorForAddingEmptyStrings rightRange
                { start = leftRange.end
                , end = rightRange.end
                }
            ]

        ( Expression.ListExpr [], _ ) ->
            [ errorForAddingEmptyLists leftRange
                { start = leftRange.start
                , end = rightRange.start
                }
            ]

        ( _, Expression.ListExpr [] ) ->
            [ errorForAddingEmptyLists rightRange
                { start = leftRange.end
                , end = rightRange.end
                }
            ]

        ( Expression.ListExpr _, Expression.ListExpr _ ) ->
            [ Rule.errorWithFix
                { message = "Expression could be simplified to be a single List"
                , details = [ "Try moving all the elements into a single list." ]
                }
                parentRange
                [ Fix.replaceRangeBy
                    { start = { row = leftRange.end.row, column = leftRange.end.column - 1 }
                    , end = { row = rightRange.start.row, column = rightRange.start.column + 1 }
                    }
                    ","
                ]
            ]

        ( Expression.ListExpr [ _ ], _ ) ->
            if isOnTheRightSideOfPlusPlus then
                []

            else
                [ Rule.errorWithFix
                    { message = "Should use (::) instead of (++)"
                    , details = [ "Concatenating a list with a single value is the same as using (::) on the list with the value." ]
                    }
                    parentRange
                    [ Fix.replaceRangeBy
                        { start = leftRange.start
                        , end = { row = leftRange.start.row, column = leftRange.start.column + 1 }
                        }
                        "("
                    , Fix.replaceRangeBy
                        { start = { row = leftRange.end.row, column = leftRange.end.column - 1 }
                        , end = rightRange.start
                        }
                        ") :: "
                    ]
                ]

        _ ->
            []


errorForAddingEmptyStrings : Range -> Range -> Error {}
errorForAddingEmptyStrings range rangeToRemove =
    Rule.errorWithFix
        { message = "Unnecessary concatenation with an empty string"
        , details = [ "You should remove the concatenation with the empty string." ]
        }
        range
        [ Fix.removeRange rangeToRemove ]


errorForAddingEmptyLists : Range -> Range -> Error {}
errorForAddingEmptyLists range rangeToRemove =
    Rule.errorWithFix
        { message = "Concatenating with a single list doesn't have any effect"
        , details = [ "You should remove the concatenation with the empty list." ]
        }
        range
        [ Fix.removeRange rangeToRemove ]


consChecks : OperatorCheckInfo -> List (Error {})
consChecks { right, leftRange, rightRange } =
    case Node.value right of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Element added to the beginning of the list could be included in the list"
                , details = [ "Try moving the element inside the list it is being added to." ]
                }
                leftRange
                [ Fix.insertAt leftRange.start "[ "
                , Fix.replaceRangeBy
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                    " ]"
                ]
            ]

        Expression.ListExpr _ ->
            [ Rule.errorWithFix
                { message = "Element added to the beginning of the list could be included in the list"
                , details = [ "Try moving the element inside the list it is being added to." ]
                }
                leftRange
                [ Fix.insertAt leftRange.start "[ "
                , Fix.replaceRangeBy
                    { start = leftRange.end
                    , end = { row = rightRange.start.row, column = rightRange.start.column + 1 }
                    }
                    ","
                ]
            ]

        _ ->
            []



-- NUMBERS


negateNegateCompositionErrorMessage : { message : String, details : List String }
negateNegateCompositionErrorMessage =
    { message = "Unnecessary double negation"
    , details = [ "Composing `negate` with `negate` cancel each other out." ]
    }


negateCompositionCheck : CompositionCheckInfo -> List (Error {})
negateCompositionCheck { lookupTable, fromLeftToRight, parentRange, left, right, leftRange, rightRange } =
    case Maybe.map2 Tuple.pair (getNegateFunction lookupTable left) (getNegateFunction lookupTable right) of
        Just _ ->
            [ Rule.errorWithFix
                negateNegateCompositionErrorMessage
                parentRange
                [ Fix.replaceRangeBy parentRange "identity" ]
            ]

        _ ->
            case getNegateFunction lookupTable left of
                Just leftNotRange ->
                    case getNegateComposition lookupTable fromLeftToRight right of
                        Just rightNotRange ->
                            [ Rule.errorWithFix
                                negateNegateCompositionErrorMessage
                                { start = leftNotRange.start, end = rightNotRange.end }
                                [ Fix.removeRange { start = leftNotRange.start, end = rightRange.start }
                                , Fix.removeRange rightNotRange
                                ]
                            ]

                        Nothing ->
                            []

                Nothing ->
                    case getNegateFunction lookupTable right of
                        Just rightNotRange ->
                            case getNegateComposition lookupTable (not fromLeftToRight) left of
                                Just leftNotRange ->
                                    [ Rule.errorWithFix
                                        negateNegateCompositionErrorMessage
                                        { start = leftNotRange.start, end = rightNotRange.end }
                                        [ Fix.removeRange leftNotRange
                                        , Fix.removeRange { start = leftRange.end, end = rightNotRange.end }
                                        ]
                                    ]

                                Nothing ->
                                    []

                        Nothing ->
                            []


getNegateComposition : ModuleNameLookupTable -> Bool -> Node Expression -> Maybe Range
getNegateComposition lookupTable takeFirstFunction node =
    case Node.value (removeParens node) of
        Expression.OperatorApplication "<<" _ left right ->
            if takeFirstFunction then
                getNegateFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

            else
                getNegateFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

        Expression.OperatorApplication ">>" _ left right ->
            if takeFirstFunction then
                getNegateFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

            else
                getNegateFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

        _ ->
            Nothing


basicsNegateChecks : CheckInfo -> List (Error {})
basicsNegateChecks checkInfo =
    removeAlongWithOtherFunctionCheck negateNegateCompositionErrorMessage getNegateFunction checkInfo


getNegateFunction : ModuleNameLookupTable -> Node Expression -> Maybe Range
getNegateFunction lookupTable baseNode =
    case removeParens baseNode of
        Node range (Expression.FunctionOrValue _ "negate") ->
            case ModuleNameLookupTable.moduleNameAt lookupTable range of
                Just [ "Basics" ] ->
                    Just range

                _ ->
                    Nothing

        _ ->
            Nothing



-- BOOLEAN


basicsNotChecks : CheckInfo -> List (Error {})
basicsNotChecks checkInfo =
    case getBoolean checkInfo.lookupTable checkInfo.firstArg of
        Just bool ->
            [ Rule.errorWithFix
                { message = "Expression is equal to " ++ boolToString (not bool)
                , details = [ "You can replace the call to `not` by the boolean value directly." ]
                }
                checkInfo.parentRange
                [ Fix.replaceRangeBy checkInfo.parentRange (boolToString (not bool)) ]
            ]

        Nothing ->
            removeAlongWithOtherFunctionCheck notNotCompositionErrorMessage getNotFunction checkInfo


notNotCompositionCheck : CompositionCheckInfo -> List (Error {})
notNotCompositionCheck { lookupTable, fromLeftToRight, parentRange, left, right, leftRange, rightRange } =
    case Maybe.map2 Tuple.pair (getNotFunction lookupTable left) (getNotFunction lookupTable right) of
        Just _ ->
            [ Rule.errorWithFix
                notNotCompositionErrorMessage
                parentRange
                [ Fix.replaceRangeBy parentRange "identity" ]
            ]

        _ ->
            case getNotFunction lookupTable left of
                Just leftNotRange ->
                    case getNotComposition lookupTable fromLeftToRight right of
                        Just rightNotRange ->
                            [ Rule.errorWithFix
                                notNotCompositionErrorMessage
                                { start = leftNotRange.start, end = rightNotRange.end }
                                [ Fix.removeRange { start = leftNotRange.start, end = rightRange.start }
                                , Fix.removeRange rightNotRange
                                ]
                            ]

                        Nothing ->
                            []

                Nothing ->
                    case getNotFunction lookupTable right of
                        Just rightNotRange ->
                            case getNotComposition lookupTable (not fromLeftToRight) left of
                                Just leftNotRange ->
                                    [ Rule.errorWithFix
                                        notNotCompositionErrorMessage
                                        { start = leftNotRange.start, end = rightNotRange.end }
                                        [ Fix.removeRange leftNotRange
                                        , Fix.removeRange { start = leftRange.end, end = rightNotRange.end }
                                        ]
                                    ]

                                Nothing ->
                                    []

                        Nothing ->
                            []


notNotCompositionErrorMessage : { message : String, details : List String }
notNotCompositionErrorMessage =
    { message = "Unnecessary double negation"
    , details = [ "Composing `not` with `not` cancel each other out." ]
    }


getNotComposition : ModuleNameLookupTable -> Bool -> Node Expression -> Maybe Range
getNotComposition lookupTable takeFirstFunction node =
    case Node.value (removeParens node) of
        Expression.OperatorApplication "<<" _ left right ->
            if takeFirstFunction then
                getNotFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

            else
                getNotFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

        Expression.OperatorApplication ">>" _ left right ->
            if takeFirstFunction then
                getNotFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

            else
                getNotFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

        _ ->
            Nothing


orChecks : OperatorCheckInfo -> List (Error {})
orChecks operatorCheckInfo =
    firstThatReportsError
        [ \() ->
            List.concat
                [ or_isLeftSimplifiableError operatorCheckInfo
                , or_isRightSimplifiableError operatorCheckInfo
                ]
        , \() -> findSimilarConditionsError operatorCheckInfo
        ]
        ()


type RedundantConditionResolution
    = RemoveFrom Location
    | ReplaceByNoop Bool


findSimilarConditionsError : OperatorCheckInfo -> List (Error {})
findSimilarConditionsError operatorCheckInfo =
    let
        conditionsOnTheRight : List ( RedundantConditionResolution, Node Expression )
        conditionsOnTheRight =
            listConditions
                operatorCheckInfo.operator
                (RemoveFrom operatorCheckInfo.leftRange.end)
                operatorCheckInfo.right

        errorsForNode : Node Expression -> List (Error {})
        errorsForNode nodeToCompareTo =
            List.concatMap
                (areSimilarConditionsError
                    operatorCheckInfo.lookupTable
                    operatorCheckInfo.operator
                    nodeToCompareTo
                )
                conditionsOnTheRight
    in
    operatorCheckInfo.left
        |> listConditions operatorCheckInfo.operator (RemoveFrom operatorCheckInfo.leftRange.end)
        |> List.concatMap (Tuple.second >> errorsForNode)


areSimilarConditionsError : ModuleNameLookupTable -> String -> Node Expression -> ( RedundantConditionResolution, Node Expression ) -> List (Error {})
areSimilarConditionsError lookupTable operator nodeToCompareTo ( redundantConditionResolution, nodeToLookAt ) =
    case Normalize.compare lookupTable nodeToCompareTo nodeToLookAt of
        Normalize.ConfirmedEquality ->
            errorForRedundantCondition operator redundantConditionResolution nodeToLookAt

        Normalize.ConfirmedInequality ->
            []

        Normalize.Unconfirmed ->
            []


errorForRedundantCondition : String -> RedundantConditionResolution -> Node a -> List (Error {})
errorForRedundantCondition operator redundantConditionResolution node =
    let
        ( range, fix ) =
            rangeAndFixForRedundantCondition redundantConditionResolution node
    in
    [ Rule.errorWithFix
        { message = "Condition is redundant"
        , details =
            [ "This condition is the same as another one found on the left side of the (" ++ operator ++ ") operator, therefore one of them can be removed."
            ]
        }
        range
        fix
    ]


rangeAndFixForRedundantCondition : RedundantConditionResolution -> Node a -> ( Range, List Fix )
rangeAndFixForRedundantCondition redundantConditionResolution node =
    case redundantConditionResolution of
        RemoveFrom locationOfPrevElement ->
            let
                range : Range
                range =
                    { start = locationOfPrevElement
                    , end = (Node.range node).end
                    }
            in
            ( range
            , [ Fix.removeRange range ]
            )

        ReplaceByNoop noopValue ->
            let
                range : Range
                range =
                    Node.range node
            in
            ( range
            , [ Fix.replaceRangeBy range (boolToString noopValue) ]
            )


listConditions : String -> RedundantConditionResolution -> Node Expression -> List ( RedundantConditionResolution, Node Expression )
listConditions operatorToLookFor redundantConditionResolution node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            let
                noopValue : Bool
                noopValue =
                    operatorToLookFor == "&&"
            in
            listConditions operatorToLookFor (ReplaceByNoop noopValue) expr

        Expression.OperatorApplication operator _ left right ->
            if operator == operatorToLookFor then
                listConditions operatorToLookFor redundantConditionResolution left
                    ++ listConditions operatorToLookFor (RemoveFrom (Node.range left).end) right

            else
                [ ( redundantConditionResolution, node ) ]

        _ ->
            [ ( redundantConditionResolution, node ) ]


or_isLeftSimplifiableError : OperatorCheckInfo -> List (Error {})
or_isLeftSimplifiableError { lookupTable, parentRange, left, leftRange, rightRange } =
    case getBoolean lookupTable left of
        Just True ->
            [ Rule.errorWithFix
                { message = "Condition is always True"
                , details = alwaysSameDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Just False ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Nothing ->
            []


or_isRightSimplifiableError : OperatorCheckInfo -> List (Error {})
or_isRightSimplifiableError { lookupTable, parentRange, right, leftRange, rightRange } =
    case getBoolean lookupTable right of
        Just True ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Just False ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Nothing ->
            []


andChecks : OperatorCheckInfo -> List (Error {})
andChecks operatorCheckInfo =
    firstThatReportsError
        [ \() ->
            List.concat
                [ and_isLeftSimplifiableError operatorCheckInfo
                , and_isRightSimplifiableError operatorCheckInfo
                ]
        , \() -> findSimilarConditionsError operatorCheckInfo
        ]
        ()


and_isLeftSimplifiableError : OperatorCheckInfo -> List (Rule.Error {})
and_isLeftSimplifiableError { lookupTable, parentRange, left, leftRange, rightRange } =
    case getBoolean lookupTable left of
        Just True ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Just False ->
            [ Rule.errorWithFix
                { message = "Condition is always False"
                , details = alwaysSameDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Nothing ->
            []


and_isRightSimplifiableError : OperatorCheckInfo -> List (Rule.Error {})
and_isRightSimplifiableError { lookupTable, parentRange, leftRange, right, rightRange } =
    case getBoolean lookupTable right of
        Just True ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Just False ->
            [ Rule.errorWithFix
                { message = "Condition is always False"
                , details = alwaysSameDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Nothing ->
            []



-- EQUALITY


equalityChecks : Bool -> OperatorCheckInfo -> List (Error {})
equalityChecks isEqual { lookupTable, parentRange, left, right, leftRange, rightRange } =
    if getBoolean lookupTable right == Just isEqual then
        [ Rule.errorWithFix
            { message = "Unnecessary comparison with boolean"
            , details = [ "The result of the expression will be the same with or without the comparison." ]
            }
            parentRange
            [ Fix.removeRange { start = leftRange.end, end = rightRange.end } ]
        ]

    else if getBoolean lookupTable left == Just isEqual then
        [ Rule.errorWithFix
            { message = "Unnecessary comparison with boolean"
            , details = [ "The result of the expression will be the same with or without the comparison." ]
            }
            parentRange
            [ Fix.removeRange { start = leftRange.start, end = rightRange.start } ]
        ]

    else
        case Maybe.map2 Tuple.pair (getNotCall lookupTable left) (getNotCall lookupTable right) of
            Just ( notRangeLeft, notRangeRight ) ->
                [ Rule.errorWithFix
                    { message = "Unnecessary negation on both sides"
                    , details = [ "Since both sides are negated using `not`, they are redundant and can be removed." ]
                    }
                    parentRange
                    [ Fix.removeRange notRangeLeft, Fix.removeRange notRangeRight ]
                ]

            _ ->
                case Normalize.compare lookupTable left right of
                    Normalize.ConfirmedEquality ->
                        [ Rule.errorWithFix
                            { message = "Condition is always " ++ boolToString isEqual
                            , details = sameThingOnBothSidesDetails isEqual
                            }
                            parentRange
                            [ Fix.replaceRangeBy parentRange (boolToString isEqual)
                            ]
                        ]

                    Normalize.ConfirmedInequality ->
                        [ Rule.errorWithFix
                            { message = "Condition is always " ++ boolToString (not isEqual)
                            , details = sameThingOnBothSidesDetails (not isEqual)
                            }
                            parentRange
                            [ Fix.replaceRangeBy parentRange (boolToString (not isEqual))
                            ]
                        ]

                    Normalize.Unconfirmed ->
                        []


getNotCall : ModuleNameLookupTable -> Node Expression -> Maybe Range
getNotCall lookupTable baseNode =
    case Node.value (removeParens baseNode) of
        Expression.Application ((Node notRange (Expression.FunctionOrValue _ "not")) :: _) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable notRange of
                Just [ "Basics" ] ->
                    Just notRange

                _ ->
                    Nothing

        _ ->
            Nothing


getNotFunction : ModuleNameLookupTable -> Node Expression -> Maybe Range
getNotFunction lookupTable baseNode =
    case removeParens baseNode of
        Node notRange (Expression.FunctionOrValue _ "not") ->
            case ModuleNameLookupTable.moduleNameAt lookupTable notRange of
                Just [ "Basics" ] ->
                    Just notRange

                _ ->
                    Nothing

        _ ->
            Nothing


getSpecificFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificFunction ( moduleName, name ) lookupTable baseNode =
    case removeParens baseNode of
        Node notRange (Expression.FunctionOrValue _ foundName) ->
            if
                (foundName == name)
                    && (ModuleNameLookupTable.moduleNameAt lookupTable notRange == Just moduleName)
            then
                Just notRange

            else
                Nothing

        _ ->
            Nothing


alwaysSameDetails : List String
alwaysSameDetails =
    [ "This condition will always result in the same value. You may have hardcoded a value or mistyped a condition."
    ]


unnecessaryMessage : String
unnecessaryMessage =
    "Part of the expression is unnecessary"


unnecessaryDetails : List String
unnecessaryDetails =
    [ "A part of this condition is unnecessary. You can remove it and it would not impact the behavior of the program."
    ]


sameThingOnBothSidesDetails : Bool -> List String
sameThingOnBothSidesDetails computedResult =
    let
        computedResultString : String
        computedResultString =
            if computedResult then
                "True"

            else
                "False"
    in
    [ "The value on the left and on the right are the same. Therefore we can determine that the expression will always be " ++ computedResultString ++ "."
    ]



-- COMPARISONS


comparisonChecks : (Float -> Float -> Bool) -> OperatorCheckInfo -> List (Error {})
comparisonChecks operatorFunction operatorCheckInfo =
    case
        Maybe.map2 operatorFunction
            (Normalize.getNumberValue operatorCheckInfo.left)
            (Normalize.getNumberValue operatorCheckInfo.right)
            |> Maybe.map boolToString
    of
        Just value ->
            [ Rule.errorWithFix
                { message = "Comparison is always " ++ value
                , details =
                    [ "The value on the left and on the right are the same. Therefore we can determine that the expression will always be " ++ value ++ "."
                    ]
                }
                operatorCheckInfo.parentRange
                [ Fix.replaceRangeBy operatorCheckInfo.parentRange value
                ]
            ]

        Nothing ->
            []



-- IF EXPRESSIONS


targetIf : Node a -> Range
targetIf node =
    let
        { start } =
            Node.range node
    in
    { start = start
    , end = { start | column = start.column + 2 }
    }



-- BASICS


basicsIdentityChecks : CheckInfo -> List (Error {})
basicsIdentityChecks checkInfo =
    [ Rule.errorWithFix
        { message = "`identity` should be removed"
        , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
        }
        checkInfo.fnRange
        [ removeFunctionFromFunctionCall checkInfo
        ]
    ]


identityCompositionCheck : CompositionCheckInfo -> List (Error {})
identityCompositionCheck { lookupTable, left, right } =
    if isIdentity lookupTable right then
        [ Rule.errorWithFix
            { message = "`identity` should be removed"
            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
            }
            (Node.range right)
            [ Fix.removeRange { start = (Node.range left).end, end = (Node.range right).end }
            ]
        ]

    else if isIdentity lookupTable left then
        [ Rule.errorWithFix
            { message = "`identity` should be removed"
            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
            }
            (Node.range left)
            [ Fix.removeRange { start = (Node.range left).start, end = (Node.range right).start }
            ]
        ]

    else
        []


basicsAlwaysChecks : CheckInfo -> List (Error {})
basicsAlwaysChecks ({ fnRange, firstArg, secondArg, usingRightPizza } as checkInfo) =
    case secondArg of
        Just (Node secondArgRange _) ->
            [ Rule.errorWithFix
                { message = "Expression can be replaced by the first argument given to `always`"
                , details = [ "The second argument will be ignored because of the `always` call." ]
                }
                fnRange
                (if usingRightPizza then
                    [ Fix.removeRange { start = secondArgRange.start, end = (Node.range firstArg).start }
                    ]

                 else
                    [ removeFunctionFromFunctionCall checkInfo
                    , Fix.removeRange { start = (Node.range firstArg).end, end = secondArgRange.end }
                    ]
                )
            ]

        Nothing ->
            []


alwaysCompositionCheck : CompositionCheckInfo -> List (Error {})
alwaysCompositionCheck { lookupTable, fromLeftToRight, left, right, leftRange, rightRange } =
    if fromLeftToRight then
        if isAlwaysCall lookupTable right then
            [ Rule.errorWithFix
                { message = "Function composed with always will be ignored"
                , details = [ "`always` will swallow the function composed into it." ]
                }
                rightRange
                [ Fix.removeRange { start = leftRange.start, end = rightRange.start } ]
            ]

        else
            []

    else if isAlwaysCall lookupTable left then
        [ Rule.errorWithFix
            { message = "Function composed with always will be ignored"
            , details = [ "`always` will swallow the function composed into it." ]
            }
            leftRange
            [ Fix.removeRange { start = leftRange.end, end = rightRange.end } ]
        ]

    else
        []


isAlwaysCall : ModuleNameLookupTable -> Node Expression -> Bool
isAlwaysCall lookupTable node =
    case Node.value (removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: _ :: []) ->
            ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange == Just [ "Basics" ]

        _ ->
            False


reportEmptyListSecondArgument : ( ( ModuleName, String ), CheckInfo -> List (Error {}) ) -> ( ( ModuleName, String ), CheckInfo -> List (Error {}) )
reportEmptyListSecondArgument ( ( moduleName, name ), function ) =
    ( ( moduleName, name )
    , \checkInfo ->
        case checkInfo.secondArg of
            Just (Node _ (Expression.ListExpr [])) ->
                [ Rule.errorWithFix
                    { message = "Using " ++ String.join "." moduleName ++ "." ++ name ++ " on an empty list will result in a empty list"
                    , details = [ "You can replace this call by an empty list." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )


reportEmptyListFirstArgument : ( ( ModuleName, String ), CheckInfo -> List (Error {}) ) -> ( ( ModuleName, String ), CheckInfo -> List (Error {}) )
reportEmptyListFirstArgument ( ( moduleName, name ), function ) =
    ( ( moduleName, name )
    , \checkInfo ->
        case checkInfo.firstArg of
            Node _ (Expression.ListExpr []) ->
                [ Rule.errorWithFix
                    { message = "Using " ++ String.join "." moduleName ++ "." ++ name ++ " on an empty list will result in a empty list"
                    , details = [ "You can replace this call by an empty list." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )



-- STRING


stringIsEmptyChecks : CheckInfo -> List (Error {})
stringIsEmptyChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal str ->
            let
                replacementValue : String
                replacementValue =
                    boolToString (str == "")
            in
            [ Rule.errorWithFix
                { message = "The call to String.isEmpty will result in " ++ replacementValue
                , details = [ "You can replace this call by " ++ replacementValue ++ "." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange replacementValue ]
            ]

        _ ->
            []


stringConcatChecks : CheckInfo -> List (Error {})
stringConcatChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using String.concat on an empty list will result in a empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "\"\"" ]
            ]

        _ ->
            []


stringWordsChecks : CheckInfo -> List (Error {})
stringWordsChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal "" ->
            [ Rule.errorWithFix
                { message = "Using String.words on an empty string will result in a empty list"
                , details = [ "You can replace this call by an empty list." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            []


stringLinesChecks : CheckInfo -> List (Error {})
stringLinesChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal "" ->
            [ Rule.errorWithFix
                { message = "Using String.lines on an empty string will result in a empty list"
                , details = [ "You can replace this call by an empty list." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            []


stringReverseChecks : CheckInfo -> List (Error {})
stringReverseChecks ({ parentRange, fnRange, firstArg } as checkInfo) =
    case Node.value firstArg of
        Expression.Literal "" ->
            [ Rule.errorWithFix
                { message = "Using String.reverse on an empty string will result in a empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "\"\"" ]
            ]

        _ ->
            removeAlongWithOtherFunctionCheck
                reverseReverseCompositionErrorMessage
                (getSpecificFunction ( [ "String" ], "reverse" ))
                checkInfo


reverseReverseCompositionErrorMessage : { message : String, details : List String }
reverseReverseCompositionErrorMessage =
    { message = "Unnecessary double reversal"
    , details = [ "Composing `reverse` with `reverse` cancel each other out." ]
    }


stringJoinChecks : CheckInfo -> List (Error {})
stringJoinChecks { parentRange, fnRange, firstArg, secondArg } =
    case secondArg of
        Just (Node _ (Expression.ListExpr [])) ->
            [ Rule.errorWithFix
                { message = "Using String.join on an empty list will result in a empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "\"\"" ]
            ]

        _ ->
            case Node.value firstArg of
                Expression.Literal "" ->
                    [ Rule.errorWithFix
                        { message = "Use String.concat instead"
                        , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                        }
                        fnRange
                        [ Fix.replaceRangeBy { start = fnRange.start, end = (Node.range firstArg).end } "String.concat" ]
                    ]

                _ ->
                    []


stringLengthChecks : CheckInfo -> List (Error {})
stringLengthChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal str ->
            [ Rule.errorWithFix
                { message = "The length of the string is " ++ String.fromInt (String.length str)
                , details = [ "The length of the string can be determined by looking at the code." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange (String.fromInt (String.length str)) ]
            ]

        _ ->
            []


stringRepeatChecks : CheckInfo -> List (Error {})
stringRepeatChecks { parentRange, fnRange, firstArg, secondArg } =
    case secondArg of
        Just (Node _ (Expression.Literal "")) ->
            [ Rule.errorWithFix
                { message = "Using String.repeat with an empty string will result in a empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "\"\"" ]
            ]

        _ ->
            case getIntValue firstArg of
                Just intValue ->
                    if intValue == 1 then
                        [ Rule.errorWithFix
                            { message = "String.repeat 1 won't do anything"
                            , details = [ "Using String.repeat with 1 will result in the second argument." ]
                            }
                            fnRange
                            [ Fix.removeRange { start = fnRange.start, end = (Node.range firstArg).end } ]
                        ]

                    else if intValue < 1 then
                        [ Rule.errorWithFix
                            { message = "String.repeat will result in an empty string"
                            , details = [ "Using String.repeat with a number less than 1 will result in an empty string. You can replace this call by an empty string." ]
                            }
                            fnRange
                            (replaceByEmptyFix "\"\"" parentRange secondArg)
                        ]

                    else
                        []

                _ ->
                    []



-- MAYBE FUNCTIONS


maybeMapChecks : CheckInfo -> List (Error {})
maybeMapChecks checkInfo =
    firstThatReportsError
        [ \() -> collectionMapChecks maybeCollection checkInfo
        , \() ->
            case Maybe.andThen (getMaybeValue checkInfo.lookupTable) checkInfo.secondArg of
                Just (Just justRange) ->
                    [ Rule.errorWithFix
                        { message = "Calling Maybe.map on a value that is Just"
                        , details = [ "The function can be called without Maybe.map." ]
                        }
                        checkInfo.fnRange
                        (if checkInfo.usingRightPizza then
                            [ Fix.removeRange justRange
                            , Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            , Fix.insertAt (Node.range checkInfo.firstArg).end " |> Just"
                            ]

                         else
                            [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start } "Just ("
                            , Fix.removeRange justRange
                            , Fix.insertAt checkInfo.parentRange.end ")"
                            ]
                        )
                    ]

                _ ->
                    []
        ]
        ()


maybeMapCompositionChecks : CompositionCheckInfo -> List (Error {})
maybeMapCompositionChecks { lookupTable, fromLeftToRight, parentRange, left, right } =
    if fromLeftToRight then
        case ( removeParens left, Node.value (removeParens right) ) of
            ( Node justRange (Expression.FunctionOrValue _ "Just"), Expression.Application ((Node maybeMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []) ) ->
                if
                    (ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Maybe" ])
                        && (ModuleNameLookupTable.moduleNameAt lookupTable maybeMapRange == Just [ "Maybe" ])
                then
                    [ Rule.errorWithFix
                        { message = "Calling Maybe.map on a value that is Just"
                        , details = [ "The function can be called without Maybe.map." ]
                        }
                        maybeMapRange
                        [ Fix.removeRange { start = parentRange.start, end = (Node.range mapperFunction).start }
                        , Fix.insertAt (Node.range mapperFunction).end " >> Just"
                        ]
                    ]

                else
                    []

            _ ->
                []

    else
        case ( Node.value (removeParens left), removeParens right ) of
            ( Expression.Application ((Node maybeMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []), Node justRange (Expression.FunctionOrValue _ "Just") ) ->
                if ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Maybe" ] && ModuleNameLookupTable.moduleNameAt lookupTable maybeMapRange == Just [ "Maybe" ] then
                    [ Rule.errorWithFix
                        { message = "Calling Maybe.map on a value that is Just"
                        , details = [ "The function can be called without Maybe.map." ]
                        }
                        maybeMapRange
                        [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range mapperFunction).start } "Just << "
                        , Fix.removeRange { start = (Node.range mapperFunction).end, end = parentRange.end }
                        ]
                    ]

                else
                    []

            _ ->
                []


resultMapCompositionChecks : CompositionCheckInfo -> List (Error {})
resultMapCompositionChecks { lookupTable, fromLeftToRight, parentRange, left, right } =
    if fromLeftToRight then
        case ( removeParens left, Node.value (removeParens right) ) of
            ( Node justRange (Expression.FunctionOrValue _ "Ok"), Expression.Application ((Node resultMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []) ) ->
                if
                    (ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Result" ])
                        && (ModuleNameLookupTable.moduleNameAt lookupTable resultMapRange == Just [ "Result" ])
                then
                    [ Rule.errorWithFix
                        { message = "Calling Result.map on a value that is Ok"
                        , details = [ "The function can be called without Result.map." ]
                        }
                        resultMapRange
                        [ Fix.removeRange { start = parentRange.start, end = (Node.range mapperFunction).start }
                        , Fix.insertAt (Node.range mapperFunction).end " >> Ok"
                        ]
                    ]

                else
                    []

            _ ->
                []

    else
        case ( Node.value (removeParens left), removeParens right ) of
            ( Expression.Application ((Node resultMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []), Node justRange (Expression.FunctionOrValue _ "Ok") ) ->
                if ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Result" ] && ModuleNameLookupTable.moduleNameAt lookupTable resultMapRange == Just [ "Result" ] then
                    [ Rule.errorWithFix
                        { message = "Calling Result.map on a value that is Ok"
                        , details = [ "The function can be called without Result.map." ]
                        }
                        resultMapRange
                        [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range mapperFunction).start } "Ok << "
                        , Fix.removeRange { start = (Node.range mapperFunction).end, end = parentRange.end }
                        ]
                    ]

                else
                    []

            _ ->
                []



-- RESULT FUNCTIONS


resultMapChecks : CheckInfo -> List (Error {})
resultMapChecks checkInfo =
    firstThatReportsError
        [ \() -> collectionMapChecks resultCollection checkInfo
        , \() ->
            case Maybe.andThen (getResultValue checkInfo.lookupTable) checkInfo.secondArg of
                Just (Ok okRange) ->
                    [ Rule.errorWithFix
                        { message = "Calling Result.map on a value that is Ok"
                        , details = [ "The function can be called without Result.map." ]
                        }
                        checkInfo.fnRange
                        (if checkInfo.usingRightPizza then
                            [ Fix.removeRange okRange
                            , Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            , Fix.insertAt (Node.range checkInfo.firstArg).end " |> Ok"
                            ]

                         else
                            [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start } "Ok ("
                            , Fix.removeRange okRange
                            , Fix.insertAt checkInfo.parentRange.end ")"
                            ]
                        )
                    ]

                _ ->
                    []
        ]
        ()



-- LIST FUNCTIONS


listConcatChecks : CheckInfo -> List (Error {})
listConcatChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr list ->
            case list of
                [ Node elementRange _ ] ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary use of List.concat on a list with 1 element"
                        , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                        }
                        parentRange
                        [ Fix.removeRange { start = parentRange.start, end = elementRange.start }
                        , Fix.removeRange { start = elementRange.end, end = parentRange.end }
                        ]
                    ]

                (firstListElement :: restOfListElements) as args ->
                    if List.all isListLiteral list then
                        [ Rule.errorWithFix
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            }
                            parentRange
                            (Fix.removeRange fnRange
                                :: List.concatMap removeBoundariesFix args
                            )
                        ]

                    else
                        case findConsecutiveListLiterals firstListElement restOfListElements of
                            [] ->
                                []

                            fixes ->
                                [ Rule.errorWithFix
                                    { message = "Consecutive literal lists should be merged"
                                    , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                                    }
                                    fnRange
                                    fixes
                                ]

                _ ->
                    []

        _ ->
            []


findConsecutiveListLiterals : Node Expression -> List (Node Expression) -> List Fix
findConsecutiveListLiterals firstListElement restOfListElements =
    case ( firstListElement, restOfListElements ) of
        ( Node firstRange (Expression.ListExpr _), ((Node secondRange (Expression.ListExpr _)) as second) :: rest ) ->
            Fix.replaceRangeBy
                { start = { row = firstRange.end.row, column = firstRange.end.column - 1 }
                , end = { row = secondRange.start.row, column = secondRange.start.column + 1 }
                }
                ", "
                :: findConsecutiveListLiterals second rest

        ( _, x :: xs ) ->
            findConsecutiveListLiterals x xs

        _ ->
            []


listConcatMapChecks : CheckInfo -> List (Error {})
listConcatMapChecks { lookupTable, parentRange, fnRange, firstArg, secondArg, usingRightPizza } =
    if isIdentity lookupTable firstArg then
        [ Rule.errorWithFix
            { message = "Using List.concatMap with an identity function is the same as using List.concat"
            , details = [ "You can replace this call by List.concat." ]
            }
            fnRange
            [ Fix.replaceRangeBy { start = fnRange.start, end = (Node.range firstArg).end } "List.concat" ]
        ]

    else if isAlwaysEmptyList lookupTable firstArg then
        [ Rule.errorWithFix
            { message = "List.concatMap will result in on an empty list"
            , details = [ "You can replace this call by an empty list." ]
            }
            fnRange
            (replaceByEmptyFix "[]" parentRange secondArg)
        ]

    else
        case secondArg of
            Just (Node listRange (Expression.ListExpr [ Node singleElementRange _ ])) ->
                [ Rule.errorWithFix
                    { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                    , details = [ "You can replace this call by a call to the function directly." ]
                    }
                    fnRange
                    (if usingRightPizza then
                        [ Fix.replaceRangeBy { start = listRange.start, end = singleElementRange.start } "("
                        , Fix.replaceRangeBy { start = singleElementRange.end, end = listRange.end } ")"
                        , Fix.removeRange fnRange
                        ]

                     else
                        [ Fix.removeRange fnRange
                        , Fix.replaceRangeBy { start = listRange.start, end = singleElementRange.start } "("
                        , Fix.replaceRangeBy { start = singleElementRange.end, end = listRange.end } ")"
                        ]
                    )
                ]

            _ ->
                []


listAllChecks : CheckInfo -> List (Error {})
listAllChecks { lookupTable, parentRange, fnRange, firstArg, secondArg } =
    case Maybe.map (removeParens >> Node.value) secondArg of
        Just (Expression.ListExpr []) ->
            [ Rule.errorWithFix
                { message = "The call to List.all will result in True"
                , details = [ "You can replace this call by True." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "True" ]
            ]

        _ ->
            case isAlwaysBoolean lookupTable firstArg of
                Just True ->
                    [ Rule.errorWithFix
                        { message = "The call to List.all will result in True"
                        , details = [ "You can replace this call by True." ]
                        }
                        fnRange
                        (replaceByBoolFix parentRange secondArg True)
                    ]

                _ ->
                    []


listAnyChecks : CheckInfo -> List (Error {})
listAnyChecks { lookupTable, parentRange, fnRange, firstArg, secondArg } =
    case Maybe.map (removeParens >> Node.value) secondArg of
        Just (Expression.ListExpr []) ->
            [ Rule.errorWithFix
                { message = "The call to List.any will result in False"
                , details = [ "You can replace this call by False." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "False" ]
            ]

        _ ->
            case isAlwaysBoolean lookupTable firstArg of
                Just False ->
                    [ Rule.errorWithFix
                        { message = "The call to List.any will result in False"
                        , details = [ "You can replace this call by False." ]
                        }
                        fnRange
                        (replaceByBoolFix parentRange secondArg False)
                    ]

                _ ->
                    []


listFilterMapChecks : CheckInfo -> List (Error {})
listFilterMapChecks ({ lookupTable, parentRange, fnRange, firstArg, secondArg } as checkInfo) =
    case isAlwaysMaybe lookupTable firstArg of
        Just (Just _) ->
            [ Rule.errorWithFix
                { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                , details = [ "You can remove this call and replace it by the list itself." ]
                }
                fnRange
                (noopFix checkInfo)
            ]

        Just Nothing ->
            [ Rule.errorWithFix
                { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                , details = [ "You can remove this call and replace it by an empty list." ]
                }
                fnRange
                (replaceByEmptyFix "[]" parentRange secondArg)
            ]

        Nothing ->
            []


listRangeChecks : CheckInfo -> List (Error {})
listRangeChecks { parentRange, fnRange, firstArg, secondArg } =
    case Maybe.map2 Tuple.pair (getIntValue firstArg) (Maybe.andThen getIntValue secondArg) of
        Just ( first, second ) ->
            if first > second then
                [ Rule.errorWithFix
                    { message = "The call to List.range will result in []"
                    , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
                    }
                    fnRange
                    (replaceByEmptyFix "[]" parentRange secondArg)
                ]

            else
                []

        Nothing ->
            []


listRepeatChecks : CheckInfo -> List (Error {})
listRepeatChecks { parentRange, fnRange, firstArg, secondArg } =
    case getIntValue firstArg of
        Just intValue ->
            if intValue < 1 then
                [ Rule.errorWithFix
                    { message = "List.repeat will result in an empty list"
                    , details = [ "Using List.repeat with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                    }
                    fnRange
                    (replaceByEmptyFix "[]" parentRange secondArg)
                ]

            else
                []

        _ ->
            []


listReverseChecks : CheckInfo -> List (Error {})
listReverseChecks ({ parentRange, fnRange, firstArg } as checkInfo) =
    case Node.value (removeParens firstArg) of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.reverse on [] will result in []"
                , details = [ "You can replace this call by []." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            removeAlongWithOtherFunctionCheck
                reverseReverseCompositionErrorMessage
                (getSpecificFunction ( [ "List" ], "reverse" ))
                checkInfo


subAndCmdBatchChecks : String -> CheckInfo -> List (Error {})
subAndCmdBatchChecks moduleName { lookupTable, parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Replace by " ++ moduleName ++ ".batch"
                , details = [ moduleName ++ ".batch [] and " ++ moduleName ++ ".none are equivalent but the latter is more idiomatic in Elm code" ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange (moduleName ++ ".none") ]
            ]

        Expression.ListExpr [ arg ] ->
            [ Rule.errorWithFix
                { message = "Unnecessary " ++ moduleName ++ ".batch"
                , details = [ moduleName ++ ".batch with a single element is equal to that element." ]
                }
                fnRange
                [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range arg).start } "("
                , Fix.replaceRangeBy { start = (Node.range arg).end, end = parentRange.end } ")"
                ]
            ]

        Expression.ListExpr args ->
            List.map3 (\a b c -> ( a, b, c ))
                (Nothing :: List.map (Node.range >> Just) args)
                args
                (List.map (Node.range >> Just) (List.drop 1 args) ++ [ Nothing ])
                |> List.filterMap
                    (\( prev, arg, next ) ->
                        case removeParens arg of
                            Node batchRange (Expression.FunctionOrValue _ "none") ->
                                if ModuleNameLookupTable.moduleNameAt lookupTable batchRange == Just [ "Platform", moduleName ] then
                                    Just
                                        (Rule.errorWithFix
                                            { message = "Unnecessary " ++ moduleName ++ ".none"
                                            , details = [ moduleName ++ ".none will be ignored by " ++ moduleName ++ ".batch." ]
                                            }
                                            (Node.range arg)
                                            (case prev of
                                                Just prevRange ->
                                                    [ Fix.removeRange { start = prevRange.end, end = (Node.range arg).end } ]

                                                Nothing ->
                                                    case next of
                                                        Just nextRange ->
                                                            [ Fix.removeRange { start = (Node.range arg).start, end = nextRange.start } ]

                                                        Nothing ->
                                                            [ Fix.replaceRangeBy parentRange (moduleName ++ ".none") ]
                                            )
                                        )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        _ ->
            []


type alias Collection =
    { moduleName : String
    , represents : String
    , emptyAsString : String
    , emptyDescription : String
    , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    , nameForSize : String
    , determineSize : ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
    }


listCollection : Collection
listCollection =
    { moduleName = "List"
    , represents = "list"
    , emptyAsString = "[]"
    , emptyDescription = "[]"
    , isEmpty = \_ -> isEmptyList
    , nameForSize = "length"
    , determineSize = determineListLength
    }


setCollection : Collection
setCollection =
    { moduleName = "Set"
    , represents = "set"
    , emptyAsString = "Set.empty"
    , emptyDescription = "Set.empty"
    , isEmpty = isSpecificFunction [ "Set" ] "empty"
    , nameForSize = "size"
    , determineSize = determineIfCollectionIsEmpty [ "Set" ] 1
    }


dictCollection : Collection
dictCollection =
    { moduleName = "Dict"
    , represents = "Dict"
    , emptyAsString = "Dict.empty"
    , emptyDescription = "Dict.empty"
    , isEmpty = isSpecificFunction [ "Dict" ] "empty"
    , nameForSize = "size"
    , determineSize = determineIfCollectionIsEmpty [ "Dict" ] 2
    }


type alias Mappable =
    { moduleName : String
    , represents : String
    , emptyAsString : String
    , emptyDescription : String
    , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    }


type alias Defaultable =
    { moduleName : String
    , represents : String
    , emptyAsString : String
    , emptyDescription : String
    , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    , isSomethingConstructor : String
    }


maybeCollection : Defaultable
maybeCollection =
    { moduleName = "Maybe"
    , represents = "maybe"
    , emptyAsString = "Nothing"
    , emptyDescription = "Nothing"
    , isEmpty = isSpecificFunction [ "Maybe" ] "Nothing"
    , isSomethingConstructor = "Just"
    }


resultCollection : Defaultable
resultCollection =
    { moduleName = "Result"
    , represents = "result"
    , emptyAsString = "Nothing"
    , emptyDescription = "an error"
    , isEmpty = isSpecificCall [ "Result" ] "Err"
    , isSomethingConstructor = "Ok"
    }


cmdCollection : Mappable
cmdCollection =
    { moduleName = "Cmd"
    , represents = "command"
    , emptyAsString = "Cmd.none"
    , emptyDescription = "Cmd.none"
    , isEmpty = isSpecificFunction [ "Platform", "Cmd" ] "none"
    }


subCollection : Mappable
subCollection =
    { moduleName = "Sub"
    , represents = "subscription"
    , emptyAsString = "Sub.none"
    , emptyDescription = "Sub.none"
    , isEmpty = isSpecificFunction [ "Platform", "Sub" ] "none"
    }


collectionMapChecks :
    { a
        | moduleName : String
        , represents : String
        , emptyDescription : String
        , emptyAsString : String
        , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    }
    -> CheckInfo
    -> List (Error {})
collectionMapChecks collection checkInfo =
    case Maybe.map (collection.isEmpty checkInfo.lookupTable) checkInfo.secondArg of
        Just True ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".map on " ++ collection.emptyDescription ++ " will result in " ++ collection.emptyDescription
                , details = [ "You can replace this call by " ++ collection.emptyDescription ++ "." ]
                }
                checkInfo.fnRange
                (noopFix checkInfo)
            ]

        _ ->
            if isIdentity checkInfo.lookupTable checkInfo.firstArg then
                [ Rule.errorWithFix
                    { message = "Using " ++ collection.moduleName ++ ".map with an identity function is the same as not using " ++ collection.moduleName ++ ".map"
                    , details = [ "You can remove this call and replace it by the " ++ collection.represents ++ " itself." ]
                    }
                    checkInfo.fnRange
                    (noopFix checkInfo)
                ]

            else
                []


maybeAndThenChecks : CheckInfo -> List (Error {})
maybeAndThenChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case Maybe.andThen (getMaybeValue checkInfo.lookupTable) checkInfo.secondArg of
                Just (Just justRange) ->
                    [ Rule.errorWithFix
                        { message = "Calling " ++ maybeCollection.moduleName ++ ".andThen on a value that is known to be Just"
                        , details = [ "You can remove the Just and just call the function directly." ]
                        }
                        checkInfo.fnRange
                        [ Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                        , Fix.removeRange justRange
                        ]
                    ]

                Just Nothing ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ maybeCollection.moduleName ++ ".andThen on " ++ maybeCollection.emptyAsString ++ " will result in " ++ maybeCollection.emptyAsString
                        , details = [ "You can replace this call by " ++ maybeCollection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (noopFix checkInfo)
                    ]

                _ ->
                    []
        , \() ->
            case isAlwaysMaybe checkInfo.lookupTable checkInfo.firstArg of
                Just (Just justRange) ->
                    [ Rule.errorWithFix
                        { message = "Use " ++ maybeCollection.moduleName ++ ".map instead"
                        , details = [ "Using " ++ maybeCollection.moduleName ++ ".andThen with a function that always returns Just is the same thing as using Maybe.map." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.fnRange (maybeCollection.moduleName ++ ".map")
                        , Fix.removeRange justRange
                        ]
                    ]

                Just Nothing ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ maybeCollection.moduleName ++ ".andThen with a function that will always return Nothing will result in Nothing"
                        , details = [ "You can remove this call and replace it by Nothing." ]
                        }
                        checkInfo.fnRange
                        (replaceByEmptyFix maybeCollection.emptyAsString checkInfo.parentRange checkInfo.secondArg)
                    ]

                Nothing ->
                    []
        ]
        ()


resultAndThenChecks : CheckInfo -> List (Error {})
resultAndThenChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case Maybe.andThen (getResultValue checkInfo.lookupTable) checkInfo.secondArg of
                Just (Ok okRange) ->
                    [ Rule.errorWithFix
                        { message = "Calling " ++ resultCollection.moduleName ++ ".andThen on a value that is known to be Ok"
                        , details = [ "You can remove the Ok and just call the function directly." ]
                        }
                        checkInfo.fnRange
                        [ Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                        , Fix.removeRange okRange
                        ]
                    ]

                Just (Err _) ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ resultCollection.moduleName ++ ".andThen on an error will result in the error"
                        , details = [ "You can replace this call by the error itself." ]
                        }
                        checkInfo.fnRange
                        (noopFix checkInfo)
                    ]

                _ ->
                    []
        , \() ->
            case isAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just (Ok okRange) ->
                    [ Rule.errorWithFix
                        { message = "Use " ++ resultCollection.moduleName ++ ".map instead"
                        , details = [ "Using " ++ resultCollection.moduleName ++ ".andThen with a function that always returns Ok is the same thing as using Result.map." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.fnRange (resultCollection.moduleName ++ ".map")
                        , Fix.removeRange okRange
                        ]
                    ]

                _ ->
                    []
        ]
        ()


resultWithDefaultChecks : CheckInfo -> List (Error {})
resultWithDefaultChecks checkInfo =
    case Maybe.andThen (getResultValue checkInfo.lookupTable) checkInfo.secondArg of
        Just (Ok okRange) ->
            [ Rule.errorWithFix
                { message = "Using Result.withDefault on a value that is Ok will result in that value"
                , details = [ "You can replace this call by the value wrapped in Ok." ]
                }
                checkInfo.fnRange
                (Fix.removeRange okRange :: noopFix checkInfo)
            ]

        Just (Err _) ->
            [ Rule.errorWithFix
                { message = "Using Result.withDefault on an error will result in the default value"
                , details = [ "You can replace this call by the default value." ]
                }
                checkInfo.fnRange
                [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                ]
            ]

        Nothing ->
            []


collectionFilterChecks : Collection -> CheckInfo -> List (Error {})
collectionFilterChecks collection ({ lookupTable, parentRange, fnRange, firstArg, secondArg } as checkInfo) =
    case Maybe.andThen (collection.determineSize checkInfo.lookupTable) checkInfo.secondArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".filter on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                }
                checkInfo.fnRange
                (noopFix checkInfo)
            ]

        _ ->
            case isAlwaysBoolean lookupTable firstArg of
                Just True ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".filter with a function that will always return True is the same as not using " ++ collection.moduleName ++ ".filter"
                        , details = [ "You can remove this call and replace it by the " ++ collection.represents ++ " itself." ]
                        }
                        fnRange
                        (noopFix checkInfo)
                    ]

                Just False ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".filter with a function that will always return False will result in " ++ collection.emptyAsString
                        , details = [ "You can remove this call and replace it by " ++ collection.emptyAsString ++ "." ]
                        }
                        fnRange
                        (replaceByEmptyFix collection.emptyAsString parentRange secondArg)
                    ]

                Nothing ->
                    []


collectionRemoveChecks : Collection -> CheckInfo -> List (Error {})
collectionRemoveChecks collection ({ lookupTable, fnRange, secondArg } as checkInfo) =
    case Maybe.andThen (collection.determineSize lookupTable) secondArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".remove on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                }
                fnRange
                (noopFix checkInfo)
            ]

        _ ->
            []


collectionIntersectChecks : Collection -> CheckInfo -> List (Error {})
collectionIntersectChecks collection { lookupTable, parentRange, fnRange, firstArg, secondArg } =
    firstThatReportsError
        [ \() ->
            case collection.determineSize lookupTable firstArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".intersect on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                        , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                        }
                        fnRange
                        (replaceByEmptyFix collection.emptyAsString parentRange secondArg)
                    ]

                _ ->
                    []
        , \() ->
            case Maybe.andThen (collection.determineSize lookupTable) secondArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".intersect on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                        , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                        }
                        fnRange
                        (replaceByEmptyFix collection.emptyAsString parentRange secondArg)
                    ]

                _ ->
                    []
        ]
        ()


collectionDiffChecks : Collection -> CheckInfo -> List (Error {})
collectionDiffChecks collection { lookupTable, parentRange, fnRange, firstArg, secondArg } =
    firstThatReportsError
        [ \() ->
            case collection.determineSize lookupTable firstArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Diffing " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                        , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                        }
                        fnRange
                        (replaceByEmptyFix collection.emptyAsString parentRange secondArg)
                    ]

                _ ->
                    []
        , \() ->
            case Maybe.andThen (collection.determineSize lookupTable) secondArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Diffing a " ++ collection.represents ++ " with " ++ collection.emptyAsString ++ " will result in the " ++ collection.represents ++ " itself"
                        , details = [ "You can replace this call by the " ++ collection.represents ++ " itself." ]
                        }
                        fnRange
                        [ Fix.removeRange { start = parentRange.start, end = (Node.range firstArg).start }
                        , Fix.removeRange { start = (Node.range firstArg).end, end = parentRange.end }
                        ]
                    ]

                _ ->
                    []
        ]
        ()


collectionUnionChecks : Collection -> CheckInfo -> List (Error {})
collectionUnionChecks collection ({ lookupTable, parentRange, fnRange, firstArg, secondArg } as checkInfo) =
    firstThatReportsError
        [ \() ->
            case collection.determineSize lookupTable firstArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary union with Set.empty"
                        , details = [ "You can replace this call by the set itself." ]
                        }
                        fnRange
                        (noopFix checkInfo)
                    ]

                _ ->
                    []
        , \() ->
            case Maybe.andThen (collection.determineSize lookupTable) secondArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary union with Set.empty"
                        , details = [ "You can replace this call by the set itself." ]
                        }
                        fnRange
                        [ Fix.removeRange { start = parentRange.start, end = (Node.range firstArg).start }
                        , Fix.removeRange { start = (Node.range firstArg).end, end = parentRange.end }
                        ]
                    ]

                _ ->
                    []
        ]
        ()


collectionInsertChecks : Collection -> CheckInfo -> List (Error {})
collectionInsertChecks collection { lookupTable, usingRightPizza, parentRange, fnRange, firstArg, secondArg } =
    case Maybe.andThen (collection.determineSize lookupTable) secondArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Use " ++ collection.moduleName ++ ".singleton instead of inserting in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.moduleName ++ ".singleton." ]
                }
                fnRange
                [ Fix.replaceRangeBy fnRange (collection.moduleName ++ ".singleton")
                , if usingRightPizza then
                    Fix.removeRange { start = parentRange.start, end = fnRange.start }

                  else
                    Fix.removeRange { start = (Node.range firstArg).end, end = parentRange.end }
                ]
            ]

        _ ->
            []


collectionMemberChecks : Collection -> CheckInfo -> List (Error {})
collectionMemberChecks collection { lookupTable, parentRange, fnRange, secondArg } =
    case Maybe.andThen (collection.determineSize lookupTable) secondArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".member on " ++ collection.emptyAsString ++ " will result in False"
                , details = [ "You can replace this call by False." ]
                }
                fnRange
                (replaceByBoolFix parentRange secondArg False)
            ]

        _ ->
            []


collectionIsEmptyChecks : Collection -> CheckInfo -> List (Error {})
collectionIsEmptyChecks collection { lookupTable, parentRange, fnRange, firstArg } =
    case collection.determineSize lookupTable firstArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".isEmpty will result in True"
                , details = [ "You can replace this call by True." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "True" ]
            ]

        Just _ ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".isEmpty will result in False"
                , details = [ "You can replace this call by False." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "False" ]
            ]

        Nothing ->
            []


collectionSizeChecks : Collection -> CheckInfo -> List (Error {})
collectionSizeChecks collection { lookupTable, parentRange, fnRange, firstArg } =
    case collection.determineSize lookupTable firstArg of
        Just (Exactly size) ->
            [ Rule.errorWithFix
                { message = "The " ++ collection.nameForSize ++ " of the " ++ collection.represents ++ " is " ++ String.fromInt size
                , details = [ "The " ++ collection.nameForSize ++ " of the " ++ collection.represents ++ " can be determined by looking at the code." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange (String.fromInt size) ]
            ]

        _ ->
            []


collectionFromListChecks : Collection -> CheckInfo -> List (Error {})
collectionFromListChecks collection { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".fromList will result in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange collection.emptyAsString ]
            ]

        _ ->
            []


collectionToListChecks : Collection -> CheckInfo -> List (Error {})
collectionToListChecks collection { lookupTable, parentRange, fnRange, firstArg } =
    case collection.determineSize lookupTable firstArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".toList will result in []"
                , details = [ "You can replace this call by []." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            []


collectionPartitionChecks : Collection -> CheckInfo -> List (Error {})
collectionPartitionChecks collection checkInfo =
    case Maybe.andThen (collection.determineSize checkInfo.lookupTable) checkInfo.secondArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".partition on " ++ collection.emptyAsString ++ " will result in ( " ++ collection.emptyAsString ++ ", " ++ collection.emptyAsString ++ " )"
                , details = [ "You can replace this call by ( " ++ collection.emptyAsString ++ ", " ++ collection.emptyAsString ++ " )." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange ("( " ++ collection.emptyAsString ++ ", " ++ collection.emptyAsString ++ " )") ]
            ]

        _ ->
            case isAlwaysBoolean checkInfo.lookupTable checkInfo.firstArg of
                Just True ->
                    case checkInfo.secondArg of
                        Just listArg ->
                            [ Rule.errorWithFix
                                { message = "All elements will go to the first " ++ collection.represents
                                , details = [ "Since the predicate function always returns True, the second " ++ collection.represents ++ " will always be " ++ collection.emptyAsString ++ "." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range listArg).start } "( "
                                , Fix.insertAt (Node.range listArg).end (", " ++ collection.emptyAsString ++ " )")
                                ]
                            ]

                        Nothing ->
                            []

                Just False ->
                    [ Rule.errorWithFix
                        { message = "All elements will go to the second " ++ collection.represents
                        , details = [ "Since the predicate function always returns False, the first " ++ collection.represents ++ " will always be " ++ collection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (case checkInfo.secondArg of
                            Just listArg ->
                                [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range listArg).start } ("( " ++ collection.emptyAsString ++ ", ")
                                , Fix.insertAt (Node.range listArg).end " )"
                                ]

                            Nothing ->
                                [ Fix.replaceRangeBy checkInfo.parentRange ("(Tuple.pair " ++ collection.emptyAsString ++ ")") ]
                        )
                    ]

                Nothing ->
                    []


maybeWithDefaultChecks : CheckInfo -> List (Error {})
maybeWithDefaultChecks checkInfo =
    case Maybe.andThen (getMaybeValue checkInfo.lookupTable) checkInfo.secondArg of
        Just (Just justRange) ->
            [ Rule.errorWithFix
                { message = "Using Maybe.withDefault on a value that is Just will result in that value"
                , details = [ "You can replace this call by the value wrapped in Just." ]
                }
                checkInfo.fnRange
                (Fix.removeRange justRange :: noopFix checkInfo)
            ]

        Just Nothing ->
            [ Rule.errorWithFix
                { message = "Using Maybe.withDefault on Nothing will result in the default value"
                , details = [ "You can replace this call by the default value." ]
                }
                checkInfo.fnRange
                [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                ]
            ]

        Nothing ->
            []


type CollectionSize
    = Exactly Int
    | NotEmpty


determineListLength : ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
determineListLength lookupTable node =
    case Node.value (removeParens node) of
        Expression.ListExpr list ->
            Just (Exactly (List.length list))

        Expression.OperatorApplication "::" _ _ _ ->
            -- TODO Try to determine the size of the right hand size
            Just NotEmpty

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: _ :: []) ->
            if ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just [ "List" ] then
                Just (Exactly 1)

            else
                Nothing

        _ ->
            Nothing


determineIfCollectionIsEmpty : ModuleName -> Int -> ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
determineIfCollectionIsEmpty moduleName singletonNumberOfArgs lookupTable node =
    if isSpecificFunction moduleName "empty" lookupTable node then
        Just (Exactly 0)

    else
        case Node.value (removeParens node) of
            Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: args) ->
                if List.length args == singletonNumberOfArgs && ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just moduleName then
                    Just (Exactly 1)

                else
                    Nothing

            Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "fromList")) :: (Node _ (Expression.ListExpr list)) :: []) ->
                if ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just moduleName then
                    Just (Exactly (List.length list))

                else
                    Nothing

            _ ->
                Nothing



-- RECORD UPDATE


removeRecordFields : Range -> Node String -> List (Node Expression.RecordSetter) -> List (Error {})
removeRecordFields recordUpdateRange variable fields =
    case fields of
        [] ->
            -- Not possible
            []

        (Node _ ( field, valueWithParens )) :: [] ->
            let
                value : Node Expression
                value =
                    removeParens valueWithParens
            in
            if isUnnecessaryRecordUpdateSetter variable field value then
                [ Rule.errorWithFix
                    { message = "Unnecessary field assignment"
                    , details = [ "The field is being set to its own value." ]
                    }
                    (Node.range value)
                    [ Fix.removeRange { start = recordUpdateRange.start, end = (Node.range variable).start }
                    , Fix.removeRange { start = (Node.range variable).end, end = recordUpdateRange.end }
                    ]
                ]

            else
                []

        (Node firstRange _) :: second :: _ ->
            List.filterMap
                (\( Node range ( field, valueWithParens ), previousRange ) ->
                    let
                        value : Node Expression
                        value =
                            removeParens valueWithParens
                    in
                    if isUnnecessaryRecordUpdateSetter variable field value then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary field assignment"
                                , details = [ "The field is being set to its own value." ]
                                }
                                (Node.range value)
                                (case previousRange of
                                    Just prevRange ->
                                        [ Fix.removeRange { start = prevRange.end, end = range.end } ]

                                    Nothing ->
                                        -- It's the first element, so we can remove until the second element
                                        [ Fix.removeRange { start = firstRange.start, end = (Node.range second).start } ]
                                )
                            )

                    else
                        Nothing
                )
                (List.map2 Tuple.pair fields (Nothing :: List.map (Node.range >> Just) fields))


isUnnecessaryRecordUpdateSetter : Node String -> Node String -> Node Expression -> Bool
isUnnecessaryRecordUpdateSetter variable field value =
    case Node.value value of
        Expression.RecordAccess (Node _ (Expression.FunctionOrValue [] valueHolder)) fieldName ->
            Node.value field == Node.value fieldName && Node.value variable == valueHolder

        _ ->
            False



-- CASE OF


caseOfChecks : ModuleContext -> Range -> Expression.CaseBlock -> List (Error {})
caseOfChecks context parentRange caseBlock =
    firstThatReportsError
        [ \() -> sameBodyForCaseOfChecks context parentRange caseBlock.cases
        , \() -> booleanCaseOfChecks context.lookupTable parentRange caseBlock
        ]
        ()


sameBodyForCaseOfChecks : ModuleContext -> Range -> List ( Node Pattern, Node Expression ) -> List (Error {})
sameBodyForCaseOfChecks context parentRange cases =
    case cases of
        [] ->
            []

        first :: rest ->
            if
                List.any (Tuple.first >> introducesVariable) (first :: rest)
                    || not (Normalize.areAllTheSame context.lookupTable (Tuple.second first) (List.map Tuple.second rest))
            then
                []

            else
                let
                    constructorsUsed : () -> List ( ModuleName, String )
                    constructorsUsed () =
                        List.concatMap (Tuple.first >> findUsedConstructors context) (first :: rest)
                            |> Set.fromList
                            |> Set.toList
                in
                if not (List.isEmpty context.ignoredCustomTypes) && allConstructorsWereUsedOfAType context.ignoredCustomTypes (constructorsUsed ()) then
                    []

                else
                    let
                        firstBodyRange : Range
                        firstBodyRange =
                            Node.range (Tuple.second first)
                    in
                    [ Rule.errorWithFix
                        { message = "Unnecessary case expression"
                        , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                        }
                        (caseKeyWordRange parentRange)
                        [ Fix.removeRange { start = parentRange.start, end = firstBodyRange.start }
                        , Fix.removeRange { start = firstBodyRange.end, end = parentRange.end }
                        ]
                    ]


allConstructorsWereUsedOfAType : List Constructor -> List ( ModuleName, String ) -> Bool
allConstructorsWereUsedOfAType ignoredCustomTypes constructorsUsed =
    case constructorsUsed of
        [] ->
            False

        ( moduleName, constructorName ) :: rest ->
            case find (\type_ -> type_.moduleName == moduleName && List.member constructorName type_.constructors) ignoredCustomTypes of
                Just customType ->
                    List.all
                        (\constructor -> List.member ( moduleName, constructor ) (( moduleName, constructorName ) :: rest))
                        customType.constructors
                        || allConstructorsWereUsedOfAType ignoredCustomTypes rest

                Nothing ->
                    allConstructorsWereUsedOfAType ignoredCustomTypes rest


caseKeyWordRange : Range -> Range
caseKeyWordRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 4 }
    }


introducesVariable : Node Pattern -> Bool
introducesVariable node =
    case Node.value node of
        Pattern.VarPattern _ ->
            True

        Pattern.RecordPattern _ ->
            True

        Pattern.AsPattern _ _ ->
            True

        Pattern.ParenthesizedPattern pattern ->
            introducesVariable pattern

        Pattern.TuplePattern nodes ->
            List.any introducesVariable nodes

        Pattern.UnConsPattern first rest ->
            List.any introducesVariable [ first, rest ]

        Pattern.ListPattern nodes ->
            List.any introducesVariable nodes

        Pattern.NamedPattern _ nodes ->
            List.any introducesVariable nodes

        _ ->
            False


findUsedConstructors : ModuleContext -> Node Pattern -> List ( ModuleName, String )
findUsedConstructors context node =
    case Node.value node of
        Pattern.NamedPattern { name } nodes ->
            case isIgnoredConstructor2 context (Node.range node) name of
                Just moduleName ->
                    ( moduleName, name ) :: List.concatMap (findUsedConstructors context) nodes

                Nothing ->
                    List.concatMap (findUsedConstructors context) nodes

        Pattern.AsPattern pattern _ ->
            findUsedConstructors context pattern

        Pattern.ParenthesizedPattern pattern ->
            findUsedConstructors context pattern

        Pattern.TuplePattern nodes ->
            List.concatMap (findUsedConstructors context) nodes

        Pattern.UnConsPattern first rest ->
            List.concatMap (findUsedConstructors context) [ first, rest ]

        Pattern.ListPattern nodes ->
            List.concatMap (findUsedConstructors context) nodes

        _ ->
            []


isIgnoredConstructor2 : ModuleContext -> Range -> String -> Maybe ModuleName
isIgnoredConstructor2 context range name =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
        Just [] ->
            if Set.member ( context.moduleName, name ) context.constructorsToIgnore then
                Just context.moduleName

            else
                Nothing

        Just moduleName ->
            if Set.member ( moduleName, name ) context.constructorsToIgnore then
                Just moduleName

            else
                Nothing

        Nothing ->
            Nothing


booleanCaseOfChecks : ModuleNameLookupTable -> Range -> Expression.CaseBlock -> List (Error {})
booleanCaseOfChecks lookupTable parentRange { expression, cases } =
    case cases of
        [ ( firstPattern, Node firstRange _ ), ( Node secondPatternRange _, Node secondExprRange _ ) ] ->
            case getBooleanPattern lookupTable firstPattern of
                Just isTrueFirst ->
                    [ Rule.errorWithFix
                        { message = "Replace `case..of` by an `if` condition"
                        , details =
                            [ "The idiomatic way to check for a condition is to use an `if` expression."
                            , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
                            ]
                        }
                        (Node.range firstPattern)
                        (if isTrueFirst then
                            [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range expression).start } "if "
                            , Fix.replaceRangeBy { start = (Node.range expression).end, end = firstRange.start } " then "
                            , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                            ]

                         else
                            [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range expression).start } "if not ("
                            , Fix.replaceRangeBy { start = (Node.range expression).end, end = firstRange.start } ") then "
                            , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                            ]
                        )
                    ]

                _ ->
                    []

        _ ->
            []


isSpecificFunction : ModuleName -> String -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificFunction moduleName fnName lookupTable node =
    case removeParens node of
        Node noneRange (Expression.FunctionOrValue _ foundFnName) ->
            (foundFnName == fnName)
                && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)

        _ ->
            False


isSpecificCall : ModuleName -> String -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificCall moduleName fnName lookupTable node =
    case Node.value (removeParens node) of
        Expression.Application ((Node noneRange (Expression.FunctionOrValue _ foundFnName)) :: _ :: []) ->
            (foundFnName == fnName)
                && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)

        _ ->
            False


getIntValue : Node Expression -> Maybe Int
getIntValue node =
    case Node.value (removeParens node) of
        Expression.Integer n ->
            Just n

        Expression.Hex n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getIntValue expr)

        _ ->
            Nothing


getUncomputedNumberValue : Node Expression -> Maybe Float
getUncomputedNumberValue node =
    case Node.value (removeParens node) of
        Expression.Integer n ->
            Just (toFloat n)

        Expression.Hex n ->
            Just (toFloat n)

        Expression.Floatable n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getUncomputedNumberValue expr)

        _ ->
            Nothing



-- FIX HELPERS


removeFunctionFromFunctionCall : { a | fnRange : Range, firstArg : Node b, usingRightPizza : Bool } -> Fix
removeFunctionFromFunctionCall { fnRange, firstArg, usingRightPizza } =
    if usingRightPizza then
        Fix.removeRange { start = (Node.range firstArg).end, end = fnRange.end }

    else
        Fix.removeRange { start = fnRange.start, end = (Node.range firstArg).start }


removeBoundariesFix : Node a -> List Fix
removeBoundariesFix node =
    let
        { start, end } =
            Node.range node
    in
    [ Fix.removeRange
        { start = { row = start.row, column = start.column }
        , end = { row = start.row, column = start.column + 1 }
        }
    , Fix.removeRange
        { start = { row = end.row, column = end.column - 1 }
        , end = { row = end.row, column = end.column }
        }
    ]


noopFix : CheckInfo -> List Fix
noopFix { fnRange, parentRange, secondArg, usingRightPizza } =
    [ case secondArg of
        Just listArg ->
            if usingRightPizza then
                Fix.removeRange { start = (Node.range listArg).end, end = parentRange.end }

            else
                Fix.removeRange { start = fnRange.start, end = (Node.range listArg).start }

        Nothing ->
            Fix.replaceRangeBy parentRange "identity"
    ]


replaceByEmptyFix : String -> Range -> Maybe a -> List Fix
replaceByEmptyFix empty parentRange secondArg =
    [ case secondArg of
        Just _ ->
            Fix.replaceRangeBy parentRange empty

        Nothing ->
            Fix.replaceRangeBy parentRange ("(always " ++ empty ++ ")")
    ]


replaceByBoolFix : Range -> Maybe a -> Bool -> List Fix
replaceByBoolFix parentRange secondArg replacementValue =
    [ case secondArg of
        Just _ ->
            Fix.replaceRangeBy parentRange (boolToString replacementValue)

        Nothing ->
            Fix.replaceRangeBy parentRange ("(always " ++ boolToString replacementValue ++ ")")
    ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"



-- MATCHERS


isIdentity : ModuleNameLookupTable -> Node Expression -> Bool
isIdentity lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "identity" ->
            ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ]

        Expression.LambdaExpression { args, expression } ->
            case args of
                arg :: [] ->
                    case getVarPattern arg of
                        Just patternName ->
                            case getExpressionName expression of
                                Just expressionName ->
                                    patternName == expressionName

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False

        _ ->
            False


getVarPattern : Node Pattern -> Maybe String
getVarPattern node =
    case Node.value node of
        Pattern.VarPattern name ->
            Just name

        Pattern.ParenthesizedPattern pattern ->
            getVarPattern pattern

        _ ->
            Nothing


getExpressionName : Node Expression -> Maybe String
getExpressionName node =
    case Node.value (removeParens node) of
        Expression.FunctionOrValue [] name ->
            Just name

        _ ->
            Nothing


isListLiteral : Node Expression -> Bool
isListLiteral node =
    case Node.value node of
        Expression.ListExpr _ ->
            True

        _ ->
            False


removeParens : Node Expression -> Node Expression
removeParens node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParens expr

        _ ->
            node


isAlwaysBoolean : ModuleNameLookupTable -> Node Expression -> Maybe Bool
isAlwaysBoolean lookupTable node =
    case Node.value (removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: boolean :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getBoolean lookupTable boolean

                _ ->
                    Nothing

        Expression.LambdaExpression { expression } ->
            getBoolean lookupTable expression

        _ ->
            Nothing


getBoolean : ModuleNameLookupTable -> Node Expression -> Maybe Bool
getBoolean lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "True" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Basics" ] ->
                    Just True

                _ ->
                    Nothing

        Expression.FunctionOrValue _ "False" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Basics" ] ->
                    Just False

                _ ->
                    Nothing

        _ ->
            Nothing


getBooleanPattern : ModuleNameLookupTable -> Node Pattern -> Maybe Bool
getBooleanPattern lookupTable node =
    case Node.value node of
        Pattern.NamedPattern { name } _ ->
            case name of
                "True" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ] then
                        Just True

                    else
                        Nothing

                "False" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ] then
                        Just False

                    else
                        Nothing

                _ ->
                    Nothing

        Pattern.ParenthesizedPattern pattern ->
            getBooleanPattern lookupTable pattern

        _ ->
            Nothing


isAlwaysMaybe : ModuleNameLookupTable -> Node Expression -> Maybe (Maybe Range)
isAlwaysMaybe lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "Just" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Maybe" ] ->
                    Just (Just (Node.range node))

                _ ->
                    Nothing

        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: value :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getMaybeValue lookupTable value

                _ ->
                    Nothing

        Expression.LambdaExpression { args, expression } ->
            case Node.value expression of
                Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Just")) :: (Node argRange (Expression.FunctionOrValue [] justArgName)) :: []) ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                        Just [ "Maybe" ] ->
                            case args of
                                (Node _ (Pattern.VarPattern lambdaArgName)) :: [] ->
                                    if lambdaArgName == justArgName then
                                        Just (Just { start = justRange.start, end = argRange.start })

                                    else
                                        Nothing

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                Expression.FunctionOrValue _ "Nothing" ->
                    case ModuleNameLookupTable.moduleNameFor lookupTable expression of
                        Just [ "Maybe" ] ->
                            Just Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


getMaybeValue : ModuleNameLookupTable -> Node Expression -> Maybe (Maybe Range)
getMaybeValue lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Just")) :: arg :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Maybe" ] ->
                    Just (Just { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        Expression.OperatorApplication "|>" _ arg (Node justRange (Expression.FunctionOrValue _ "Just")) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Maybe" ] ->
                    Just (Just { start = (Node.range arg).end, end = justRange.end })

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ (Node justRange (Expression.FunctionOrValue _ "Just")) arg ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Maybe" ] ->
                    Just (Just { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        Expression.FunctionOrValue _ "Nothing" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Maybe" ] ->
                    Just Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


getResultValue : ModuleNameLookupTable -> Node Expression -> Maybe (Result Range Range)
getResultValue lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Ok")) :: arg :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Ok { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        Expression.OperatorApplication "|>" _ arg (Node justRange (Expression.FunctionOrValue _ "Ok")) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Ok { start = (Node.range arg).end, end = justRange.end })

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ (Node justRange (Expression.FunctionOrValue _ "Ok")) arg ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Ok { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Err")) :: arg :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Err { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        Expression.OperatorApplication "|>" _ arg (Node justRange (Expression.FunctionOrValue _ "Err")) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Err { start = (Node.range arg).end, end = justRange.end })

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ (Node justRange (Expression.FunctionOrValue _ "Err")) arg ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Err { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        _ ->
            Nothing


isAlwaysResult : ModuleNameLookupTable -> Node Expression -> Maybe (Result Range Range)
isAlwaysResult lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "Ok" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Result" ] ->
                    Just (Ok (Node.range node))

                _ ->
                    Nothing

        Expression.FunctionOrValue _ "Err" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Result" ] ->
                    Just (Err (Node.range node))

                _ ->
                    Nothing

        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: value :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getResultValue lookupTable value

                _ ->
                    Nothing

        Expression.LambdaExpression { args, expression } ->
            case Node.value expression of
                Expression.Application ((Node okRange (Expression.FunctionOrValue _ "Ok")) :: (Node argRange (Expression.FunctionOrValue [] justArgName)) :: []) ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable okRange of
                        Just [ "Result" ] ->
                            case args of
                                (Node _ (Pattern.VarPattern lambdaArgName)) :: [] ->
                                    if lambdaArgName == justArgName then
                                        Just (Ok { start = okRange.start, end = argRange.start })

                                    else
                                        Nothing

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                Expression.Application ((Node errRange (Expression.FunctionOrValue _ "Err")) :: (Node argRange (Expression.FunctionOrValue [] justArgName)) :: []) ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable errRange of
                        Just [ "Result" ] ->
                            case args of
                                (Node _ (Pattern.VarPattern lambdaArgName)) :: [] ->
                                    if lambdaArgName == justArgName then
                                        Just (Ok { start = errRange.start, end = argRange.start })

                                    else
                                        Nothing

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


isAlwaysEmptyList : ModuleNameLookupTable -> Node Expression -> Bool
isAlwaysEmptyList lookupTable node =
    case Node.value (removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: alwaysValue :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    isEmptyList alwaysValue

                _ ->
                    False

        Expression.LambdaExpression { expression } ->
            isEmptyList expression

        _ ->
            False


isEmptyList : Node Expression -> Bool
isEmptyList node =
    case Node.value (removeParens node) of
        Expression.ListExpr [] ->
            True

        _ ->
            False
