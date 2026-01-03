module NoUnused.Parameters exposing (rule)

{-| Report parameters that are not used.

@docs rule

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import List.Extra
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import NoUnused.Parameters.ParameterPath as ParameterPath exposing (Nesting, Path)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (FixV2, ModuleKey, Rule)
import Set exposing (Set)


{-| Report parameters that are not used.

🔧 Running with `--fix` will automatically remove some of the reported errors.

    config =
        [ NoUnused.Parameters.rule
        ]

This rule looks within function arguments, let functions and lambdas to find any values that are unused. It will report any parameters that are not used.


## Fixes for lambdas

We're only offering fixes for lambdas here because we believe unused parameters in functions are a code smell that should be refactored.


## Fail

Value `number` is not used:

    add1 number =
        1

The rule will also report parameters that are only used to be passed again to the containing recursive function:

    last list unused =
        case list of
            [] ->
                Nothing

            [ a ] ->
                Just a

            _ :: rest ->
                last rest unused


## Success

    add1 number =
        number + 1


## Automatic fixes

When possible, the rule will automatically remove function arguments from the declaration and call sites

    -- Before
    someFunction : Int -> Int -> Int
    someFunction used unused =
        used + 1

    two =
        someFunction 1 10

    -- After
    someFunction : Int -> Int
    someFunction used =
        used + 1

    two =
        someFunction 1

Also for fields and tuple values inside parameters, such as:

    -- Before
    someFunction : { unused : Int, used : Int } -> Int
    someFunction { unused, used } =
        otherFunction used

    a =
        someFunction { unused = 1, used = 2 }

    -- After
    someFunction : { used : b } -> b
    someFunction { used } =
        otherFunction used

    a =
        someFunction { used = 2 }

When fixing the calls is not possible, a fix will be made to ignore the argument (by replacing it by `_` for instance.
`_` arguments will only be reported if they're automatically fixable, otherwise they will be ignored.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Parameters
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Parameters" initialContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextWithErrors
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.withContextFromImportedModules
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    let
        exposedModules : Set ModuleName
        exposedModules =
            case maybeProject |> Maybe.map .project of
                Just (Elm.Project.Package { exposed }) ->
                    let
                        exposedModuleNames : List Elm.Module.Name
                        exposedModuleNames =
                            case exposed of
                                Elm.Project.ExposedList names ->
                                    names

                                Elm.Project.ExposedDict fakeDict ->
                                    List.concatMap Tuple.second fakeDict
                    in
                    List.foldl
                        (\moduleName acc ->
                            Set.insert (moduleName |> Elm.Module.toString |> String.split ".") acc
                        )
                        Set.empty
                        exposedModuleNames

                Just (Elm.Project.Application _) ->
                    Set.empty

                Nothing ->
                    Set.empty
    in
    ( [], { projectContext | exposedModules = exposedModules } )


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationEnterVisitor node context ))
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionEnterVisitor node context ))
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withLetDeclarationEnterVisitor (\_ letDeclaration context -> ( [], letDeclarationEnterVisitor letDeclaration context ))
        |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor



--- CONTEXT


type alias ProjectContext =
    { exposedModules : Set ModuleName
    , toReport : Dict ModuleName { key : ModuleKey, args : List ArgumentToReport }
    , functionCallsWithArguments : Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, isFileFixable : Bool, callSites : List CallSite })
    }


type alias CallSite =
    { fnNameEnd : Location
    , arguments : Array (Node Expression)
    }


initialContext : ProjectContext
initialContext =
    { exposedModules = Set.empty
    , toReport = Dict.empty
    , functionCallsWithArguments = Dict.empty
    }


type alias ModuleContext =
    { exposedModules : Set ModuleName
    , lookupTable : ModuleNameLookupTable
    , scopes : Nonempty Scope
    , recursiveFunctions : Dict String FunctionArgs
    , locationsToIgnoreForRecursiveArguments : LocationsToIgnore
    , functionCallsWithArguments : Dict FunctionName (List CallSite)
    , functionCallsWithArgumentsForOtherModules : Dict ( ModuleName, FunctionName ) (List CallSite)
    , locationsToIgnoreFunctionCalls : List Location
    , functionsFromOtherModulesToFix : Set ( ModuleName, FunctionName )
    }


type alias Scope =
    { functionName : FunctionName
    , declared : List Declared
    , functionsDeclaredInSubScope : Set String
    , used : Set String
    , usedRecursively : Set String
    , toReport : List ArgumentToReport
    , locationsToIgnoreForFunctionCalls : List Location
    }


type alias FunctionName =
    String


type alias ArgumentToReport =
    { functionName : FunctionName
    , path : Path
    , details : { message : String, details : List String }
    , range : Range
    , backupWhenFixImpossible : BackupWhenFixImpossible
    }


type BackupWhenFixImpossible
    = ApplyFix (List Fix)
    | ReportButDontFix
    | DontReport


type alias Declared =
    { name : String
    , range : Range
    , kind : Kind
    , source : Source
    , path : Path
    , tryToRemoveArg : Bool
    , toIgnoredFix : List Fix
    , backupWhenFixImpossible : BackupWhenFixImpossible
    }


type alias LocationsToIgnore =
    Dict String (List Range)


type alias FunctionArgs =
    Dict Int String


type Kind
    = Parameter
    | Alias
    | TupleWithoutVariables


type Source
    = NamedFunction
    | Lambda


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContent ->
            { exposedModules = projectContent.exposedModules
            , lookupTable = lookupTable
            , scopes =
                NonemptyList.fromElement
                    { functionName = "root"
                    , declared = []
                    , functionsDeclaredInSubScope = Set.empty
                    , used = Set.empty
                    , usedRecursively = Set.empty
                    , toReport = []
                    , locationsToIgnoreForFunctionCalls = []
                    }
            , recursiveFunctions = Dict.empty
            , locationsToIgnoreForRecursiveArguments = Dict.empty
            , functionCallsWithArguments = Dict.empty
            , functionCallsWithArgumentsForOtherModules = Dict.empty
            , locationsToIgnoreFunctionCalls = []
            , functionsFromOtherModulesToFix =
                Dict.foldl
                    (\moduleName { args } set ->
                        List.foldl (\{ functionName } setAcc -> Set.insert ( moduleName, functionName ) setAcc) set args
                    )
                    Set.empty
                    projectContent.toReport
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ( List (Rule.Error {}), ProjectContext )
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleKey ast isFileIgnored isFileFixable moduleContext ->
            let
                isExposed : String -> Bool
                isExposed =
                    case Module.exposingList (Node.value ast.moduleDefinition) of
                        Exposing.All _ ->
                            always True

                        Exposing.Explicit explicitlyExposed ->
                            let
                                exposed : Set String
                                exposed =
                                    collectExposedElements explicitlyExposed
                            in
                            \name -> Set.member name exposed
            in
            if isFileIgnored then
                ( []
                , { exposedModules = Set.empty
                  , toReport = Dict.empty
                  , functionCallsWithArguments =
                        List.foldl
                            (\arg functionCallsWithArguments ->
                                if isExposed arg.functionName then
                                    if Set.member moduleName moduleContext.exposedModules then
                                        functionCallsWithArguments

                                    else
                                        case Dict.get arg.functionName moduleContext.functionCallsWithArguments of
                                            Just callSites ->
                                                let
                                                    key : ( ModuleName, FunctionName )
                                                    key =
                                                        ( moduleName, arg.functionName )
                                                in
                                                case Dict.get key functionCallsWithArguments of
                                                    Just previous ->
                                                        Dict.insert key ({ key = moduleKey, isFileFixable = isFileFixable, callSites = callSites } :: previous) functionCallsWithArguments

                                                    Nothing ->
                                                        Dict.insert key [ { key = moduleKey, isFileFixable = isFileFixable, callSites = callSites } ] functionCallsWithArguments

                                            Nothing ->
                                                functionCallsWithArguments

                                else
                                    functionCallsWithArguments
                            )
                            (Dict.map (\_ callSites -> [ { key = moduleKey, isFileFixable = isFileFixable, callSites = callSites } ]) moduleContext.functionCallsWithArgumentsForOtherModules)
                            (NonemptyList.head moduleContext.scopes).toReport
                  }
                )

            else
                let
                    { errors, toReport, functionCallsWithArguments } =
                        List.foldl
                            (\arg acc ->
                                if isExposed arg.functionName then
                                    if Set.member moduleName moduleContext.exposedModules then
                                        let
                                            newErrors : List (Rule.Error {})
                                            newErrors =
                                                case arg.backupWhenFixImpossible of
                                                    ApplyFix edits ->
                                                        Rule.errorWithFix arg.details arg.range edits :: acc.errors

                                                    ReportButDontFix ->
                                                        Rule.error arg.details arg.range :: acc.errors

                                                    DontReport ->
                                                        acc.errors
                                        in
                                        { errors = newErrors
                                        , toReport = acc.toReport
                                        , functionCallsWithArguments = acc.functionCallsWithArguments
                                        }

                                    else
                                        { errors = acc.errors
                                        , toReport = arg :: acc.toReport
                                        , functionCallsWithArguments =
                                            case Dict.get arg.functionName moduleContext.functionCallsWithArguments of
                                                Just callSites ->
                                                    let
                                                        key : ( ModuleName, FunctionName )
                                                        key =
                                                            ( moduleName, arg.functionName )
                                                    in
                                                    case Dict.get key acc.functionCallsWithArguments of
                                                        Just previous ->
                                                            Dict.insert key ({ key = moduleKey, isFileFixable = isFileFixable, callSites = callSites } :: previous) acc.functionCallsWithArguments

                                                        Nothing ->
                                                            Dict.insert key [ { key = moduleKey, isFileFixable = isFileFixable, callSites = callSites } ] acc.functionCallsWithArguments

                                                Nothing ->
                                                    acc.functionCallsWithArguments
                                        }

                                else
                                    { errors = List.Extra.maybeCons (reportError moduleContext.functionCallsWithArguments arg) acc.errors
                                    , toReport = acc.toReport
                                    , functionCallsWithArguments = acc.functionCallsWithArguments
                                    }
                            )
                            { errors = []
                            , toReport = []
                            , functionCallsWithArguments =
                                Dict.map
                                    (\_ callSites -> [ { key = moduleKey, isFileFixable = isFileFixable, callSites = callSites } ])
                                    moduleContext.functionCallsWithArgumentsForOtherModules
                            }
                            (NonemptyList.head moduleContext.scopes).toReport
                in
                ( errors
                , { exposedModules = Set.empty
                  , toReport = Dict.singleton moduleName { key = moduleKey, args = toReport }
                  , functionCallsWithArguments = functionCallsWithArguments
                  }
                )
        )
        |> Rule.withModuleName
        |> Rule.withModuleKey
        |> Rule.withFullAst
        |> Rule.withIsFileIgnored
        |> Rule.withIsFileFixable


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedModules = previousContext.exposedModules
    , toReport = Dict.union newContext.toReport previousContext.toReport
    , functionCallsWithArguments = mergeFunctionCallsWithArguments previousContext.functionCallsWithArguments newContext.functionCallsWithArguments
    }


mergeFunctionCallsWithArguments :
    Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, isFileFixable : Bool, callSites : List CallSite })
    -> Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, isFileFixable : Bool, callSites : List CallSite })
    -> Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, isFileFixable : Bool, callSites : List CallSite })
mergeFunctionCallsWithArguments new previous =
    Dict.foldl
        (\key newDict acc ->
            case Dict.get key acc of
                Just previousList ->
                    Dict.insert key (newDict ++ previousList) acc

                Nothing ->
                    Dict.insert key newDict acc
        )
        previous
        new


finalEvaluation : ProjectContext -> List (Rule.Error scope)
finalEvaluation projectContext =
    Dict.foldl
        (\moduleName { key, args } acc ->
            List.filterMap
                (\arg ->
                    let
                        toError : List FixV2 -> Maybe (Rule.Error scope)
                        toError fixes =
                            Rule.errorForModule key arg.details arg.range
                                |> Rule.withFixesV2 fixes
                                |> Just
                    in
                    case
                        Dict.get ( moduleName, arg.functionName ) projectContext.functionCallsWithArguments
                            |> Maybe.withDefault []
                            |> (\callSitesPerFile ->
                                    case ParameterPath.fixDeclaration arg.path of
                                        Just edits ->
                                            edits
                                                |> List.map Fix.removeRange
                                                |> Rule.editModule key
                                                |> List.singleton
                                                |> applyFixesAcrossModules arg callSitesPerFile

                                        Nothing ->
                                            Nothing
                               )
                    of
                        Just edits ->
                            toError edits

                        Nothing ->
                            case arg.backupWhenFixImpossible of
                                ReportButDontFix ->
                                    toError []

                                DontReport ->
                                    Nothing

                                ApplyFix edits ->
                                    toError [ Rule.editModule key edits ]
                )
                args
                ++ acc
        )
        []
        projectContext.toReport


applyFixesAcrossModules : ArgumentToReport -> List { key : ModuleKey, isFileFixable : Bool, callSites : List CallSite } -> List FixV2 -> Maybe (List FixV2)
applyFixesAcrossModules arg callSitesPerFile fixesSoFar =
    case callSitesPerFile of
        [] ->
            Just fixesSoFar

        { key, isFileFixable, callSites } :: rest ->
            if isFileFixable then
                case addArgumentToRemove arg.path.index (List.reverse arg.path.nesting) callSites [] of
                    Nothing ->
                        Nothing

                    Just rangesToRemove ->
                        applyFixesAcrossModules
                            arg
                            rest
                            (Rule.editModule key (List.map Fix.removeRange rangesToRemove) :: fixesSoFar)

            else
                Nothing



-- MODULE DEFINITION


collectExposedElements : List (Node Exposing.TopLevelExpose) -> Set String
collectExposedElements exposed =
    List.foldl
        (\(Node _ exp) set ->
            case exp of
                Exposing.FunctionExpose name ->
                    Set.insert name set

                _ ->
                    set
        )
        Set.empty
        exposed



-- DECLARATION VISITOR


declarationEnterVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationEnterVisitor (Node _ node) context =
    case node of
        Declaration.FunctionDeclaration f ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value f.declaration

                (Node functionNameRange functionName) =
                    declaration.name

                declared : List (List Declared)
                declared =
                    findDeclared functionName NamedFunction functionNameRange.end declaration.arguments f.signature
            in
            { exposedModules = context.exposedModules
            , lookupTable = context.lookupTable
            , scopes =
                NonemptyList.cons
                    { functionName = functionName
                    , declared = List.concat declared
                    , functionsDeclaredInSubScope = Set.empty
                    , used = Set.empty
                    , usedRecursively = Set.empty
                    , toReport = []
                    , locationsToIgnoreForFunctionCalls = []
                    }
                    context.scopes
            , recursiveFunctions = Dict.singleton functionName (getArgNames declared)
            , locationsToIgnoreForRecursiveArguments = Dict.empty
            , functionCallsWithArguments = context.functionCallsWithArguments
            , functionCallsWithArgumentsForOtherModules = context.functionCallsWithArgumentsForOtherModules
            , functionsFromOtherModulesToFix = context.functionsFromOtherModulesToFix
            , locationsToIgnoreFunctionCalls = []
            }

        _ ->
            context


declarationExitVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationExitVisitor (Node _ node) context =
    case node of
        Declaration.FunctionDeclaration _ ->
            report context

        _ ->
            ( [], context )


getArgNames : List (List Declared) -> FunctionArgs
getArgNames declared =
    getArgNamesHelp declared 0 Dict.empty


getArgNamesHelp : List (List Declared) -> Int -> FunctionArgs -> FunctionArgs
getArgNamesHelp declared index acc =
    case declared of
        [] ->
            acc

        args :: restOfDeclared ->
            let
                newAcc : Dict Int String
                newAcc =
                    case args of
                        [ arg ] ->
                            Dict.insert index arg.name acc

                        _ ->
                            acc
            in
            getArgNamesHelp restOfDeclared (index + 1) newAcc


getParametersFromPatterns : ParameterPath.Path -> Location -> Source -> Node Pattern -> List Declared
getParametersFromPatterns path previousEnd source (Node range node) =
    case node of
        Pattern.ParenthesizedPattern pattern ->
            getParametersFromPatterns path previousEnd source pattern

        Pattern.VarPattern name ->
            let
                ignoreFix : List Fix
                ignoreFix =
                    [ Fix.replaceRangeBy range "_" ]
            in
            [ { name = name
              , range = range
              , kind = Parameter
              , tryToRemoveArg = True
              , toIgnoredFix = ignoreFix
              , path = path
              , source = source
              , backupWhenFixImpossible = ApplyFix ignoreFix
              }
            ]

        Pattern.AllPattern ->
            if List.isEmpty path.nesting && source == NamedFunction then
                [ { name = "_"
                  , range = range
                  , kind = Parameter
                  , tryToRemoveArg = True
                  , toIgnoredFix = []
                  , path = path
                  , source = source
                  , backupWhenFixImpossible = DontReport
                  }
                ]

            else
                []

        Pattern.AsPattern pattern asName ->
            getParametersFromAsPattern path previousEnd source pattern asName

        Pattern.RecordPattern fields ->
            case fields of
                [ Node fieldRange fieldName ] ->
                    [ { name = fieldName
                      , range = fieldRange
                      , kind = Parameter
                      , tryToRemoveArg = not path.isUnderAlias
                      , toIgnoredFix = [ Fix.replaceRangeBy range "_" ]
                      , path = path
                      , source = source
                      , backupWhenFixImpossible = ReportButDontFix
                      }
                    ]

                _ ->
                    getParametersFromRecordPattern path source fields Nothing []

        Pattern.TuplePattern patterns ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    patterns
                        |> List.indexedMap
                            (\tupleIndex pattern ->
                                getParametersFromPatterns
                                    (ParameterPath.inTuple tupleIndex path)
                                    previousEnd
                                    source
                                    pattern
                            )
                        |> List.concat
            in
            if List.isEmpty parametersFromPatterns && List.all isPatternWildCard patterns then
                [ { name = ""
                  , range = range
                  , kind = TupleWithoutVariables
                  , tryToRemoveArg = True
                  , toIgnoredFix = [ Fix.replaceRangeBy range "_" ]
                  , path = path
                  , source = source
                  , backupWhenFixImpossible = ReportButDontFix
                  }
                ]

            else
                parametersFromPatterns

        Pattern.NamedPattern _ patterns ->
            patterns
                |> List.concatMap
                    (\pattern ->
                        getParametersFromPatterns
                            (ParameterPath.inNamedPattern path)
                            previousEnd
                            source
                            pattern
                    )

        _ ->
            []


getParametersFromRecordPattern : Path -> Source -> List (Node String) -> Maybe Location -> List Declared -> List Declared
getParametersFromRecordPattern path source fields previousEnd acc =
    case fields of
        [] ->
            acc

        (Node fieldRange fieldName) :: rest ->
            let
                subPath : Path
                subPath =
                    ParameterPath.inRecord fieldName path

                removeRange : Range
                removeRange =
                    case previousEnd of
                        Just previousEnd_ ->
                            { start = previousEnd_, end = fieldRange.end }

                        Nothing ->
                            case rest of
                                (Node next _) :: _ ->
                                    { start = fieldRange.start, end = next.start }

                                [] ->
                                    { start = fieldRange.start, end = fieldRange.end }

                parameter : Declared
                parameter =
                    { name = fieldName
                    , range = fieldRange
                    , kind = Parameter
                    , tryToRemoveArg = not path.isUnderAlias
                    , toIgnoredFix = [ Fix.removeRange removeRange ]
                    , path = subPath
                    , source = source
                    , backupWhenFixImpossible = ApplyFix [ Fix.removeRange removeRange ]
                    }
            in
            getParametersFromRecordPattern
                path
                source
                rest
                (Just fieldRange.end)
                (parameter :: acc)


getParametersFromAsPattern : Path -> Location -> Source -> Node Pattern -> Node String -> List Declared
getParametersFromAsPattern path previousEnd source ((Node patternRange _) as pattern) (Node asRange asName) =
    let
        parametersFromPatterns : List Declared
        parametersFromPatterns =
            getParametersFromPatterns (ParameterPath.inAlias path) previousEnd source pattern

        asParameter : Declared
        asParameter =
            { name = asName
            , range = asRange
            , kind = Alias
            , tryToRemoveArg = False
            , toIgnoredFix = [ Fix.removeRange { start = patternRange.end, end = asRange.end } ]
            , path = path
            , source = source
            , backupWhenFixImpossible = ReportButDontFix
            }
    in
    asParameter :: parametersFromPatterns


isPatternWildCard : Node Pattern -> Bool
isPatternWildCard (Node _ node) =
    case node of
        Pattern.ParenthesizedPattern pattern ->
            isPatternWildCard pattern

        Pattern.AllPattern ->
            True

        _ ->
            False



-- EXPRESSION ENTER VISITOR


expressionEnterVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionEnterVisitor (Node range node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            context
                |> markValueAsUsed range name
                |> registerFunctionCallReference name range []

        Expression.RecordUpdateExpression (Node nameRange name) _ ->
            markValueAsUsed nameRange name context

        Expression.LambdaExpression { args } ->
            let
                start : Location
                start =
                    range.start
            in
            { context
                | scopes =
                    NonemptyList.cons
                        { functionName = "dummy lambda"
                        , declared = findDeclared "dummy lambda" Lambda { row = start.row, column = start.column + 1 } args Nothing |> List.concat
                        , functionsDeclaredInSubScope = Set.empty
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = []
                        , locationsToIgnoreForFunctionCalls = []
                        }
                        context.scopes
            }

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments) ->
            registerFunctionCallReference fnName fnRange arguments context

        Expression.OperatorApplication "|>" _ (Node { start } lastArg) (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments))) ->
            registerFunctionCallReference
                fnName
                fnRange
                (arguments ++ [ Node { start = start, end = applicationRange.start } lastArg ])
                context

        Expression.OperatorApplication "<|" _ (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments))) (Node { end } lastArg) ->
            registerFunctionCallReference
                fnName
                fnRange
                (arguments ++ [ Node { start = applicationRange.end, end = end } lastArg ])
                context

        _ ->
            context



-- EXPRESSION EXIT VISITOR


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionExitVisitor (Node _ node) context =
    case node of
        Expression.LambdaExpression _ ->
            report context

        _ ->
            ( [], context )


letDeclarationEnterVisitor : Node Expression.LetDeclaration -> ModuleContext -> ModuleContext
letDeclarationEnterVisitor (Node _ letDeclaration) context =
    case letDeclaration of
        Expression.LetFunction function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration
            in
            if List.isEmpty declaration.arguments then
                context

            else
                let
                    (Node functionNameRange functionName) =
                        declaration.name

                    declared : List (List Declared)
                    declared =
                        findDeclared functionName NamedFunction functionNameRange.end declaration.arguments function.signature

                    newScope : Scope
                    newScope =
                        { functionName = functionName
                        , declared = List.concat declared
                        , functionsDeclaredInSubScope = Set.empty
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        , toReport = []
                        , locationsToIgnoreForFunctionCalls = []
                        }
                in
                { context
                    | scopes = NonemptyList.cons newScope context.scopes
                    , recursiveFunctions = Dict.insert functionName (getArgNames declared) context.recursiveFunctions
                }

        Expression.LetDestructuring _ _ ->
            context


letDeclarationExitVisitor : a -> Node Expression.LetDeclaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
letDeclarationExitVisitor _ (Node _ letDeclaration) context =
    case letDeclaration of
        Expression.LetFunction function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration
            in
            if List.isEmpty declaration.arguments then
                ( [], context )

            else
                report context

        Expression.LetDestructuring _ _ ->
            ( [], context )


registerFunctionCallReference : FunctionName -> Range -> List (Node Expression) -> ModuleContext -> ModuleContext
registerFunctionCallReference fnName fnRange arguments context =
    if isVariableOrFunctionName fnName && not (List.member fnRange.start context.locationsToIgnoreFunctionCalls) then
        case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
            Just [] ->
                case Dict.get fnName context.recursiveFunctions of
                    Just fnArgs ->
                        let
                            locationsToIgnore : LocationsToIgnore
                            locationsToIgnore =
                                ignoreLocationsForRecursiveArguments fnArgs arguments 0 context.locationsToIgnoreForRecursiveArguments
                        in
                        { context
                            | locationsToIgnoreForRecursiveArguments = locationsToIgnore
                            , locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls
                        }
                            |> registerLocalFunctionReference fnName fnRange.end (Array.fromList arguments)

                    Nothing ->
                        { context | locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls }
                            |> registerLocalFunctionReference fnName fnRange.end (Array.fromList arguments)

            Just moduleName ->
                registerExternalFunctionReference moduleName
                    fnName
                    fnRange
                    arguments
                    { context | locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls }

            Nothing ->
                context

    else
        context


registerExternalFunctionReference : ModuleName -> FunctionName -> Range -> List (Node Expression) -> ModuleContext -> ModuleContext
registerExternalFunctionReference moduleName fnName fnRange arguments context =
    let
        key : ( ModuleName, FunctionName )
        key =
            ( moduleName, fnName )
    in
    if Set.member key context.functionsFromOtherModulesToFix then
        let
            functionCallsWithArgumentsForOtherModules : Dict ( ModuleName, FunctionName ) (List CallSite)
            functionCallsWithArgumentsForOtherModules =
                case Dict.get key context.functionCallsWithArgumentsForOtherModules of
                    Just previous ->
                        Dict.insert key ({ fnNameEnd = fnRange.end, arguments = Array.fromList arguments } :: previous) context.functionCallsWithArgumentsForOtherModules

                    Nothing ->
                        Dict.insert key [ { fnNameEnd = fnRange.end, arguments = Array.fromList arguments } ] context.functionCallsWithArgumentsForOtherModules
        in
        { context | functionCallsWithArgumentsForOtherModules = functionCallsWithArgumentsForOtherModules }

    else
        context


registerLocalFunctionReference : FunctionName -> Location -> Array (Node Expression) -> ModuleContext -> ModuleContext
registerLocalFunctionReference fnName fnNameEnd arguments context =
    { context
        | functionCallsWithArguments =
            case Dict.get fnName context.functionCallsWithArguments of
                Just previous ->
                    Dict.insert fnName ({ fnNameEnd = fnNameEnd, arguments = arguments } :: previous) context.functionCallsWithArguments

                Nothing ->
                    Dict.insert fnName [ { fnNameEnd = fnNameEnd, arguments = arguments } ] context.functionCallsWithArguments
    }


ignoreLocationsForRecursiveArguments : FunctionArgs -> List (Node Expression) -> Int -> LocationsToIgnore -> LocationsToIgnore
ignoreLocationsForRecursiveArguments fnArgs nodes index acc =
    case nodes of
        [] ->
            acc

        (Node range _) :: rest ->
            let
                newAcc : LocationsToIgnore
                newAcc =
                    case Dict.get index fnArgs of
                        Just argName ->
                            insertInDictList argName range acc

                        Nothing ->
                            acc
            in
            ignoreLocationsForRecursiveArguments fnArgs rest (index + 1) newAcc


markValueAsUsed : Range -> String -> ModuleContext -> ModuleContext
markValueAsUsed range name context =
    if isVariableOrFunctionName name then
        case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
            Just [] ->
                { context
                    | scopes =
                        NonemptyList.mapHead
                            (\scope ->
                                -- TODO Avoid changing context if name is not a parameter
                                if shouldBeIgnored range name context then
                                    { scope | usedRecursively = Set.insert name scope.usedRecursively }

                                else
                                    { scope | used = Set.insert name scope.used }
                            )
                            context.scopes
                }

            _ ->
                context

    else
        context


isVariableOrFunctionName : String -> Bool
isVariableOrFunctionName name =
    case name |> String.slice 0 1 |> String.toList |> List.head of
        Just firstChar ->
            Char.isLower firstChar

        Nothing ->
            False


shouldBeIgnored : Range -> String -> ModuleContext -> Bool
shouldBeIgnored range name context =
    case Dict.get name context.locationsToIgnoreForRecursiveArguments of
        Just ranges ->
            List.any (isRangeIncluded range) ranges

        Nothing ->
            False


isRangeIncluded : Range -> Range -> Bool
isRangeIncluded inner outer =
    (Range.compareLocations inner.start outer.start /= LT)
        && (Range.compareLocations inner.end outer.end /= GT)


markAllAsUsed : Scope -> Set String -> List ArgumentToReport -> Nonempty Scope -> Nonempty Scope
markAllAsUsed subScope names toReport scopes =
    NonemptyList.mapHead
        (\scope ->
            { scope
                | used = Set.union names scope.used
                , toReport = toReport ++ scope.toReport
                , functionsDeclaredInSubScope = List.foldl (\arg set -> Set.insert arg.path.functionName set) scope.functionsDeclaredInSubScope subScope.declared
            }
        )
        scopes


report : ModuleContext -> ( List (Rule.Error {}), ModuleContext )
report context =
    let
        ( headScope, scopes ) =
            NonemptyList.headAndPop context.scopes

        { reportLater, reportNow, remainingUsed } =
            List.foldl
                (findErrorsAndVariablesNotPartOfScope headScope)
                { reportLater = [], reportNow = [], remainingUsed = headScope.used }
                headScope.declared
    in
    ( reportErrors context.functionCallsWithArguments headScope.toReport reportNow
    , { context
        | scopes = markAllAsUsed headScope remainingUsed reportLater scopes
        , functionCallsWithArguments = Set.foldl Dict.remove context.functionCallsWithArguments headScope.functionsDeclaredInSubScope
        , recursiveFunctions = Dict.remove headScope.functionName context.recursiveFunctions
      }
    )


reportErrors : Dict FunctionName (List CallSite) -> List ArgumentToReport -> List (Rule.Error {}) -> List (Rule.Error {})
reportErrors functionCallsWithArguments toReport initialErrors =
    List.foldl
        (\arg errorAcc ->
            case reportError functionCallsWithArguments arg of
                Just error ->
                    error :: errorAcc

                Nothing ->
                    errorAcc
        )
        initialErrors
        toReport


reportError : Dict FunctionName (List CallSite) -> ArgumentToReport -> Maybe (Rule.Error {})
reportError functionCallsWithArguments ({ functionName, details, path, range } as arg) =
    case
        ParameterPath.fixDeclaration path
            |> Maybe.andThen
                (\edits ->
                    case Dict.get functionName functionCallsWithArguments of
                        Nothing ->
                            Just edits

                        Just callArgumentList ->
                            addArgumentToRemove path.index (List.reverse path.nesting) callArgumentList edits
                )
    of
        Just edits ->
            edits
                |> List.map Fix.removeRange
                |> Rule.errorWithFix details range
                |> Just

        Nothing ->
            reportErrorWithBackup arg


reportErrorWithBackup : ArgumentToReport -> Maybe (Rule.Error {})
reportErrorWithBackup arg =
    case arg.backupWhenFixImpossible of
        ReportButDontFix ->
            Just (Rule.error arg.details arg.range)

        DontReport ->
            Nothing

        ApplyFix edits ->
            edits
                |> Rule.errorWithFix arg.details arg.range
                |> Just


addArgumentToRemove : Int -> List Nesting -> List CallSite -> List Range -> Maybe (List Range)
addArgumentToRemove position nesting callSites acc =
    case callSites of
        [] ->
            Just acc

        callSite :: rest ->
            case Array.get position callSite.arguments of
                Just ((Node range _) as node) ->
                    case ParameterPath.fixCall (prettyRemovalRange range position callSite) node nesting acc of
                        Just edits ->
                            addArgumentToRemove position nesting rest edits

                        Nothing ->
                            Nothing

                Nothing ->
                    -- If an argument at that location could not be found, then we can't autofix the issue.
                    Nothing


prettyRemovalRange : Range -> Int -> CallSite -> Range
prettyRemovalRange range position callSite =
    let
        previousEnd : Location
        previousEnd =
            case Array.get (position - 1) callSite.arguments of
                Just (Node { end } _) ->
                    end

                Nothing ->
                    callSite.fnNameEnd
    in
    -- If the call was made with |>, then the constructed range will be negative.
    -- Therefore in that case, simply remove `range` which corresponds to `arg |> `
    case compare previousEnd.row range.end.row of
        LT ->
            { start = previousEnd, end = range.end }

        EQ ->
            if previousEnd.column <= range.end.column then
                { start = previousEnd, end = range.end }

            else
                range

        GT ->
            range


findErrorsAndVariablesNotPartOfScope :
    Scope
    -> Declared
    -> { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
    -> { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
findErrorsAndVariablesNotPartOfScope scope declared ({ reportLater, reportNow, remainingUsed } as acc) =
    if Set.member declared.name scope.usedRecursively then
        -- If variable was used as a recursive argument
        if Set.member declared.name remainingUsed then
            -- If variable was used somewhere else as well
            { reportLater = reportLater
            , reportNow = reportNow
            , remainingUsed = Set.remove declared.name remainingUsed
            }

        else
        -- If variable was used ONLY as a recursive argument
        if
            declared.path.hasOnlyOneArgument
        then
            acc

        else
            recursiveParameterError scope.functionName declared
                |> accumulate acc

    else if Set.member declared.name remainingUsed then
        { reportLater = reportLater
        , reportNow = reportNow
        , remainingUsed = Set.remove declared.name remainingUsed
        }

    else
        reportParameter (errorsDetails declared) scope.functionName declared
            |> accumulate acc


accumulate :
    { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
    -> ReportTime
    -> { reportLater : List ArgumentToReport, reportNow : List (Rule.Error {}), remainingUsed : Set String }
accumulate { reportLater, reportNow, remainingUsed } reportTime =
    case reportTime of
        ReportNow error ->
            { reportLater = reportLater
            , reportNow = error :: reportNow
            , remainingUsed = remainingUsed
            }

        ReportLater error ->
            { reportLater = error :: reportLater
            , reportNow = reportNow
            , remainingUsed = remainingUsed
            }


insertInDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
insertInDictList key value dict =
    case Dict.get key dict of
        Nothing ->
            Dict.insert key [ value ] dict

        Just previous ->
            Dict.insert key (value :: previous) dict


findDeclared : String -> Source -> Location -> List (Node Pattern) -> Maybe (Node Signature) -> List (List Declared)
findDeclared functionName source previousEnd arguments signature =
    findDeclaredHelp
        source
        previousEnd
        (ParameterPath.new functionName previousEnd arguments signature)
        arguments
        []


findDeclaredHelp : Source -> Location -> Path -> List (Node Pattern) -> List (List Declared) -> List (List Declared)
findDeclaredHelp source previousEnd path arguments acc =
    case arguments of
        [] ->
            List.reverse acc

        ((Node { end } _) as arg) :: remainingArguments ->
            findDeclaredHelp
                source
                end
                (ParameterPath.nextArgument path)
                remainingArguments
                (getParametersFromPatterns path previousEnd source arg :: acc)


type ReportTime
    = ReportNow (Rule.Error {})
    | ReportLater ArgumentToReport


errorsDetails : Declared -> { message : String, details : List String }
errorsDetails { name, kind } =
    case kind of
        Parameter ->
            { message = "Parameter `" ++ name ++ "` is not used"
            , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
            }

        Alias ->
            { message = "Pattern alias `" ++ name ++ "` is not used"
            , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
            }

        TupleWithoutVariables ->
            { message = "Tuple pattern is not needed"
            , details = [ "You should remove this pattern." ]
            }


reportParameter : { message : String, details : List String } -> FunctionName -> Declared -> ReportTime
reportParameter details functionName arg =
    case arg.source of
        NamedFunction ->
            if arg.tryToRemoveArg then
                ReportLater
                    { functionName = functionName
                    , details = details
                    , path = arg.path
                    , range = arg.range
                    , backupWhenFixImpossible = arg.backupWhenFixImpossible
                    }

            else
                ReportNow (Rule.errorWithFix details arg.range arg.toIgnoredFix)

        Lambda ->
            ReportNow (Rule.errorWithFix details arg.range arg.toIgnoredFix)


recursiveParameterError : String -> Declared -> ReportTime
recursiveParameterError functionName arg =
    let
        details : { message : String, details : List String }
        details =
            { message = "Parameter `" ++ arg.name ++ "` is only used in recursion"
            , details =
                [ "This parameter is only used to be passed as an argument to '" ++ functionName ++ "', but its value is never read or used."
                , "You should either use this parameter somewhere, or remove it at the location I pointed at."
                ]
            }
    in
    case arg.kind of
        Parameter ->
            reportParameter details functionName arg

        _ ->
            Rule.error details arg.range
                |> ReportNow
