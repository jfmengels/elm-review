module Review.Test exposing
    ( ReviewResult, run, runWithProjectData, runOnModules, runOnModulesWithProjectData
    , ExpectedError, expectNoErrors, expectErrors, error, atExactly, whenFixed, shouldFixFiles
    , ExpectedFix, shouldFixFilesWithIO, edited, removed
    , expectErrorsForModules, expectErrorsForElmJson, expectErrorsForReadme, expectErrorsForExtraFile
    , expectGlobalErrors, expectGlobalErrorsWithFixes
    , expectConfigurationError
    , expectDataExtract
    , ignoredFilesImpactResults
    , expect, ReviewExpectation
    , moduleErrors, globalErrors, globalErrorsWithFixes, elmJsonErrors, readmeErrors, extraFileErrors, dataExtract
    , expectGlobalAndLocalErrors, expectGlobalAndModuleErrors
    )

{-| Module that helps you test your rules, using [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/).

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should not report anything when <condition>" <|
                \() ->
                    """module A exposing (..)
    a = foo n"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "should report Debug.log use" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "some" "message" """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
            ]


# Strategies for effective testing


## Use Test-Driven Development

Writing a rule is a process that works really well with the Test-Driven
Development process loop, which is:

  - Before writing any code, write a failing test.
  - Run the test and make sure that it is failing, otherwise you can't be
    sure that the test is well-written.
  - Write the simplest (almost stupid) code to make the test pass
  - Run the tests again and make sure that the test is passing, and that you
    didn't break any previous tests
  - Optionally, refactor your code but be sure not to change the behavior of the
    implementation. You should not add support for new patterns, as you will
    want to write tests for those first.

Then repeat for every pattern you wish to handle.


## Have a good title

A good test title explains

  - what is tested - Probably the rule, but making it explicit
    in a [`describe`](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test#describe)
    might improve your test report. Or maybe you are testing a sub-part of the rule,
    and you can name it explicitly.
  - what should happen: (not) reporting an error, fix <something> by <doing something>, ...
  - when: what is the situation that this test sets up?

Ideally, by only reading through the test titles, someone else should be able to
rewrite the rule you are testing.


## What should you test?

You should test the scenarios where you expect the rule to report something. At
the same time, you should also test when it shouldn't. I encourage writing tests
to make sure that things that are similar to what you want to report are not
reported.

For instance, if you wish to report uses of variables named `foo`, write a test
that ensures that the use of variables named differently does not get reported.

Tests are pretty cheap, and in the case of rules, it is probably better to have
too many tests rather than too few, since the behavior of a rule rarely changes
drastically.


# Design goals

If you are interested, you can read
[the design goals](https://github.com/jfmengels/elm-review/blob/master/documentation/design/test-module.md)
for this module.


# Running tests

@docs ReviewResult, run, runWithProjectData, runOnModules, runOnModulesWithProjectData


# Making assertions

@docs ExpectedError, expectNoErrors, expectErrors, error, atExactly, whenFixed, shouldFixFiles
@docs ExpectedFix, shouldFixFilesWithIO, edited, removed
@docs expectErrorsForModules, expectErrorsForElmJson, expectErrorsForReadme, expectErrorsForExtraFile
@docs expectGlobalErrors, expectGlobalErrorsWithFixes
@docs expectConfigurationError
@docs expectDataExtract
@docs ignoredFilesImpactResults


## Composite assertions

@docs expect, ReviewExpectation
@docs moduleErrors, globalErrors, globalErrorsWithFixes, elmJsonErrors, readmeErrors, extraFileErrors, dataExtract


# Deprecated

@docs expectGlobalAndLocalErrors, expectGlobalAndModuleErrors

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Json.Decode as Decode
import Json.Encode as Encode
import Regex exposing (Regex)
import Review.Error.FileTarget as FileTarget exposing (FileTarget)
import Review.Error.Fixes as ErrorFixes exposing (ErrorFixes(..))
import Review.Error.ReviewError exposing (ReviewError(..))
import Review.Error.Target as Target exposing (Target)
import Review.FileParser as FileParser
import Review.Fix exposing (Fix)
import Review.Fix.Internal as FixInternal
import Review.Options as ReviewOptions
import Review.Project as Project exposing (Project, ProjectModule)
import Review.Project.Internal exposing (ProjectInternals)
import Review.Project.ProjectModule as ProjectModule
import Review.Rule as Rule exposing (ReviewError, Rule)
import Review.Test.Dependencies exposing (projectWithElmCore)
import Review.Test.ExpectationExtra exposing (onFail)
import Review.Test.FailureMessage as FailureMessage
import Set exposing (Set)
import Unicode
import Vendor.Diff as Diff
import Vendor.ListExtra as ListExtra



-- REVIEW RESULT


{-| The result of running a rule on a `String` containing source code.
-}
type ReviewResult
    = ConfigurationError { message : String, details : List String }
    | FailedRun String
    | SuccessfulRun SuccessfulRunData ReRun


type alias SuccessfulRunData =
    { foundGlobalErrors : List ReviewError
    , runResults : List SuccessfulRunResult
    , extract : ExtractResult
    , allErrors : List ReviewError
    , project : ProjectInternals
    }


type ReRun
    = AttemptReRun Rule Project
    | DontAttemptReRun


type alias GlobalError =
    { message : String
    , details : List String
    , fixes : Dict String ExpectedFix
    }


type alias SuccessfulRunResult =
    { moduleName : String
    , inspector : CodeInspector
    , errors : List ReviewError
    }


type alias CodeInspector =
    { isModule : Bool
    , source : String
    , getCodeAtLocation : Range -> Maybe String
    , checkIfLocationIsAmbiguous : ReviewError -> String -> Expectation
    }


type alias ExtractResult =
    Maybe Encode.Value


{-| An expectation for an error. Use [`error`](#error) to create one.
-}
type ExpectedError
    = ExpectedError ExpectedErrorDetails


type alias ExpectedErrorDetails =
    { message : String
    , details : List String
    , under : Under
    , expectedFixes : ExpectedFixes
    }


type ExpectedFixes
    = NoFixesExpected
    | ComesFromWhenFixed String
    | ComesFromShouldFixFiles (Dict String ExpectedFix)


type Under
    = Under String
    | UnderExactly String Range


type alias SourceCode =
    String


{-| Run a `Rule` on a `String` containing source code. You can then use
[`expectNoErrors`](#expectNoErrors) or [`expectErrors`](#expectErrors) to assert
the errors reported by the rule.

    import My.Rule exposing (rule)
    import Review.Test
    import Test exposing (Test, test)

    someTest : Test
    someTest =
        test "test title" <|
            \() ->
                """
    module SomeModule exposing (a)
    a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors

The source code needs to be syntactically valid Elm code. If the code
can't be parsed, the test will fail regardless of the expectations you set on it.

Note that to be syntactically valid, you need at least a module declaration at the
top of the file (like `module A exposing (..)`) and one declaration (like `a = 1`).
You can't just have an expression like `1 + 2`.

Note: This is a simpler version of [`runWithProjectData`](#runWithProjectData).
If your rule is interested in project related details, then you should use
[`runWithProjectData`](#runWithProjectData) instead.

-}
run : Rule -> String -> ReviewResult
run rule source =
    runWithProjectData projectWithElmCore rule source


{-| Run a `Rule` on a `String` containing source code, with data about the
project loaded, such as the contents of `elm.json` file.

    import My.Rule exposing (rule)
    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, test)

    someTest : Test
    someTest =
        test "test title" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addElmJson elmJsonToConstructManually
                in
                """module SomeModule exposing (a)
    a = 1"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors

The source code needs to be syntactically valid Elm code. If the code
can't be parsed, the test will fail regardless of the expectations you set on it.

Note that to be syntactically valid, you need at least a module declaration at the
top of the file (like `module A exposing (..)`) and one declaration (like `a = 1`).
You can't just have an expression like `1 + 2`.

Note: This is a more complex version of [`run`](#run). If your rule is not
interested in project related details, then you should use [`run`](#run) instead.

-}
runWithProjectData : Project -> Rule -> String -> ReviewResult
runWithProjectData project rule source =
    runOnModulesWithProjectData project rule [ source ]


{-| Run a `Rule` on several modules. You can then use
[`expectNoErrors`](#expectNoErrors) or [`expectErrorsForModules`](#expectErrorsForModules) to assert
the errors reported by the rule.

This is the same as [`run`](#run), but you can pass several modules.
This is especially useful to test rules created with
[`Review.Rule.newProjectRuleSchema`](./Review-Rule#newProjectRuleSchema), that look at
several files, and where the context of the project is important.

    import My.Rule exposing (rule)
    import Review.Test
    import Test exposing (Test, test)

    someTest : Test
    someTest =
        test "test title" <|
            \() ->
                [ """
    module A exposing (a)
    a = 1""", """
    module B exposing (a)
    a = 1""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors

The source codes need to be syntactically valid Elm code. If the code
can't be parsed, the test will fail regardless of the expectations you set on it.

Note that to be syntactically valid, you need at least a module declaration at the
top of each file (like `module A exposing (..)`) and one declaration (like `a = 1`).
You can't just have an expression like `1 + 2`.

Note: This is a simpler version of [`runOnModulesWithProjectData`](#runOnModulesWithProjectData).
If your rule is interested in project related details, then you should use
[`runOnModulesWithProjectData`](#runOnModulesWithProjectData) instead.

-}
runOnModules : Rule -> List String -> ReviewResult
runOnModules rule sources =
    runOnModulesWithProjectData projectWithElmCore rule sources


{-| Run a `Rule` on several modules. You can then use
[`expectNoErrors`](#expectNoErrors) or [`expectErrorsForModules`](#expectErrorsForModules) to assert
the errors reported by the rule.

This is basically the same as [`run`](#run), but you can pass several modules.
This is especially useful to test rules created with
[`Review.Rule.newProjectRuleSchema`](./Review-Rule#newProjectRuleSchema), that look at
several modules, and where the context of the project is important.

    import My.Rule exposing (rule)
    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, test)

    someTest : Test
    someTest =
        test "test title" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addElmJson elmJsonToConstructManually
                in
                [ """
    module A exposing (a)
    a = 1""", """
    module B exposing (a)
    a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectNoErrors

The source codes need to be syntactically valid Elm code. If the code
can't be parsed, the test will fail regardless of the expectations you set on it.

Note that to be syntactically valid, you need at least a module declaration at the
top of each file (like `module A exposing (..)`) and one declaration (like `a = 1`).
You can't just have an expression like `1 + 2`.

Note: This is a more complex version of [`runOnModules`](#runOnModules). If your rule is not
interested in project related details, then you should use [`runOnModules`](#runOnModules) instead.

-}
runOnModulesWithProjectData : Project -> Rule -> List String -> ReviewResult
runOnModulesWithProjectData project rule sources =
    case Rule.getConfigurationError rule of
        Just configurationError ->
            ConfigurationError configurationError

        Nothing ->
            runOnModulesWithProjectDataHelp project rule sources


runOnModulesWithProjectDataHelp : Project -> Rule -> List String -> ReviewResult
runOnModulesWithProjectDataHelp project rule sources =
    let
        ( projectWithModules, modulesThatFailedToParse ) =
            List.foldl
                (\source ( accProject, accModulesThatFailedToParse ) ->
                    case FileParser.parse source of
                        Ok ast ->
                            ( Project.addParsedModule
                                { path = "src/" ++ String.join "/" (Module.moduleName (Node.value ast.moduleDefinition)) ++ ".elm"
                                , source = source
                                , ast = ast
                                }
                                accProject
                            , accModulesThatFailedToParse
                            )

                        Err _ ->
                            ( accProject
                            , { source = source, path = "test/DummyFilePath.elm" } :: accModulesThatFailedToParse
                            )
                )
                ( project, [] )
                sources
    in
    case modulesThatFailedToParse ++ Project.modulesThatFailedToParse projectWithModules of
        { source } :: _ ->
            let
                fileAndIndex : { source : String, index : Int }
                fileAndIndex =
                    { source = source
                    , index = indexOf source sources |> Maybe.withDefault -1
                    }
            in
            FailedRun <| FailureMessage.parsingFailure (hasOneElement sources) fileAndIndex

        [] ->
            let
                modules : List ProjectModule
                modules =
                    Project.modules projectWithModules
            in
            if List.isEmpty modules then
                FailedRun FailureMessage.missingSources

            else
                case findDuplicateModuleNames Set.empty modules of
                    Just moduleName ->
                        FailedRun <| FailureMessage.duplicateModuleName moduleName

                    Nothing ->
                        let
                            { errors, extracts } =
                                Rule.reviewV3
                                    (ReviewOptions.defaults
                                        |> ReviewOptions.withDataExtraction True
                                        |> ReviewOptions.withFileRemovalFixes True
                                    )
                                    [ rule ]
                                    projectWithModules
                        in
                        case ListExtra.find (\err -> Rule.errorTarget err == Target.Global) errors of
                            Just globalError_ ->
                                FailedRun <| FailureMessage.globalErrorInTest globalError_

                            Nothing ->
                                let
                                    extract : ExtractResult
                                    extract =
                                        Dict.get (Rule.ruleName rule) extracts

                                    fileErrors : List SuccessfulRunResult
                                    fileErrors =
                                        List.concat
                                            [ List.map (\module_ -> moduleToRunResult errors module_) modules
                                            , elmJsonRunResult errors projectWithModules
                                            , readmeRunResult errors projectWithModules
                                            , extraFileRunResult errors projectWithModules
                                            ]

                                    foundGlobalErrors : List ReviewError
                                    foundGlobalErrors =
                                        errors
                                            |> List.filter (\error_ -> Rule.errorTarget error_ == Target.UserGlobal)

                                    (Review.Project.Internal.Project unwrappedProject) =
                                        projectWithModules
                                in
                                SuccessfulRun
                                    { foundGlobalErrors = foundGlobalErrors
                                    , runResults = fileErrors
                                    , extract = extract
                                    , allErrors = errors
                                    , project = unwrappedProject
                                    }
                                    (AttemptReRun rule projectWithModules)


hasOneElement : List a -> Bool
hasOneElement list =
    case list of
        [ _ ] ->
            True

        _ ->
            False


moduleToRunResult : List ReviewError -> ProjectModule -> SuccessfulRunResult
moduleToRunResult errors projectModule =
    { moduleName = String.join "." projectModule.moduleName
    , inspector = codeInspectorForSource True projectModule.source
    , errors =
        errors
            |> List.filter (\error_ -> Rule.errorFilePath error_ == projectModule.path)
            |> List.sortWith compareErrorPositions
    }


elmJsonRunResult : List ReviewError -> Project -> List SuccessfulRunResult
elmJsonRunResult errors project =
    case Project.elmJson project of
        Just elmJsonData ->
            case List.filter (\error_ -> Rule.errorFilePath error_ == elmJsonData.path) errors of
                [] ->
                    []

                errorsForElmJson ->
                    [ { moduleName = elmJsonData.path
                      , inspector = codeInspectorForSource False elmJsonData.raw
                      , errors = errorsForElmJson
                      }
                    ]

        Nothing ->
            []


readmeRunResult : List ReviewError -> Project -> List SuccessfulRunResult
readmeRunResult errors project =
    case Project.readme project of
        Just projectReadme ->
            case List.filter (\error_ -> Rule.errorFilePath error_ == projectReadme.path) errors of
                [] ->
                    []

                errorsForReadme ->
                    [ { moduleName = projectReadme.path
                      , inspector = codeInspectorForSource False projectReadme.content
                      , errors = errorsForReadme
                      }
                    ]

        Nothing ->
            []


extraFileRunResult : List ReviewError -> Project -> List SuccessfulRunResult
extraFileRunResult errors project =
    let
        errorsPerFile_ : Dict String (List ReviewError)
        errorsPerFile_ =
            errorsPerFile errors
    in
    Dict.foldl
        (\path content acc ->
            { moduleName = path
            , inspector = codeInspectorForSource False content
            , errors = Dict.get path errorsPerFile_ |> Maybe.withDefault []
            }
                :: acc
        )
        []
        (Project.extraFiles project)


errorsPerFile : List ReviewError -> Dict String (List ReviewError)
errorsPerFile reviewErrors =
    List.foldl
        (\error_ acc ->
            Dict.update
                (Rule.errorFilePath error_)
                (\maybePreviousErrors ->
                    case maybePreviousErrors of
                        Nothing ->
                            Just [ error_ ]

                        Just previousErrors ->
                            Just (error_ :: previousErrors)
                )
                acc
        )
        Dict.empty
        reviewErrors


indexOf : a -> List a -> Maybe Int
indexOf elementToFind aList =
    indexOfHelp elementToFind aList 0


indexOfHelp : a -> List a -> Int -> Maybe Int
indexOfHelp elementToFind aList offset =
    case aList of
        [] ->
            Nothing

        a :: rest ->
            if a == elementToFind then
                Just offset

            else
                indexOfHelp elementToFind rest (offset + 1)


codeInspectorForSource : Bool -> String -> CodeInspector
codeInspectorForSource isModule source =
    { isModule = isModule
    , source = source
    , getCodeAtLocation = getCodeAtLocationInSourceCode source
    , checkIfLocationIsAmbiguous = checkIfLocationIsAmbiguousInSourceCode source
    }


findDuplicateModuleNames : Set (List String) -> List ProjectModule -> Maybe (List String)
findDuplicateModuleNames previousModuleNames modules =
    case modules of
        [] ->
            Nothing

        { moduleName } :: restOfModules ->
            if Set.member moduleName previousModuleNames then
                Just moduleName

            else
                findDuplicateModuleNames (Set.insert moduleName previousModuleNames) restOfModules


compareErrorPositions : ReviewError -> ReviewError -> Order
compareErrorPositions a b =
    compareRange (Rule.errorRange a) (Rule.errorRange b)


compareRange : Range -> Range -> Order
compareRange a b =
    if a.start.row < b.start.row then
        LT

    else if a.start.row > b.start.row then
        GT

    else
    -- Start row is the same from here on
    if
        a.start.column < b.start.column
    then
        LT

    else if a.start.column > b.start.column then
        GT

    else
    -- Start row and column are the same from here on
    if
        a.end.row < b.end.row
    then
        LT

    else if a.end.row > b.end.row then
        GT

    else
    -- Start row and column, and end row are the same from here on
    if
        a.end.column < b.end.column
    then
        LT

    else if a.end.column > b.end.column then
        GT

    else
        EQ


{-| Assert that the rule reported no errors. Note, this is equivalent to using [`expectErrors`](#expectErrors)
like `expectErrors []`.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should not report anything when <condition>" <|
                \() ->
                    """module A exposing (..)
    a = foo n"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            ]

-}
expectNoErrors : ReviewResult -> Expectation
expectNoErrors reviewResult =
    expect [] reviewResult


checkNoGlobalErrors : List ReviewError -> Expectation
checkNoGlobalErrors foundGlobalErrors =
    if List.isEmpty foundGlobalErrors then
        Expect.pass

    else
        foundGlobalErrors
            |> List.map Rule.errorMessage
            |> FailureMessage.didNotExpectGlobalErrors
            |> Expect.fail


checkNoModuleErrors : List SuccessfulRunResult -> Expectation
checkNoModuleErrors runResults =
    Expect.all
        (List.map (\runResult -> \() -> checkNoErrorForModuleRunResult runResult) runResults)
        ()


checkNoErrorForModuleRunResult : SuccessfulRunResult -> Expectation
checkNoErrorForModuleRunResult { moduleName, errors } =
    if List.isEmpty errors then
        Expect.pass

    else
        Expect.fail <| FailureMessage.didNotExpectErrors moduleName errors


{-| Assert that the rule reported some errors, by specifying which ones.

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should report Debug.log use" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "some" "message"
    """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
            ]

-}
expectErrors : List ExpectedError -> ReviewResult -> Expectation
expectErrors expectedErrors reviewResult =
    expectGlobalAndLocalErrors
        { global = []
        , local = expectedErrors
        }
        reviewResult


{-| Assert that the rule reported some errors, by specifying which ones and the
module for which they were reported.

This is the same as [`expectErrors`](#expectErrors), but for when you used
[`runOnModules`](#runOnModules) or [`runOnModulesWithProjectData`](#runOnModulesWithProjectData).
to create the test. When using those, the errors you expect need to be associated
with a module. If we don't specify this, your tests might pass because you
expected the right errors, but they may be reported for the wrong module!

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`moduleErrors`](#moduleErrors) functions.

The expected errors are tupled: the first element is the module name
(for example: `List` or `My.Module.Name`) and the second element is the list of
errors you expect to be reported.

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report an error when a module uses `Debug.log`" <|
            \() ->
                [ """
    module ModuleA exposing (a)
    a = 1""", """
    module ModuleB exposing (a)
    a = Debug.log "log" 1""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "ModuleB"
                          , [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
                          )
                        ]

-}
expectErrorsForModules : List ( String, List ExpectedError ) -> ReviewResult -> Expectation
expectErrorsForModules expectedErrorsList reviewResult =
    expect
        (List.map (\( moduleName, errors ) -> moduleErrors moduleName errors) expectedErrorsList)
        reviewResult


{-| **@deprecated** Use [`Review.Test.expect`](#expect) instead.

Assert that the rule reported some [global errors](./Review-Rule#globalError) and [local](./Review-Test#ExpectedError) errors, by specifying which ones.

Use this function when you expect both local and global errors for a particular test, and when you are using [`run`](#run) or [`runWithProjectData`](#runWithProjectData).
When using [`runOnModules`](#runOnModules) or [`runOnModulesWithProjectData`](#runOnModulesWithProjectData), use [`expectGlobalAndModuleErrors`](#expectGlobalAndModuleErrors) instead.

If you only have local or global errors, you should instead use [`expectErrors`](#expectErrors) or [`expectGlobalErrors`](#expectGlobalErrors) respectively.

This function works in the same way as [`expectErrors`](#expectErrors) and [`expectGlobalErrors`](#expectGlobalErrors).

-}
expectGlobalAndLocalErrors : { local : List ExpectedError, global : List { message : String, details : List String } } -> ReviewResult -> Expectation
expectGlobalAndLocalErrors { global, local } reviewResult =
    case reviewResult of
        ConfigurationError configurationError ->
            Expect.fail (FailureMessage.unexpectedConfigurationError configurationError)

        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun { foundGlobalErrors, runResults, extract, allErrors, project } reRun ->
            Expect.all
                [ \() ->
                    if List.isEmpty global then
                        checkNoGlobalErrors foundGlobalErrors

                    else
                        checkAllGlobalErrorsMatch
                            project
                            (List.length global)
                            { expected = List.map (\{ message, details } -> { message = message, details = details, fixes = Dict.empty }) global
                            , actual = foundGlobalErrors
                            }
                , \() ->
                    if List.isEmpty local then
                        checkNoModuleErrors runResults

                    else
                        case runResults of
                            runResult :: [] ->
                                checkAllErrorsMatch project runResult local

                            _ ->
                                Expect.fail FailureMessage.needToUsedExpectErrorsForModules
                , \() -> checkNoDataExtract extract
                , \() -> checkResultsAreTheSameWhenIgnoringFiles allErrors reRun
                ]
                ()


{-| **@deprecated** Use [`Review.Test.expect`](#expect) instead.

Assert that the rule reported some errors for modules and global errors, by specifying which ones.

Use this function when you expect both local and global errors for a particular test, and when you are using [`runOnModules`](#runOnModules) or [`runOnModulesWithProjectData`](#runOnModulesWithProjectData).
When using[`run`](#run) or [`runWithProjectData`](#runWithProjectData), use [`expectGlobalAndLocalErrors`](#expectGlobalAndLocalErrors) instead.

If you only have local or global errors, you should instead use [`expectErrorsForModules`](#expectErrorsForModules) or [`expectGlobalErrors`](#expectGlobalErrors) respectively.

This function works in the same way as [`expectErrorsForModules`](#expectErrorsForModules) and [`expectGlobalErrors`](#expectGlobalErrors).

-}
expectGlobalAndModuleErrors : { global : List { message : String, details : List String }, modules : List ( String, List ExpectedError ) } -> ReviewResult -> Expectation
expectGlobalAndModuleErrors { global, modules } reviewResult =
    let
        expectations : List ReviewExpectation
        expectations =
            List.foldl (\( moduleName, errors ) acc -> moduleErrors moduleName errors :: acc)
                [ globalErrors global ]
                modules
    in
    expect expectations reviewResult


checkResultsAreTheSameWhenIgnoringFiles : List ReviewError -> ReRun -> Expectation
checkResultsAreTheSameWhenIgnoringFiles allErrors reRun =
    case reRun of
        AttemptReRun rule project ->
            if not (Rule.ruleKnowsAboutIgnoredFiles rule) then
                Expect.pass

            else
                doCheckResultsAreTheSameWhenIgnoringFiles allErrors rule project

        DontAttemptReRun ->
            Expect.pass


doCheckResultsAreTheSameWhenIgnoringFiles : List ReviewError -> Rule -> Project -> Expectation
doCheckResultsAreTheSameWhenIgnoringFiles allErrors rule project =
    let
        filePaths : List String
        filePaths =
            Project.modules project
                |> List.map .path
                |> maybeCons .path (Project.elmJson project)
                -- TODO Add extra files? Are there other remaining places?
                |> maybeCons .path (Project.readme project)

        combinationsOfFilesToIgnore : List (List String)
        combinationsOfFilesToIgnore =
            allCombinations filePaths
    in
    if List.isEmpty combinationsOfFilesToIgnore then
        Expect.pass

    else
        Expect.all
            (List.map (\filesToIgnore () -> checkResultsAreTheSameWhenIgnoringFilesHelp rule project allErrors filesToIgnore) combinationsOfFilesToIgnore)
            ()


checkResultsAreTheSameWhenIgnoringFilesHelp : Rule -> Project -> List ReviewError -> List String -> Expectation
checkResultsAreTheSameWhenIgnoringFilesHelp rule project allErrors filesToIgnore =
    let
        ( missing, unexpected ) =
            Rule.reviewV3
                (ReviewOptions.withDataExtraction False ReviewOptions.defaults)
                [ Rule.ignoreErrorsForFiles filesToIgnore rule ]
                project
                |> .errors
                |> removeInCommon (removeErrorsForIgnoredFiles filesToIgnore allErrors) []
    in
    if List.isEmpty missing && List.isEmpty unexpected then
        Expect.pass

    else
        Expect.fail (FailureMessage.resultsAreDifferentWhenFilesAreIgnored { ignoredFiles = filesToIgnore, missing = missing, unexpected = unexpected })


removeInCommon : List a -> List a -> List a -> ( List a, List a )
removeInCommon expected excessFromActual actual =
    case actual of
        [] ->
            ( expected, excessFromActual )

        first :: restOfActual ->
            case removeFirst first expected [] of
                Just newExpected ->
                    removeInCommon newExpected excessFromActual restOfActual

                Nothing ->
                    removeInCommon expected (first :: excessFromActual) restOfActual


removeFirst : a -> List a -> List a -> Maybe (List a)
removeFirst a list prev =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x == a then
                Just (prev ++ xs)

            else
                removeFirst a xs (x :: prev)


removeErrorsForIgnoredFiles : List String -> List ReviewError -> List ReviewError
removeErrorsForIgnoredFiles filesToIgnore errors =
    List.filter
        (\err -> not (List.member (Rule.errorFilePath err) filesToIgnore))
        errors


allCombinations : List a -> List (List a)
allCombinations list =
    case list of
        [] ->
            []

        [ _ ] ->
            []

        _ ->
            allCombinationsHelp list


allCombinationsHelp : List a -> List (List a)
allCombinationsHelp list =
    case list of
        [] ->
            []

        first :: rest ->
            let
                addFirst : List a -> List (List a) -> List (List a)
                addFirst subList acc =
                    subList :: (first :: subList) :: acc
            in
            [ first ] :: List.foldr addFirst [] (allCombinationsHelp rest)


maybeCons : (b -> a) -> Maybe b -> List a -> List a
maybeCons mapper maybe list =
    case maybe of
        Just b ->
            mapper b :: list

        Nothing ->
            list


checkErrorsForModules : ProjectInternals -> List ( String, List ExpectedError ) -> List SuccessfulRunResult -> Expectation
checkErrorsForModules project expectedErrorsList runResults =
    let
        unknownModules : List String
        unknownModules =
            Set.diff
                (expectedErrorsList |> List.map Tuple.first |> Set.fromList)
                (Set.fromList (List.map .moduleName runResults))
                |> Set.toList
    in
    case unknownModules of
        unknownModule :: _ ->
            FailureMessage.unknownModulesInExpectedErrors unknownModule
                |> Expect.fail

        [] ->
            Expect.all
                (checkErrorsForModuleFiles project expectedErrorsList runResults)
                ()


checkErrorsForModuleFiles : ProjectInternals -> List ( String, List ExpectedError ) -> List SuccessfulRunResult -> List (() -> Expectation)
checkErrorsForModuleFiles project expectedErrorsList runResults =
    List.map
        (\runResult () ->
            let
                expectedErrors : List ExpectedError
                expectedErrors =
                    expectedErrorsList
                        |> ListExtra.find (\( moduleName, _ ) -> moduleName == runResult.moduleName)
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault []
            in
            if List.isEmpty expectedErrors then
                checkNoErrorForModuleRunResult runResult

            else
                checkAllErrorsMatch project runResult expectedErrors
        )
        runResults


{-| Assert that the rule reported some errors for the `elm.json` file, by specifying which ones.

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`elmJsonErrors`](#elmJsonErrors) functions.

    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "report an error when a module is unused" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addElmJson elmJsonToConstructManually
                in
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrorsForElmJson
                        [ Review.Test.error
                            { message = "Unused dependency `author/package`"
                            , details = [ "Dependency should be removed" ]
                            , under = "author/package"
                            }
                        ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
expectErrorsForElmJson : List ExpectedError -> ReviewResult -> Expectation
expectErrorsForElmJson expectedErrors reviewResult =
    expect [ elmJsonErrors expectedErrors ] reviewResult


{-| Assert that the rule reported some [global errors](./Review-Rule#globalError), by specifying which ones.

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`globalErrors`](#globalErrors) functions.
If you need to specify fixes for the global error, you will want to use [`globalErrorsWithFixes`](#globalErrorsWithFixes) instead.

Assert which errors are reported using records with the expected message and details. The test will fail if
a different number of errors than expected are reported, or if the message or details is incorrect.

    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report a global error when the specified module could not be found" <|
            \() ->
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.run (rule "ModuleB")
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find module ModuleB"
                          , details =
                                [ "You mentioned the module ModuleB in the configuration of this rule, but it could not be found."
                                , "This likely means you misconfigured the rule or the configuration has become out of date with recent changes in your project."
                                ]
                          }
                        ]

-}
expectGlobalErrors : List { message : String, details : List String } -> ReviewResult -> Expectation
expectGlobalErrors expectedErrors reviewResult =
    expect [ globalErrors expectedErrors ] reviewResult


{-| Assert that the rule reported some [global errors](./Review-Rule#globalError), by specifying which ones.

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`globalErrors`](#globalErrors) functions.

Assert which errors are reported using records with the expected message and details. The test will fail if
a different number of errors than expected are reported, or if the message or details is incorrect.

    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report a global error when the specified module could not be found" <|
            \() ->
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.run (rule "ModuleB")
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find module ModuleB"
                          , details =
                                [ "You mentioned the module ModuleB in the configuration of this rule, but it could not be found."
                                , "This likely means you misconfigured the rule or the configuration has become out of date with recent changes in your project."
                                ]
                          }
                        ]

-}
expectGlobalErrorsWithFixes : List { message : String, details : List String, fixes : List ( String, ExpectedFix ) } -> ReviewResult -> Expectation
expectGlobalErrorsWithFixes expectedErrors reviewResult =
    expect [ globalErrorsWithFixes expectedErrors ] reviewResult


{-| Assert that the rule reported some errors for the `README.md` file, by specifying which ones.

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`readmeErrors`](#readmeErrors) functions.

    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "report an error when a module is unused" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addReadme { path = "README.md", context = "# Project\n..." }
                in
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Invalid link"
                            , details = [ "README contains an invalid link" ]
                            , under = "htt://example.com"
                            }
                        ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
expectErrorsForReadme : List ExpectedError -> ReviewResult -> Expectation
expectErrorsForReadme expectedErrors reviewResult =
    expect [ readmeErrors expectedErrors ] reviewResult


{-| Assert that the rule reported some errors for an extra file, by specifying which ones.

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`extraFileErrors`](#extraFileErrors) functions.

    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "report an error when a module is unused" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addExtraFile { path = "some-file.ext", context = "some string" }
                in
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrorsForExtraFile "some-file.ext"
                        [ Review.Test.error
                            { message = "Some message"
                            , details = [ "Some details" ]
                            , under = "some string"
                            }
                        ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
expectErrorsForExtraFile : String -> List ExpectedError -> ReviewResult -> Expectation
expectErrorsForExtraFile filePath expectedErrors reviewResult =
    expect [ extraFileErrors filePath expectedErrors ] reviewResult


{-| Create an expectation for an error.

`message` should be the message you're expecting to be shown to the user.

`under` is the part of the code where you are expecting the error to be shown to
the user. If it helps, imagine `under` to be the text under which the squiggly
lines will appear if the error appeared in an editor.

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should report Debug.log use" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "some" "message\""""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
            ]

If there are multiple locations where the value of `under` appears, the test will
fail unless you use [`atExactly`](#atExactly) to remove any ambiguity of where the
error should be used.

-}
error : { message : String, details : List String, under : String } -> ExpectedError
error input =
    ExpectedError
        { message = input.message
        , details = input.details
        , under = Under input.under
        , expectedFixes = NoFixesExpected
        }


{-| Precise the exact position where the error should be shown to the user. This
is only necessary when the `under` field is ambiguous.

`atExactly` takes a record with start and end positions.

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should report multiple Debug.log calls" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "foo" z
    b = Debug.log "foo" z
    """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                                |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
                            , Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                                |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                            ]
            ]

Tip: By default, do not use this function. If the test fails because there is some
ambiguity, the test error will give you a recommendation of what to use as a parameter
of `atExactly`, so you do not have to bother writing this hard-to-write argument yourself.

-}
atExactly : { start : { row : Int, column : Int }, end : { row : Int, column : Int } } -> ExpectedError -> ExpectedError
atExactly range (ExpectedError expectedError) =
    ExpectedError { expectedError | under = UnderExactly (formatUnder expectedError.under) range }


{-| Create an expectation that the error provides an automatic fix, meaning that it used
functions like [`errorWithFix`](./Review-Rule#errorWithFix), and an expectation of what the source
code should be after the error's fix has been applied.

In the absence of `whenFixed` or `shouldFixFiles`, the test will fail if the error provides a fix.
In other words, you only need to use this function if the error provides a fix.

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should report multiple Debug.log calls" <|
                \() ->
                    """module A exposing (..)
    a = 1
    b = Debug.log "foo" 2
    """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                                |> Review.Test.whenFixed """module SomeModule exposing (b)
    a = 1
    b = 2
    """
                            ]
            ]

-}
whenFixed : String -> ExpectedError -> ExpectedError
whenFixed fixedSource (ExpectedError expectedError) =
    ExpectedError { expectedError | expectedFixes = ComesFromWhenFixed fixedSource }


{-| Create an expectation that the error provides automatic fixes for one or several files, meaning that it used
functions like [`withFixesV2`](./Review-Rule#withFixesV2), and an expectation of what the source
code should be after the error's fix has been applied.

In the absence of `whenFixed` or `shouldFixFiles`, the test will fail if the error provides a fix.
In other words, you only need to use this function if the error provides a fix.

The first element in the tuples is the file to be fixed, which can be either the module name or the file path.
The second element in the tuples is the expected source code.

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should report multiple Debug.log calls" <|
                \() ->
                    [ """module A exposing (main)
    import Other exposing (Msg(..))
    a = Used
    main = case foo of
      Unused -> 1
      Used -> 2
    """
                    , """module Other exposing (Msg(..))
    type Msg = Unused | Used
    """
                    ]
                        |> Review.Test.runOnModules rule
                        |> Review.Test.expectErrorsForModules
                            [ ( "A"
                              , [ Review.Test.error
                                    { message = "Remove the use of `Debug` before shipping to production"
                                    , details = [ "Details about the error" ]
                                    , under = "Debug.log"
                                    }
                                    |> Review.Test.shouldFixFiles
                                        [ ( "A", """module A exposing (main)
    import Other exposing (Msg(..))
    a = Used
    main = case foo of
      $
      Used -> 2
    """ )
                                        , ( "src/Other.elm", """module Other exposing (Msg(..))
    type Msg = Used
    """ )
                                        ]
                                ]
                              )
                            ]
            ]

-}
shouldFixFiles : List ( String, String ) -> ExpectedError -> ExpectedError
shouldFixFiles expectedFixes (ExpectedError expectedError) =
    ExpectedError
        { expectedError
            | expectedFixes =
                expectedFixes
                    |> List.map (\( path, expectedSource ) -> ( path, ExpectEdited expectedSource ))
                    |> Dict.fromList
                    |> ComesFromShouldFixFiles
        }


{-| TODO MULTIFILE-FIXES Update documentation
-}
type ExpectedFix
    = ExpectEdited String
    | ExpectRemoved


{-| TODO MULTIFILE-FIXES Update documentation
-}
shouldFixFilesWithIO : List ( String, ExpectedFix ) -> ExpectedError -> ExpectedError
shouldFixFilesWithIO expectedFixes (ExpectedError expectedError) =
    ExpectedError { expectedError | expectedFixes = ComesFromShouldFixFiles (Dict.fromList expectedFixes) }


{-| TODO MULTIFILE-FIXES Update documentation
-}
edited : String -> ExpectedFix
edited =
    ExpectEdited


{-| TODO MULTIFILE-FIXES Update documentation
-}
removed : ExpectedFix
removed =
    ExpectRemoved


formatUnder : Under -> String
formatUnder under =
    case under of
        Under str ->
            str

        UnderExactly str _ ->
            str


getCodeAtLocationInSourceCode : SourceCode -> Range -> Maybe String
getCodeAtLocationInSourceCode sourceCode =
    let
        lines : Array String
        lines =
            String.lines sourceCode
                |> Array.fromList
    in
    \{ start, end } ->
        if start.row == end.row then
            Array.get (start.row - 1) lines
                |> Maybe.map (Unicode.slice (start.column - 1) (end.column - 1))

        else
            let
                firstLine : String
                firstLine =
                    case Array.get (start.row - 1) lines of
                        Just str ->
                            Unicode.dropLeft (start.column - 1) str

                        Nothing ->
                            ""

                lastLine : String
                lastLine =
                    case Array.get (end.row - 1) lines of
                        Just str ->
                            Unicode.left end.column str

                        Nothing ->
                            ""

                resultingLines : List String
                resultingLines =
                    if start.row + 1 == end.row then
                        [ firstLine
                        , lastLine
                        ]

                    else
                        [ firstLine
                        , Array.slice start.row (end.row - 1) lines
                            |> Array.toList
                            |> String.join "\n"
                        , lastLine
                        ]
            in
            resultingLines
                |> String.join "\n"
                |> Just


checkIfLocationIsAmbiguousInSourceCode : SourceCode -> ReviewError -> String -> Expectation
checkIfLocationIsAmbiguousInSourceCode sourceCode =
    \error_ under ->
        let
            occurrencesInSourceCode : List Int
            occurrencesInSourceCode =
                String.indexes under sourceCode
        in
        hasOneElement occurrencesInSourceCode
            |> Expect.equal True
            |> onFail (\() -> FailureMessage.locationIsAmbiguousInSourceCode sourceCode error_ under occurrencesInSourceCode)



-- RUNNING THE CHECKS


type alias ReorderState =
    { expectedErrors : List ExpectedError
    , reviewErrors : List ReviewError
    , pairs : List ( ExpectedError, ReviewError )
    , expectedErrorsWithNoMatch : List ExpectedError
    }


reorderErrors : CodeInspector -> ReorderState -> ( List ExpectedError, List ReviewError )
reorderErrors codeInspector reorderState =
    case reorderState.expectedErrors of
        [] ->
            ( List.reverse <| reorderState.expectedErrorsWithNoMatch ++ List.map Tuple.first reorderState.pairs
            , List.reverse <| reorderState.reviewErrors ++ List.map Tuple.second reorderState.pairs
            )

        ((ExpectedError expectedErrorDetails) as expectedError) :: restOfExpectedErrors ->
            case findBestMatchingReviewError codeInspector expectedErrorDetails reorderState.reviewErrors { error = Nothing, confidenceLevel = 0 } of
                Just reviewError ->
                    reorderErrors codeInspector
                        { reorderState
                            | pairs = ( expectedError, reviewError ) :: reorderState.pairs
                            , reviewErrors = removeFirstOccurrence reviewError reorderState.reviewErrors
                            , expectedErrors = restOfExpectedErrors
                        }

                Nothing ->
                    reorderErrors codeInspector
                        { reorderState
                            | expectedErrorsWithNoMatch = expectedError :: reorderState.expectedErrorsWithNoMatch
                            , expectedErrors = restOfExpectedErrors
                        }


removeFirstOccurrence : a -> List a -> List a
removeFirstOccurrence elementToRemove list =
    case list of
        [] ->
            []

        x :: xs ->
            if x == elementToRemove then
                xs

            else
                x :: removeFirstOccurrence elementToRemove xs


findBestMatchingReviewError : CodeInspector -> ExpectedErrorDetails -> List ReviewError -> { error : Maybe ReviewError, confidenceLevel : Int } -> Maybe ReviewError
findBestMatchingReviewError codeInspector expectedErrorDetails reviewErrors bestMatch =
    case reviewErrors of
        [] ->
            bestMatch.error

        reviewError :: restOfReviewErrors ->
            let
                confidenceLevel : Int
                confidenceLevel =
                    matchingConfidenceLevel codeInspector expectedErrorDetails reviewError
            in
            if confidenceLevel > bestMatch.confidenceLevel then
                findBestMatchingReviewError
                    codeInspector
                    expectedErrorDetails
                    restOfReviewErrors
                    { error = Just reviewError, confidenceLevel = confidenceLevel }

            else
                findBestMatchingReviewError
                    codeInspector
                    expectedErrorDetails
                    restOfReviewErrors
                    bestMatch


matchingConfidenceLevel : CodeInspector -> ExpectedErrorDetails -> ReviewError -> Int
matchingConfidenceLevel codeInspector expectedErrorDetails reviewError =
    if expectedErrorDetails.message /= Rule.errorMessage reviewError then
        0

    else
        case expectedErrorDetails.under of
            Under under ->
                if codeInspector.getCodeAtLocation (Rule.errorRange reviewError) /= Just under then
                    1

                else
                    2

            UnderExactly under range ->
                if codeInspector.getCodeAtLocation (Rule.errorRange reviewError) /= Just under then
                    1

                else if range /= Rule.errorRange reviewError then
                    2

                else
                    3


checkAllErrorsMatch : ProjectInternals -> SuccessfulRunResult -> List ExpectedError -> Expectation
checkAllErrorsMatch project runResult unorderedExpectedErrors =
    let
        ( expectedErrors, reviewErrors ) =
            reorderErrors
                runResult.inspector
                { expectedErrors = unorderedExpectedErrors
                , reviewErrors = runResult.errors
                , pairs = []
                , expectedErrorsWithNoMatch = []
                }
    in
    Expect.all
        (List.reverse (checkErrorsMatch project runResult expectedErrors (List.length expectedErrors) reviewErrors))
        ()


checkGlobalErrorsMatch : ProjectInternals -> Int -> { expected : List GlobalError, actual : List ReviewError, needSecondPass : List GlobalError } -> Expectation
checkGlobalErrorsMatch project originalNumberOfExpectedErrors params =
    case params.expected of
        head :: rest ->
            case findAndRemove (\error_ -> Rule.errorMessage error_ == head.message && Rule.errorDetails error_ == head.details) params.actual of
                Just ( (ReviewError { fixes }) as matchedError, newActual ) ->
                    if List.isEmpty head.details then
                        Expect.fail (FailureMessage.emptyDetails head.message)

                    else
                        case checkGlobalErrorFixesMatch project matchedError head.fixes (ErrorFixes.toList fixes) of
                            Err failure ->
                                Expect.fail failure

                            Ok () ->
                                checkGlobalErrorsMatch project originalNumberOfExpectedErrors { expected = rest, actual = newActual, needSecondPass = params.needSecondPass }

                Nothing ->
                    checkGlobalErrorsMatch project originalNumberOfExpectedErrors { expected = rest, actual = params.actual, needSecondPass = head :: params.needSecondPass }

        [] ->
            case params.actual of
                firstActual :: restOfActual ->
                    case List.head (List.reverse params.needSecondPass) of
                        Just notFoundError ->
                            failBecauseExpectedErrorCouldNotBeFound notFoundError ( firstActual, restOfActual )

                        Nothing ->
                            Expect.fail <| FailureMessage.tooManyGlobalErrors (List.map Rule.errorMessage params.actual)

                [] ->
                    if List.isEmpty params.needSecondPass then
                        -- All expected errors were found
                        Expect.pass

                    else
                        -- We expected more errors
                        Expect.fail <| FailureMessage.expectedMoreGlobalErrors originalNumberOfExpectedErrors (List.map .message params.needSecondPass)


findAndRemove : (a -> Bool) -> List a -> Maybe ( a, List a )
findAndRemove fn list =
    findAndRemoveHelp fn [] list


findAndRemoveHelp : (a -> Bool) -> List a -> List a -> Maybe ( a, List a )
findAndRemoveHelp fn previous list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if fn head then
                Just ( head, List.reverse previous ++ rest )

            else
                findAndRemoveHelp fn (head :: previous) rest


failBecauseExpectedErrorCouldNotBeFound : GlobalError -> ( ReviewError, List ReviewError ) -> Expectation
failBecauseExpectedErrorCouldNotBeFound expectedError ( firstActual, restOfActual ) =
    case ListExtra.find (\e -> Rule.errorMessage e == expectedError.message) (firstActual :: restOfActual) of
        Just actualErrorWithTheSameMessage ->
            FailureMessage.unexpectedGlobalErrorDetails expectedError.details
                { message = Rule.errorMessage actualErrorWithTheSameMessage
                , details = Rule.errorDetails actualErrorWithTheSameMessage
                }
                |> Expect.fail

        Nothing ->
            FailureMessage.messageMismatch
                { target = FailureMessage.Global
                , expected = expectedError.message
                , actual = Rule.errorMessage firstActual
                }
                |> Expect.fail


checkAllGlobalErrorsMatch : ProjectInternals -> Int -> { expected : List GlobalError, actual : List ReviewError } -> Expectation
checkAllGlobalErrorsMatch project originalNumberOfExpectedErrors params =
    checkGlobalErrorsMatch project originalNumberOfExpectedErrors { expected = params.expected, actual = params.actual, needSecondPass = [] }


checkErrorsMatch : ProjectInternals -> SuccessfulRunResult -> List ExpectedError -> Int -> List ReviewError -> List (() -> Expectation)
checkErrorsMatch project runResult expectedErrors expectedNumberOfErrors errors =
    case ( expectedErrors, errors ) of
        ( [], [] ) ->
            [ always Expect.pass ]

        ( expected :: restOfExpectedErrors, error_ :: restOfErrors ) ->
            checkErrorMatch project runResult expected error_
                :: checkErrorsMatch project runResult restOfExpectedErrors expectedNumberOfErrors restOfErrors

        ( _ :: _, [] ) ->
            [ \() ->
                expectedErrors
                    |> List.map (\(ExpectedError { message }) -> message)
                    |> FailureMessage.expectedMoreErrors runResult.moduleName expectedNumberOfErrors
                    |> Expect.fail
            ]

        ( [], error_ :: restOfErrors ) ->
            [ \() ->
                FailureMessage.tooManyErrors runResult.moduleName (error_ :: restOfErrors)
                    |> Expect.fail
            ]


checkErrorMatch : ProjectInternals -> SuccessfulRunResult -> ExpectedError -> ReviewError -> (() -> Expectation)
checkErrorMatch project runResult (ExpectedError expectedError) error_ =
    Expect.all
        [ -- Error message
          \() ->
            Rule.errorMessage error_
                |> Expect.equal expectedError.message
                |> onFail
                    (\() ->
                        FailureMessage.messageMismatch
                            { target = FailureMessage.Module runResult.moduleName
                            , expected = expectedError.message
                            , actual = Rule.errorMessage error_
                            }
                    )
        , \() -> checkMessageAppearsUnder runResult.inspector error_ expectedError

        -- Error details
        , \() ->
            case Rule.errorDetails error_ of
                [] ->
                    Expect.fail (FailureMessage.emptyDetails (Rule.errorMessage error_))

                errorDetails ->
                    errorDetails
                        |> Expect.equal expectedError.details
                        |> onFail (\() -> FailureMessage.unexpectedDetails expectedError.details error_)

        -- Error fixes
        , \() -> checkFixesHaveNoProblem error_
        , \() -> checkFixesAreCorrect project runResult.moduleName error_ expectedError
        ]


checkMessageAppearsUnder : CodeInspector -> ReviewError -> ExpectedErrorDetails -> Expectation
checkMessageAppearsUnder codeInspector error_ expectedError =
    if Rule.errorTarget error_ == Target.UserGlobal then
        Expect.pass

    else
        case codeInspector.getCodeAtLocation (Rule.errorRange error_) of
            Just codeAtLocation ->
                case expectedError.under of
                    Under under ->
                        Expect.all
                            [ \() ->
                                case under of
                                    "" ->
                                        FailureMessage.underMayNotBeEmpty
                                            { message = expectedError.message
                                            , codeAtLocation = codeAtLocation
                                            }
                                            |> Expect.fail

                                    _ ->
                                        Expect.pass
                            , \() ->
                                codeAtLocation
                                    |> Expect.equal under
                                    |> onFail (\() -> FailureMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                            , \() ->
                                codeInspector.checkIfLocationIsAmbiguous error_ under
                            ]
                            ()

                    UnderExactly under range ->
                        Expect.all
                            [ \() ->
                                codeAtLocation
                                    |> Expect.equal under
                                    |> onFail (\() -> FailureMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                            , \() ->
                                Rule.errorRange error_
                                    |> Expect.equal range
                                    |> onFail (\() -> FailureMessage.wrongLocation error_ range under)
                            ]
                            ()

            Nothing ->
                FailureMessage.locationNotFound error_
                    |> Expect.fail


checkFixesHaveNoProblem : ReviewError -> Expectation
checkFixesHaveNoProblem ((ReviewError err) as error_) =
    case err.fixProblem of
        Just fixProblem ->
            Expect.fail <| FailureMessage.fixProblem_ fixProblem error_

        Nothing ->
            Expect.pass


checkFixesAreCorrect : ProjectInternals -> String -> ReviewError -> ExpectedErrorDetails -> Expectation
checkFixesAreCorrect project moduleName ((ReviewError err) as error_) expectedError =
    let
        errorFixes : List ( FileTarget, ErrorFixes.FixKind )
        errorFixes =
            ErrorFixes.toList err.fixes
    in
    if List.isEmpty errorFixes then
        case expectedError.expectedFixes of
            NoFixesExpected ->
                Expect.pass

            ComesFromWhenFixed expectedFixedModule ->
                Expect.fail
                    (FailureMessage.missingFixes
                        { target = FailureMessage.Module moduleName
                        , message = expectedError.message
                        , expectedFixedModules = [ expectedFixedModule ]
                        }
                    )

            ComesFromShouldFixFiles expectedFixedModules ->
                Expect.fail
                    (FailureMessage.missingFixes
                        { target = FailureMessage.Module moduleName
                        , message = expectedError.message
                        , expectedFixedModules = Dict.keys expectedFixedModules
                        }
                    )

    else
        case expectedError.expectedFixes of
            NoFixesExpected ->
                FailureMessage.unexpectedFixes (Rule.errorMessage error_)
                    |> Expect.fail

            ComesFromWhenFixed fixedSource ->
                checkFixesMatch
                    project
                    moduleName
                    error_
                    (Dict.singleton err.filePath (ExpectEdited fixedSource))
                    errorFixes

            ComesFromShouldFixFiles expectedFixes ->
                checkFixesMatch
                    project
                    moduleName
                    error_
                    expectedFixes
                    errorFixes


checkFixesMatch : ProjectInternals -> String -> ReviewError -> Dict String ExpectedFix -> List ( FileTarget, ErrorFixes.FixKind ) -> Expectation
checkFixesMatch project moduleName error_ expectedFixed fixes =
    case fixes of
        [] ->
            if Dict.isEmpty expectedFixed then
                Expect.pass

            else
                FailureMessage.missingFixes
                    { target = FailureMessage.Module moduleName
                    , message = Rule.errorMessage error_
                    , expectedFixedModules = Dict.keys expectedFixed
                    }
                    |> Expect.fail

        ( target, ErrorFixes.Edit fileFixes ) :: rest ->
            case getTargetFileFromProject target project of
                Just targetInformation ->
                    case getExpectedFixedCodeThroughFilePathOrModuleName (FileTarget.filePath target) targetInformation.moduleName expectedFixed of
                        Just ( key, ExpectEdited expectedFix ) ->
                            case fixOneError target fileFixes targetInformation.source expectedFix error_ of
                                Err failureMessage ->
                                    Expect.fail failureMessage

                                Ok () ->
                                    checkFixesMatch
                                        project
                                        moduleName
                                        error_
                                        (Dict.remove key expectedFixed)
                                        rest

                        Just ( key, ExpectRemoved ) ->
                            FailureMessage.fileWasEditedInsteadOfRemoved
                                { target = FailureMessage.Module moduleName
                                , message = Rule.errorMessage error_
                                , nameOfFixedFile = key
                                , fixedSource = targetInformation.source
                                }
                                |> Expect.fail

                        Nothing ->
                            FailureMessage.unexpectedAdditionalFixes
                                { moduleName = moduleName
                                , message = Rule.errorMessage error_
                                , nameOfFixedFile = FileTarget.filePath target
                                , fixedSource = targetInformation.source
                                }
                                |> Expect.fail

                Nothing ->
                    FailureMessage.fixForUnknownFile (FileTarget.filePath target)
                        |> Expect.fail

        ( target, ErrorFixes.Remove ) :: rest ->
            case getTargetFileFromProject target project of
                Just targetInformation ->
                    case getExpectedFixedCodeThroughFilePathOrModuleName (FileTarget.filePath target) targetInformation.moduleName expectedFixed of
                        Just ( key, ExpectRemoved ) ->
                            checkFixesMatch
                                project
                                moduleName
                                error_
                                (Dict.remove key expectedFixed)
                                rest

                        Just ( key, ExpectEdited _ ) ->
                            FailureMessage.fileWasRemovedInsteadOfEdited
                                { target = FailureMessage.Module moduleName
                                , message = Rule.errorMessage error_
                                , nameOfFixedFile = key
                                }
                                |> Expect.fail

                        Nothing ->
                            FailureMessage.unexpectedAdditionalFixes
                                { moduleName = moduleName
                                , message = Rule.errorMessage error_
                                , nameOfFixedFile = FileTarget.filePath target
                                , fixedSource = targetInformation.source
                                }
                                |> Expect.fail

                Nothing ->
                    FailureMessage.fixForUnknownFile (FileTarget.filePath target)
                        |> Expect.fail


checkGlobalErrorFixesMatch : ProjectInternals -> ReviewError -> Dict String ExpectedFix -> List ( FileTarget, ErrorFixes.FixKind ) -> Result String ()
checkGlobalErrorFixesMatch project error_ expectedFixed fixes =
    case fixes of
        [] ->
            if Dict.isEmpty expectedFixed then
                Ok ()

            else
                FailureMessage.missingFixes
                    { target = FailureMessage.Global
                    , message = Rule.errorMessage error_
                    , expectedFixedModules = Dict.keys expectedFixed
                    }
                    |> Err

        ( target, ErrorFixes.Edit fileFixes ) :: rest ->
            case getTargetFileFromProject target project of
                Just targetInformation ->
                    case getExpectedFixedCodeThroughFilePathOrModuleName (FileTarget.filePath target) targetInformation.moduleName expectedFixed of
                        Just ( key, ExpectEdited expectedFix ) ->
                            case fixOneError target fileFixes targetInformation.source expectedFix error_ of
                                Err failureMessage ->
                                    Err failureMessage

                                Ok () ->
                                    checkGlobalErrorFixesMatch
                                        project
                                        error_
                                        (Dict.remove key expectedFixed)
                                        rest

                        Just ( key, ExpectRemoved ) ->
                            FailureMessage.fileWasEditedInsteadOfRemoved
                                { target = FailureMessage.Global
                                , message = Rule.errorMessage error_
                                , nameOfFixedFile = key
                                , fixedSource = targetInformation.source
                                }
                                |> Err

                        Nothing ->
                            FailureMessage.unexpectedAdditionalFixesForGlobalError
                                { message = Rule.errorMessage error_
                                , nameOfFixedFile = FileTarget.filePath target
                                , fixedSource = targetInformation.source
                                }
                                |> Err

                Nothing ->
                    FailureMessage.fixForUnknownFile (FileTarget.filePath target)
                        |> Err

        ( target, ErrorFixes.Remove ) :: rest ->
            case getTargetFileFromProject target project of
                Just targetInformation ->
                    case getExpectedFixedCodeThroughFilePathOrModuleName (FileTarget.filePath target) targetInformation.moduleName expectedFixed of
                        Just ( key, ExpectRemoved ) ->
                            checkGlobalErrorFixesMatch
                                project
                                error_
                                (Dict.remove key expectedFixed)
                                rest

                        Just ( key, ExpectEdited _ ) ->
                            FailureMessage.fileWasRemovedInsteadOfEdited
                                { target = FailureMessage.Global
                                , message = Rule.errorMessage error_
                                , nameOfFixedFile = key
                                }
                                |> Err

                        Nothing ->
                            FailureMessage.unexpectedAdditionalFixesForGlobalError
                                { message = Rule.errorMessage error_
                                , nameOfFixedFile = FileTarget.filePath target
                                , fixedSource = targetInformation.source
                                }
                                |> Err

                Nothing ->
                    FailureMessage.fixForUnknownFile (FileTarget.filePath target)
                        |> Err


getExpectedFixedCodeThroughFilePathOrModuleName : String -> Maybe String -> Dict String ExpectedFix -> Maybe ( String, ExpectedFix )
getExpectedFixedCodeThroughFilePathOrModuleName filePath moduleName expectedFixed =
    case Dict.get filePath expectedFixed of
        Just expectedFix ->
            Just ( filePath, expectedFix )

        Nothing ->
            moduleName
                |> Maybe.andThen
                    (\fixTargetModuleName ->
                        Dict.get fixTargetModuleName expectedFixed
                            |> Maybe.map (Tuple.pair fixTargetModuleName)
                    )


fixOneError : FileTarget -> List Fix -> String -> String -> ReviewError -> Result String ()
fixOneError target fileFixes source expectedFixedSource error_ =
    case FixInternal.fix target fileFixes source of
        Ok fixedSource ->
            let
                trimmedFixed : String
                trimmedFixed =
                    trimRightOnEveryLine fixedSource

                expectedFixed : String
                expectedFixed =
                    trimRightOnEveryLine expectedFixedSource
            in
            if trimmedFixed == expectedFixed then
                Ok ()

            else if removeWhitespace trimmedFixed == removeWhitespace expectedFixed then
                Err <| FailureMessage.fixedCodeWhitespaceMismatch trimmedFixed expectedFixed error_

            else
                Err <| FailureMessage.fixedCodeMismatch trimmedFixed expectedFixed error_

        Err fixProblem ->
            Err <| FailureMessage.fixProblem_ fixProblem error_


trimRightOnEveryLine : String -> String
trimRightOnEveryLine source =
    Regex.replace whitespaceBeforeNewLine (always "") source


whitespaceBeforeNewLine : Regex
whitespaceBeforeNewLine =
    Regex.fromStringWith { caseInsensitive = False, multiline = True } "\\s+$"
        |> Maybe.withDefault Regex.never


getTargetFileFromProject : FileTarget -> ProjectInternals -> Maybe { source : String, moduleName : Maybe String }
getTargetFileFromProject target project =
    case target of
        FileTarget.Module filePath ->
            Dict.get filePath project.modules
                |> Maybe.map
                    (\module_ ->
                        { source = ProjectModule.source module_
                        , moduleName = Just (String.join "." (ProjectModule.moduleName module_))
                        }
                    )

        FileTarget.ElmJson ->
            Maybe.map (\( { raw }, _ ) -> { source = raw, moduleName = Nothing }) project.elmJson

        FileTarget.Readme ->
            Maybe.map (\( { content }, _ ) -> { source = content, moduleName = Nothing }) project.readme

        FileTarget.ExtraFile filePath ->
            Dict.get filePath project.extraFiles
                |> Maybe.map (\content -> { source = content, moduleName = Nothing })


removeWhitespace : String -> String
removeWhitespace str =
    str
        |> String.replace " " ""
        |> String.replace "\n" ""


{-| Assert that the rule will report a configuration error.

    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report a configuration error when argument is empty" <|
            \() ->
                rule ""
                    |> Review.Test.expectConfigurationError
                        { message = "Configuration argument should not be empty"
                        , details = [ "Some details" ]
                        }

-}
expectConfigurationError : { message : String, details : List String } -> Rule -> Expectation
expectConfigurationError expectedError rule =
    case Rule.getConfigurationError rule of
        Just configurationError ->
            checkConfigurationErrorDetailsMatch expectedError configurationError

        Nothing ->
            Expect.fail (FailureMessage.missingConfigurationError expectedError.message)


checkConfigurationErrorDetailsMatch : { message : String, details : List String } -> { message : String, details : List String } -> Expectation
checkConfigurationErrorDetailsMatch expectedError configurationError =
    if expectedError.message /= configurationError.message then
        Expect.fail (FailureMessage.messageMismatchForConfigurationError expectedError configurationError)

    else if List.isEmpty configurationError.details then
        Expect.fail (FailureMessage.emptyDetails configurationError.message)

    else if expectedError.details /= configurationError.details then
        Expect.fail (FailureMessage.unexpectedConfigurationErrorDetails expectedError.details configurationError)

    else
        Expect.pass


checkNoDataExtract : ExtractResult -> Expectation
checkNoDataExtract maybeExtract =
    case maybeExtract of
        Just extract ->
            Expect.fail (FailureMessage.unexpectedExtract extract)

        Nothing ->
            Expect.pass


{-| Expect the rule to produce a specific data extract.

If you expect the rule to also report errors, then you should use the [`Review.Test.expect`](#expect) and [`dataExtract`](#dataExtract) functions.

Note: You do not need to match the exact formatting of the JSON object, though the order of fields does need to match.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        test "should extract the list of fields" <|
            \() ->
                """module A exposing (..)
    a = 1
    b = 2
    """
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
    {
        "fields": [ "a", "b" ]
    }"""

-}
expectDataExtract : String -> ReviewResult -> Expectation
expectDataExtract expectedExtract reviewResult =
    expect [ dataExtract expectedExtract ] reviewResult


checkDataExtractContent : String -> ExtractResult -> Expectation
checkDataExtractContent rawExpected maybeActualExtract =
    case maybeActualExtract of
        Nothing ->
            Expect.fail FailureMessage.missingExtract

        Just actual ->
            case Decode.decodeString Decode.value rawExpected of
                Err parsingError ->
                    Expect.fail (FailureMessage.invalidJsonForExpectedDataExtract parsingError)

                Ok expected ->
                    let
                        differences : List (Diff.Change String)
                        differences =
                            Diff.diffLines (Encode.encode 2 actual) (Encode.encode 2 expected)
                    in
                    if containsDifferences differences then
                        Expect.fail (FailureMessage.extractMismatch actual expected differences)

                    else
                        Expect.pass


{-| Expectation of something that the rule will report or do.

Check out the functions below to create these, and then pass them to [`Review.Test.expect`](#expect).

-}
type ReviewExpectation
    = FileErrorExpectation String (List ExpectedError)
    | GlobalErrorExpectation (List GlobalError)
    | DataExtractExpectation String


type CompiledDataExtract
    = NoDataExtractExpected
    | DataExtractExpected String
    | MultipleDataExtractExpected


type alias CompiledExpectations =
    { globals : List GlobalError
    , modules : List ( String, List ExpectedError )
    , dataExtract : CompiledDataExtract
    }


{-| Expect multiple outputs for tests.

Functions such as [`expectErrors`](#expectErrors) and [`expectGlobalErrors`](#expectGlobalErrors) work well, but
in some situations a rule will report multiple things: module errors, global errors, errors for `elm.json` or the
README, or even extract data.

When you have multiple expectations to make for a module, use this function.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should ..." <|
                \() ->
                    [ """module A.B exposing (..)
    import B
    a = 1
    b = 2
    c = 3
    """
                    , """module B exposing (..)
    x = 1
    y = 2
    z = 3
    """
                    ]
                        |> Review.Test.runOnModules rule
                        |> Review.Test.expect
                            [ Review.Test.globalErrors [ { message = "message", details = [ "details" ] } ]
                            , Review.Test.moduleErrors "A.B" [ { message = "message", details = [ "details" ] } ]
                            , Review.Test.dataExtract """
    {
        "foo": "bar",
        "other": [ 1, 2, 3 ]
    }"""
                            ]
            ]

-}
expect : List ReviewExpectation -> ReviewResult -> Expectation
expect expectations reviewResult =
    case reviewResult of
        ConfigurationError configurationError ->
            Expect.fail (FailureMessage.unexpectedConfigurationError configurationError)

        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun { foundGlobalErrors, runResults, extract, allErrors, project } reRun ->
            let
                expected : CompiledExpectations
                expected =
                    compileExpectations expectations
            in
            Expect.all
                [ \() ->
                    if List.isEmpty expected.globals then
                        checkNoGlobalErrors foundGlobalErrors

                    else
                        checkAllGlobalErrorsMatch project (List.length expected.globals) { expected = expected.globals, actual = foundGlobalErrors }
                , \() -> checkErrorsForModules project expected.modules runResults
                , \() ->
                    case expected.dataExtract of
                        NoDataExtractExpected ->
                            checkNoDataExtract extract

                        DataExtractExpected string ->
                            checkDataExtractContent string extract

                        MultipleDataExtractExpected ->
                            Expect.fail FailureMessage.specifiedMultipleExtracts
                , \() -> checkResultsAreTheSameWhenIgnoringFiles allErrors reRun
                ]
                ()


compileExpectations : List ReviewExpectation -> CompiledExpectations
compileExpectations expectations =
    List.foldl
        (\expectation acc ->
            case expectation of
                GlobalErrorExpectation globals ->
                    { acc | globals = globals ++ acc.globals }

                FileErrorExpectation moduleName errors ->
                    { acc | modules = ( moduleName, errors ) :: acc.modules }

                DataExtractExpectation string ->
                    { acc
                        | dataExtract =
                            case acc.dataExtract of
                                NoDataExtractExpected ->
                                    DataExtractExpected string

                                DataExtractExpected _ ->
                                    MultipleDataExtractExpected

                                MultipleDataExtractExpected ->
                                    MultipleDataExtractExpected
                    }
        )
        { globals = [], modules = [], dataExtract = NoDataExtractExpected }
        expectations


{-| Assert that the rule reported some [global errors](./Review-Rule#globalError), by specifying which ones. To be used along with [`Review.Test.expect`](#expect).

If you expect only global errors, then you may want to use [`expectGlobalErrors`](#expectGlobalErrors) which is simpler.
If you need to specify fixes for the global error, you will want to use [`globalErrorsWithFixes`](#globalErrorsWithFixes) instead.

Assert which errors are reported using records with the expected message and details. The test will fail if
a different number of errors than expected are reported, or if the message or details is incorrect.

    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report a global error when the specified module could not be found" <|
            \() ->
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.run (rule "ModuleB")
                    |> Review.Test.expect
                        [ Review.Test.globalErrors
                            [ { message = "Could not find module ModuleB"
                              , details =
                                    [ "You mentioned the module ModuleB in the configuration of this rule, but it could not be found."
                                    , "This likely means you misconfigured the rule or the configuration has become out of date with recent changes in your project."
                                    ]
                              }
                            ]
                        ]

-}
globalErrors : List { message : String, details : List String } -> ReviewExpectation
globalErrors expected =
    List.map (\{ message, details } -> { message = message, details = details, fixes = Dict.empty }) expected
        |> GlobalErrorExpectation


{-| Assert that the rule reported some [global errors](./Review-Rule#globalError), by specifying which ones. To be used along with [`Review.Test.expect`](#expect).

This is the same as [`globalErrors`](#globalErrors) with the difference that you can specify the results of the automatic fixes provided by the global error.

If you expect only global errors, then you may want to use [`expectGlobalErrors`](#expectGlobalErrors) which is simpler.

Assert which errors are reported using records with the expected message and details. The test will fail if
a different number of errors than expected are reported, or if the message or details is incorrect.

    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report a global error when the specified module could not be found" <|
            \() ->
                """
    module ModuleA exposing (a)
    import ModuleB
    a = 1"""
                    |> Review.Test.run (rule "ModuleB")
                    |> Review.Test.expect
                        [ Review.Test.globalErrorsWithFixes
                            [ { message = "Could not find module ModuleB"
                              , details =
                                    [ "You mentioned the module ModuleB in the configuration of this rule, but it could not be found."
                                    , "This likely means you misconfigured the rule or the configuration has become out of date with recent changes in your project."
                                    ]
                              , fixes =
                                    [ ( "src/ModuleA.elm"
                                      , Review.Test.edited """
    module ModuleA exposing (a)
    a = 1"""
                                      }
                                    ]
                              }
                            ]
                        ]

-}
globalErrorsWithFixes : List { message : String, details : List String, fixes : List ( String, ExpectedFix ) } -> ReviewExpectation
globalErrorsWithFixes expected =
    List.map (\{ message, details, fixes } -> { message = message, details = details, fixes = Dict.fromList fixes }) expected
        |> GlobalErrorExpectation


{-| Assert that the rule reported some errors for modules, by specifying which ones. To be used along with [`Review.Test.expect`](#expect).

If you expect only module errors, then you may want to use [`expectErrorsForModules`](#expectErrorsForModules) which is simpler.

    test "report an error when a module is unused" <|
        \() ->
            [ """
    module ModuleA exposing (a)
    a = 1""", """
    module ModuleB exposing (a)
    a = Debug.log "log" 1""" ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "ModuleB"
                        [ Review.Test.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Details about the error" ]
                            , under = "Debug.log"
                            }
                        ]
                    ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
moduleErrors : String -> List ExpectedError -> ReviewExpectation
moduleErrors moduleName expected =
    FileErrorExpectation moduleName expected


{-| Assert that the rule reported some errors for the `elm.json` file, by specifying which ones. To be used along with [`Review.Test.expect`](#expect).

If you expect only errors for `elm.json`, then you may want to use [`expectErrorsForElmJson`](#expectErrorsForElmJson) which is simpler.

    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "report an error when a module is unused" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addElmJson elmJsonToConstructManually
                in
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expect
                        [ Review.Test.elmJson
                            [ Review.Test.error
                                { message = "Unused dependency `author/package`"
                                , details = [ "Dependency should be removed" ]
                                , under = "author/package"
                                }
                            ]
                        ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
elmJsonErrors : List ExpectedError -> ReviewExpectation
elmJsonErrors expected =
    FileErrorExpectation "elm.json" expected


{-| Assert that the rule reported some errors for the `README.md` file. To be used along with [`Review.Test.expect`](#expect).

If you expect only errors for `README.md`, then you may want to use [`expectErrorsForReadme`](#expectErrorsForReadme) which is simpler.

    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should extract even if there are errors" <|
                \() ->
                    let
                        project : Project
                        project =
                            Project.new
                                |> Project.addReadme { path = "README.md", context = "# Project\n..." }
                    in
                    """module ModuleA exposing (a)
    a = 1"""
                        |> Review.Test.runWithProjectData project rule
                        |> Review.Test.expect
                            [ Review.Test.readme
                                [ Review.Test.error
                                    { message = "Invalid link"
                                    , details = [ "README contains an invalid link" ]
                                    , under = "htt://example.com"
                                    }
                                ]
                            ]
            ]

-}
readmeErrors : List ExpectedError -> ReviewExpectation
readmeErrors expected =
    FileErrorExpectation "README.md" expected


{-| Assert that the rule reported some errors for a specific extra file. To be used along with [`Review.Test.expect`](#expect).

If you expect only errors for one file, then you may want to use [`expectErrorsForExtraFile`](#expectErrorsForExtraFile) which is simpler.

    import Review.Project as Project
    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should extract even if there are errors" <|
                \() ->
                    let
                        project : Project
                        project =
                            Project.new
                                |> Project.addExtraFile { path = "my-file.ext", context = "some string" }
                    in
                    """module ModuleA exposing (a)
    a = 1"""
                        |> Review.Test.runWithProjectData project rule
                        |> Review.Test.expect
                            [ Review.Test.extraFileErrors
                                [ Review.Test.error
                                    { message = "Some message"
                                    , details = [ "Some details" ]
                                    , under = "some string"
                                    }
                                ]
                            ]
            ]

-}
extraFileErrors : String -> List ExpectedError -> ReviewExpectation
extraFileErrors filePath expected =
    FileErrorExpectation filePath expected


{-| Expect the rule to produce a specific data extract. To be used along with [`Review.Test.expect`](#expect).

If you expect the rule not to report any errors, then you may want to use [`expectDataExtract`](#expectDataExtract) which is simpler.

Note: You do not need to match the exact formatting of the JSON object, though the order of fields does need to match.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Rule.You.Want.To.Test"
            [ test "should extract even if there are errors" <|
                \() ->
                    [ """module A.B exposing (..)
    import B
    a = 1
    b = 2
    c = 3
    """
                    , """module B exposing (..)
    x = 1
    y = 2
    z = 3
    """
                    ]
                        |> Review.Test.runOnModules rule
                        |> Review.Test.expect
                            [ Review.Test.dataExtract """
    {
        "foo": "bar",
        "other": [ 1, 2, 3 ]
    }"""
                            ]
            ]

-}
dataExtract : String -> ReviewExpectation
dataExtract expectedDataExtract =
    DataExtractExpectation expectedDataExtract


{-| Indicates to the test that the knowledge of ignored files (through [`Review.Rule.withIsFileIgnored`](Review-Rule#withIsFileIgnored))
can impact results, and that that is done on purpose.

By default, `elm-review` assumes that the knowledge of which files are ignored will only be used to improve performance,
and not to impact the results of the rule.

Testing that your rule behaves as expected in all your scenarios and with or without some files being ignored can be
very hard. As such, the testing framework will automatically — if you've used `withIsFileIgnored` — run the rule again
but with some of the files being ignored (it will in practice test out all the combinations) and ensure that the results
stat the same with or without ignored files.

If your rule uses this information to change the results (report less or more errors, give different details in the error
message, ...), then you can use this function to tell the test not to attempt re-running and expecting the same results.
In this case, you should write tests where some of the files are ignored yourself.

    test "report an error when..." <|
        \() ->
            [ """
    module ModuleA exposing (a)
    a = 1""", """
    module ModuleB exposing (a)
    a = Debug.log "log" 1""" ]
                |> Review.Test.runOnModules rule
                |> Review.Test.ignoredFilesImpactResults
                |> Review.Test.expect whatYouExpect

-}
ignoredFilesImpactResults : ReviewResult -> ReviewResult
ignoredFilesImpactResults reviewResult =
    case reviewResult of
        SuccessfulRun data _ ->
            SuccessfulRun data DontAttemptReRun

        _ ->
            reviewResult


containsDifferences : List (Diff.Change a) -> Bool
containsDifferences changes =
    case changes of
        [] ->
            False

        (Diff.NoChange _) :: restOfChanges ->
            containsDifferences restOfChanges

        _ ->
            True
