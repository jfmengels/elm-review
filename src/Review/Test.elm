module Review.Test exposing
    ( ReviewResult, run, runWithProjectData, runOnModules, runOnModulesWithProjectData
    , ExpectedError, expectNoErrors, expectErrors, error, atExactly, whenFixed, expectErrorsForModules, expectErrorsForElmJson, expectErrorsForReadme
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
    want to write tests for that first.
  - (Then start over from step 1 until you've handled all the cases you wished
    to handle)


## Have a good title

A good test title explains

  - what is tested - Probably the rule, but making it explicit
    in a [`describe`](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test#describe)
    might improve your test report. Or maybe you are testing a sub-part of the rule,
    and you can name it explictly.
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

@docs ExpectedError, expectNoErrors, expectErrors, error, atExactly, whenFixed, expectErrorsForModules, expectErrorsForElmJson, expectErrorsForReadme

-}

import Array exposing (Array)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Review.Fix as Fix
import Review.Project as Project exposing (Project, ProjectModule)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test.ErrorMessage as ErrorMessage
import Set exposing (Set)
import Vendor.ListExtra as ListExtra



-- REVIEW RESULT


{-| The result of running a rule on a `String` containing source code.
-}
type ReviewResult
    = FailedRun String
    | SuccessfulRun (List SuccessfulRunResult)


type alias SuccessfulRunResult =
    { moduleName : String
    , inspector : CodeInspector
    , errors : List Error
    }


type alias CodeInspector =
    { source : String
    , getCodeAtLocation : Range -> Maybe String
    , checkIfLocationIsAmbiguous : Error -> String -> Expectation
    }


{-| An expectation for an error. Use [`error`](#error) to create one.
-}
type ExpectedError
    = ExpectedError
        { message : String
        , details : List String
        , under : Under
        , fixedSource : Maybe String
        }


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

    all : Test
    all =
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
    runWithProjectData Project.new rule source


{-| Run a `Rule` on a `String` containing source code, with data about the
project loaded, such as the contents of `elm.json` file.

    import My.Rule exposing (rule)
    import Review.Project as Project exposing (Project)
    import Review.Test
    import Test exposing (Test, test)

    all : Test
    all =
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

    all : Test
    all =
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

Note: This is a simpler version of [`runOnModulesWithProjectData`](#runOnModulesWithProjectData).
If your rule is interested in project related details, then you should use
[`runOnModulesWithProjectData`](#runOnModulesWithProjectData) instead.

-}
runOnModules : Rule -> List String -> ReviewResult
runOnModules rule sources =
    runOnModulesWithProjectData Project.new rule sources


{-| Run a `Rule` on several modules. You can then use
[`expectNoErrors`](#expectNoErrors) or [`expectErrorsForModules`](#expectErrorsForModules) to assert
the errors reported by the rule.

This is basically the same as [`run`](#run), but you can pass several modules.
This is especially useful to test rules created with
[`Review.Rule.newProjectRuleSchema`](./Review-Rule#newProjectRuleSchema), that look at
several modules, and where the context of the project is important.

    import My.Rule exposing (rule)
    import Review.Test
    import Test exposing (Test, test)

    all : Test
    all =
        test "test without errors" <|
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

Note: This is a more complex version of [`runOnModules`](#runOnModules). If your rule is not
interested in project related details, then you should use [`runOnModules`](#runOnModules) instead.

-}
runOnModulesWithProjectData : Project -> Rule -> List String -> ReviewResult
runOnModulesWithProjectData project rule sources =
    if List.isEmpty sources then
        FailedRun ErrorMessage.missingSources

    else
        let
            projectWithModules : Project
            projectWithModules =
                sources
                    |> List.indexedMap
                        (\index source ->
                            { path = "TestContent_" ++ String.fromInt index ++ ".elm"
                            , source = source
                            }
                        )
                    |> List.foldl Project.addModule project
        in
        case Project.modulesThatFailedToParse projectWithModules of
            { source } :: _ ->
                let
                    fileAndIndex : { source : String, index : Int }
                    fileAndIndex =
                        { source = source
                        , index = indexOf source sources |> Maybe.withDefault -1
                        }
                in
                FailedRun <| ErrorMessage.parsingFailure (List.length sources == 1) fileAndIndex

            [] ->
                let
                    modules : List ProjectModule
                    modules =
                        Project.modules projectWithModules
                in
                case findDuplicateModuleNames Set.empty modules of
                    Just moduleName ->
                        FailedRun <| ErrorMessage.duplicateModuleName moduleName

                    Nothing ->
                        let
                            errors : List Error
                            errors =
                                projectWithModules
                                    |> Rule.review [ rule ]
                                    |> Tuple.first
                        in
                        case ListExtra.find (\err_ -> Rule.errorFilePath err_ == "GLOBAL ERROR") errors of
                            Just globalError ->
                                FailedRun <| ErrorMessage.globalErrorInTest globalError

                            Nothing ->
                                List.concat
                                    [ List.map (moduleToRunResult errors) modules
                                    , elmJsonRunResult errors projectWithModules
                                    , readmeRunResult errors projectWithModules
                                    ]
                                    |> SuccessfulRun


moduleToRunResult : List Error -> ProjectModule -> SuccessfulRunResult
moduleToRunResult errors projectModule =
    { moduleName =
        projectModule.ast.moduleDefinition
            |> Node.value
            |> Module.moduleName
            |> String.join "."
    , inspector = codeInspectorForSource projectModule.source
    , errors =
        errors
            |> List.filter (\error_ -> Rule.errorFilePath error_ == projectModule.path)
            |> List.sortWith compareErrorPositions
    }


elmJsonRunResult : List Error -> Project -> List SuccessfulRunResult
elmJsonRunResult errors project =
    case Project.elmJson project of
        Just elmJsonData ->
            case List.filter (\error_ -> Rule.errorFilePath error_ == elmJsonData.path) errors of
                [] ->
                    []

                errorsForElmJson ->
                    [ { moduleName = elmJsonData.path
                      , inspector = codeInspectorForSource elmJsonData.raw
                      , errors = errorsForElmJson
                      }
                    ]

        Nothing ->
            []


readmeRunResult : List Error -> Project -> List SuccessfulRunResult
readmeRunResult errors project =
    case Project.readme project of
        Just readme ->
            case List.filter (\error_ -> Rule.errorFilePath error_ == readme.path) errors of
                [] ->
                    []

                errorsForElmJson ->
                    [ { moduleName = readme.path
                      , inspector = codeInspectorForSource readme.content
                      , errors = errorsForElmJson
                      }
                    ]

        Nothing ->
            []


indexOf : a -> List a -> Maybe Int
indexOf elementToFind aList =
    case aList of
        [] ->
            Nothing

        a :: rest ->
            if a == elementToFind then
                Just 0

            else
                indexOf elementToFind rest
                    |> Maybe.map ((+) 1)


codeInspectorForSource : String -> CodeInspector
codeInspectorForSource source =
    { source = source
    , getCodeAtLocation = getCodeAtLocationInSourceCode source
    , checkIfLocationIsAmbiguous = checkIfLocationIsAmbiguousInSourceCode source
    }


findDuplicateModuleNames : Set (List String) -> List ProjectModule -> Maybe (List String)
findDuplicateModuleNames previousModuleNames modules =
    case modules of
        [] ->
            Nothing

        { ast } :: restOfModules ->
            let
                moduleName : List String
                moduleName =
                    ast.moduleDefinition
                        |> Node.value
                        |> Module.moduleName
            in
            if Set.member moduleName previousModuleNames then
                Just moduleName

            else
                findDuplicateModuleNames (Set.insert moduleName previousModuleNames) restOfModules


compareErrorPositions : Error -> Error -> Order
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
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun runResults ->
            runResults
                |> List.map
                    (\{ errors, moduleName } () ->
                        List.isEmpty errors
                            |> Expect.true (ErrorMessage.didNotExpectErrors moduleName errors)
                    )
                |> (\expectations -> Expect.all expectations ())


{-| Assert that the rule reported some errors, by specifying which ones.

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

The errors should be in the order of where they appear in the source code. An error
at the start of the source code should appear earlier in the list than
an error at the end of the source code.

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
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun (runResult :: []) ->
            checkAllErrorsMatch runResult expectedErrors

        SuccessfulRun _ ->
            Expect.fail ErrorMessage.needToUsedExpectErrorsForModules


{-| Assert that the rule reported some errors, by specifying which ones and the
module for which they were reported.

This is the same as [`expectErrors`](#expectErrors), but for when you used
[`runOnModules`](#runOnModules) or [`runOnModulesWithProjectData`](#runOnModulesWithProjectData).
to create the test. When using those, the errors you expect need to be associated
with a module. If we don't specify this, your tests might pass because you
expected the right errors, but they may be reported for the wrong module!

The expected errors are tupled: the first element is the module name
(for example: `List` or `My.Module.Name`) and the second element is the list of
errors you expect to be reported.

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

The errors should be in the order of where they appear in the source code. An error
at the start of the source code should appear earlier in the list than
an error at the end of the source code.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    all : Test
    all =
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
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun runResults ->
            let
                maybeUnknownModule : Maybe String
                maybeUnknownModule =
                    Set.diff
                        (expectedErrorsList |> List.map Tuple.first |> Set.fromList)
                        (runResults |> List.map .moduleName |> Set.fromList)
                        |> Set.toList
                        |> List.head
            in
            case maybeUnknownModule of
                Just unknownModule ->
                    ErrorMessage.unknownModulesInExpectedErrors unknownModule
                        |> Expect.fail

                Nothing ->
                    runResults
                        |> List.map
                            (\runResult ->
                                let
                                    expectedErrors : List ExpectedError
                                    expectedErrors =
                                        expectedErrorsList
                                            |> ListExtra.find (\( moduleName_, _ ) -> moduleName_ == runResult.moduleName)
                                            |> Maybe.map Tuple.second
                                            |> Maybe.withDefault []
                                in
                                \() -> checkAllErrorsMatch runResult expectedErrors
                            )
                        |> (\expectations -> Expect.all expectations ())


{-| Assert that the rule reported some errors for the `elm.json` file, by specifying which ones.

    test "report an error when a module is unused" <|
        \() ->
            """
    module ModuleA exposing (a)
    a = 1"""
                |> Review.Test.runWithProjectData rule
                |> Review.Test.expectErrorsForElmJson
                    [ Review.Test.error
                        { message = "Unused dependency `author/package`"
                        , details = [ "Dependency should be removed" ]
                        , under = "author/package"
                        }
                    ]

Alternatively, or if you need to specify errors for other files too, you can use [`expectErrorsForModules`](#expectErrorsForModules), specifying `elm.json` as the module name.

    sourceCode
        |> Review.Test.runOnModules rule
        |> Review.Test.expectErrorsForModules
            [ ( "ModuleB", [ Review.Test.error someErrorModuleB ] )
            , ( "elm.json", [ Review.Test.error someErrorForElmJson ] )
            ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
expectErrorsForElmJson : List ExpectedError -> ReviewResult -> Expectation
expectErrorsForElmJson expectedErrors reviewResult =
    expectErrorsForModules [ ( "elm.json", expectedErrors ) ] reviewResult


{-| Assert that the rule reported some errors for the `README.md` file, by specifying which ones.

    test "report an error when a module is unused" <|
        \() ->
            """
    module ModuleA exposing (a)
    a = 1"""
                |> Review.Test.runWithProjectData rule
                |> Review.Test.expectErrorsForElmJson
                    [ Review.Test.error
                        { message = "Invalid link"
                        , details = [ "README contains an invalid link" ]
                        , under = "htt://example.com"
                        }
                    ]

Alternatively, or if you need to specify errors for other files too, you can use [`expectErrorsForModules`](#expectErrorsForModules), specifying `README.md` as the module name.

    sourceCode
        |> Review.Test.runOnModules rule
        |> Review.Test.expectErrorsForModules
            [ ( "ModuleB", [ Review.Test.error someErrorModuleB ] )
            , ( "README.md", [ Review.Test.error someErrorForReadme ] )
            ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
expectErrorsForReadme : List ExpectedError -> ReviewResult -> Expectation
expectErrorsForReadme expectedErrors reviewResult =
    expectErrorsForModules [ ( "README.md", expectedErrors ) ] reviewResult


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
        , fixedSource = Nothing
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
atExactly range ((ExpectedError expectedError_) as expectedError) =
    ExpectedError { expectedError_ | under = UnderExactly (getUnder expectedError) range }


{-| Create an expectation that the error provides fixes, meaning that it used
the [`withFixes`](#withFixes) function) and an expectation of what the source
code should be after the error's fixes have been applied.

In the absence of `whenFixed`, the test will fail if the error provides fixes.
In other words: If the error provides fixes, you need to use `withFixes`, and if
it doesn't, you should not use `withFixes`.

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
whenFixed fixedSource ((ExpectedError expectedError_) as expectedError) =
    ExpectedError { expectedError_ | fixedSource = Just fixedSource }


getUnder : ExpectedError -> String
getUnder (ExpectedError expectedError) =
    case expectedError.under of
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
                |> Maybe.map (String.slice (start.column - 1) (end.column - 1))

        else
            let
                firstLine : Maybe String
                firstLine =
                    Array.get (start.row - 1) lines
                        |> Maybe.map (String.dropLeft (start.column - 1))

                lastLine : Maybe String
                lastLine =
                    Array.get (end.row - 1) lines
                        |> Maybe.map (String.dropRight end.column)
            in
            [ [ firstLine ]
            , Array.slice start.row (end.row - 1) lines
                |> Array.toList
                |> List.map Just
            , [ lastLine ]
            ]
                |> List.concat
                |> List.filterMap identity
                |> String.join "\n"
                |> Just


checkIfLocationIsAmbiguousInSourceCode : SourceCode -> Error -> String -> Expectation
checkIfLocationIsAmbiguousInSourceCode sourceCode error_ under =
    let
        occurrencesInSourceCode : List Int
        occurrencesInSourceCode =
            String.indexes under sourceCode
    in
    (List.length occurrencesInSourceCode == 1)
        |> Expect.true (ErrorMessage.locationIsAmbiguousInSourceCode sourceCode error_ under occurrencesInSourceCode)



-- RUNNING THE CHECKS


checkAllErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Expectation
checkAllErrorsMatch runResult expectedErrors =
    checkErrorsMatch runResult expectedErrors runResult.errors
        |> List.reverse
        |> (\expectations -> Expect.all expectations ())


checkErrorsMatch : SuccessfulRunResult -> List ExpectedError -> List Error -> List (() -> Expectation)
checkErrorsMatch runResult expectedErrors errors =
    case ( expectedErrors, errors ) of
        ( [], [] ) ->
            [ always Expect.pass ]

        ( expected :: restOfExpectedErrors, error_ :: restOfErrors ) ->
            checkErrorMatch runResult.inspector expected error_
                :: checkErrorsMatch runResult restOfExpectedErrors restOfErrors

        ( expected :: restOfExpectedErrors, [] ) ->
            [ always <| Expect.fail <| ErrorMessage.expectedMoreErrors runResult.moduleName <| List.map extractExpectedErrorData (expected :: restOfExpectedErrors) ]

        ( [], error_ :: restOfErrors ) ->
            [ always <| Expect.fail <| ErrorMessage.tooManyErrors runResult.moduleName (error_ :: restOfErrors) ]


checkErrorMatch : CodeInspector -> ExpectedError -> Error -> (() -> Expectation)
checkErrorMatch codeInspector ((ExpectedError expectedError_) as expectedError) error_ =
    Expect.all
        [ \() ->
            (expectedError_.message == Rule.errorMessage error_)
                |> Expect.true
                    (ErrorMessage.messageMismatch
                        (extractExpectedErrorData expectedError)
                        error_
                    )
        , checkMessageAppearsUnder codeInspector error_ expectedError
        , checkDetailsAreCorrect error_ expectedError
        , \() -> checkFixesAreCorrect codeInspector error_ expectedError
        ]


checkMessageAppearsUnder : CodeInspector -> Error -> ExpectedError -> (() -> Expectation)
checkMessageAppearsUnder codeInspector error_ (ExpectedError expectedError) =
    case codeInspector.getCodeAtLocation (Rule.errorRange error_) of
        Just codeAtLocation ->
            case expectedError.under of
                Under under ->
                    Expect.all
                        [ \() ->
                            case under of
                                "" ->
                                    ErrorMessage.underMayNotBeEmpty
                                        { message = expectedError.message
                                        , codeAtLocation = codeAtLocation
                                        }
                                        |> Expect.fail

                                _ ->
                                    Expect.pass
                        , \() ->
                            (codeAtLocation == under)
                                |> Expect.true (ErrorMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                        , \() -> codeInspector.checkIfLocationIsAmbiguous error_ under
                        ]

                UnderExactly under range ->
                    Expect.all
                        [ \() ->
                            (codeAtLocation == under)
                                |> Expect.true (ErrorMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                        , \() ->
                            (Rule.errorRange error_ == range)
                                |> Expect.true (ErrorMessage.wrongLocation error_ range under)
                        ]

        Nothing ->
            \() ->
                ErrorMessage.locationNotFound error_
                    |> Expect.fail


checkDetailsAreCorrect : Error -> ExpectedError -> (() -> Expectation)
checkDetailsAreCorrect error_ (ExpectedError expectedError) =
    Expect.all
        [ (not <| List.isEmpty <| Rule.errorDetails error_)
            |> Expect.true (ErrorMessage.emptyDetails error_)
            |> always
        , (Rule.errorDetails error_ == expectedError.details)
            |> Expect.true (ErrorMessage.unexpectedDetails expectedError.details error_)
            |> always
        ]


checkFixesAreCorrect : CodeInspector -> Error -> ExpectedError -> Expectation
checkFixesAreCorrect codeInspector error_ ((ExpectedError expectedError_) as expectedError) =
    case ( expectedError_.fixedSource, Rule.errorFixes error_ ) of
        ( Nothing, Nothing ) ->
            Expect.pass

        ( Just expectedFixedSource, Nothing ) ->
            ErrorMessage.missingFixes (extractExpectedErrorData expectedError)
                |> Expect.fail

        ( Nothing, Just fixes ) ->
            ErrorMessage.unexpectedFixes error_
                |> Expect.fail

        ( Just expectedFixedSource, Just fixes ) ->
            case Fix.fix fixes codeInspector.source of
                Fix.Successful fixedSource ->
                    (fixedSource == expectedFixedSource)
                        |> Expect.true (ErrorMessage.fixedCodeMismatch fixedSource expectedFixedSource error_)

                Fix.Errored Fix.Unchanged ->
                    Expect.fail <| ErrorMessage.unchangedSourceAfterFix error_

                Fix.Errored (Fix.SourceCodeIsNotValid sourceCode) ->
                    Expect.fail <| ErrorMessage.invalidSourceAfterFix error_ sourceCode

                Fix.Errored Fix.HasCollisionsInFixRanges ->
                    Expect.fail <| ErrorMessage.hasCollisionsInFixRanges error_


extractExpectedErrorData : ExpectedError -> ErrorMessage.ExpectedErrorData
extractExpectedErrorData ((ExpectedError expectedErrorContent) as expectedError) =
    { message = expectedErrorContent.message
    , details = expectedErrorContent.details
    , under = getUnder expectedError
    }
