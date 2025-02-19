module Review.Test.FailureMessage exposing
    ( Target(..)
    , parsingFailure, globalErrorInTest, messageMismatch, emptyDetails, unexpectedDetails, wrongLocation, didNotExpectErrors
    , underMismatch, expectedMoreErrors, tooManyErrors, locationNotFound, underMayNotBeEmpty, locationIsAmbiguousInSourceCode
    , needToUsedExpectErrorsForModules, missingSources, duplicateModuleName, unknownModulesInExpectedErrors
    , missingFixes, unexpectedFixes, unexpectedAdditionalFixes, fixedCodeMismatch, fixProblem
    , fileWasEditedInsteadOfRemoved, fileWasRemovedInsteadOfEdited
    , didNotExpectGlobalErrors, expectedMoreGlobalErrors, fixedCodeWhitespaceMismatch, messageMismatchForConfigurationError
    , missingConfigurationError, tooManyGlobalErrors
    , unexpectedConfigurationError, unexpectedConfigurationErrorDetails, unexpectedGlobalErrorDetails
    , unexpectedExtract, missingExtract, invalidJsonForExpectedDataExtract, extractMismatch, specifiedMultipleExtracts
    , resultsAreDifferentWhenFilesAreIgnored
    , fixForUnknownFile
    )

{-| Failure messages for the `Review.Test` module.


# ReviewError messages

@docs Target
@docs parsingFailure, globalErrorInTest, messageMismatch, emptyDetails, unexpectedDetails, wrongLocation, didNotExpectErrors
@docs underMismatch, expectedMoreErrors, tooManyErrors, locationNotFound, underMayNotBeEmpty, locationIsAmbiguousInSourceCode
@docs needToUsedExpectErrorsForModules, missingSources, duplicateModuleName, unknownModulesInExpectedErrors
@docs missingFixes, unexpectedFixes, unexpectedAdditionalFixes, fixedCodeMismatch, fixProblem
@docs fileWasEditedInsteadOfRemoved, fileWasRemovedInsteadOfEdited
@docs didNotExpectGlobalErrors, expectedMoreGlobalErrors, fixedCodeWhitespaceMismatch, messageMismatchForConfigurationError
@docs missingConfigurationError, tooManyGlobalErrors
@docs unexpectedConfigurationError, unexpectedConfigurationErrorDetails, unexpectedGlobalErrorDetails
@docs unexpectedExtract, missingExtract, invalidJsonForExpectedDataExtract, extractMismatch, specifiedMultipleExtracts
@docs resultsAreDifferentWhenFilesAreIgnored
@docs fixForUnknownFile

-}

import Ansi
import Elm.Syntax.Range exposing (Location, Range)
import Json.Decode as Decode
import Json.Encode as Encode
import Parser
import Review.Fix.Edit exposing (Edit)
import Review.Fix.FixProblem as FixProblem
import Review.ImportCycle as ImportCycle
import Review.Rule as Rule exposing (ReviewError)
import Vendor.Diff as Diff
import Vendor.ListExtra as ListExtra


type alias SourceCode =
    String



-- ERROR MESSAGES


didNotExpectErrors : String -> List ReviewError -> String
didNotExpectErrors moduleName errors =
    failureMessage "DID NOT EXPECT ERRORS"
        ("""I expected no errors for module `""" ++ moduleName ++ """` but found:

""" ++ listErrorMessagesAndPositions errors)


didNotExpectGlobalErrors : List String -> String
didNotExpectGlobalErrors errors =
    failureMessage "DID NOT EXPECT GLOBAL ERRORS"
        ("""I expected no global errors but found:

""" ++ quotedBulletList errors)


quotedBulletList : List String -> String
quotedBulletList strings =
    strings
        |> List.map (\string -> "  - " ++ wrapInQuotes string)
        |> String.join "\n"


parsingFailure : Bool -> { index : Int, source : String } -> String
parsingFailure isOnlyFile { index, source } =
    let
        hint : String
        hint =
            """Hint: Maybe you forgot to add the module definition at the top, like:

  `module A exposing (..)`"""

        details : String
        details =
            if isOnlyFile then
                "I could not parse the test source code, because it was not valid Elm code."

            else
                """I could not parse one of the test source codes, because it was not valid
Elm code.

The source code in question is the one at index """
                    ++ String.fromInt index
                    ++ """ starting with:

  `"""
                    ++ (String.concat <| List.take 1 <| String.split "\n" <| String.trim source)
                    ++ "`"
    in
    failureMessage "TEST SOURCE CODE PARSING ERROR" (details ++ "\n\n" ++ hint)


globalErrorInTest : ReviewError -> String
globalErrorInTest error =
    failureMessage "GLOBAL ERROR IN SOURCE CODE"
        ("""I found a global error in the project you provided for this test:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

  """ ++ formatDetails (Rule.errorDetails error) ++ """

`elm-review` would fail with this error if the project to be reviewed had
the same issue. Please fix this issue in your test.
""")


messageMismatch : { target : Target, expected : String, actual : String } -> String
messageMismatch { target, expected, actual } =
    failureMessage "UNEXPECTED ERROR MESSAGE"
        ("I was looking for the " ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes expected ++ """

but I found the following error message:

  """ ++ wrapInQuotes actual)


messageMismatchForConfigurationError : { a | message : String } -> { b | message : String } -> String
messageMismatchForConfigurationError expectedError error =
    failureMessage "UNEXPECTED CONFIGURATION ERROR MESSAGE"
        ("""I was looking for the configuration error with the following message:

  """ ++ wrapInQuotes expectedError.message ++ """

but I found the following error message:

  """ ++ wrapInQuotes error.message)


underMismatch : ReviewError -> { under : String, codeAtLocation : String } -> String
underMismatch error { under, codeAtLocation } =
    failureMessage "UNEXPECTED ERROR LOCATION"
        ("""I found an error with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

and I was expecting it to be under:

  """ ++ formatSourceCode under ++ """

but I found it under:

  """ ++ formatSourceCode codeAtLocation ++ """

Hint: Maybe you're passing the `Range` of a wrong node when
calling `Rule.error`.""")


unexpectedDetails : List String -> ReviewError -> String
unexpectedDetails expectedDetails error =
    failureMessage "UNEXPECTED ERROR DETAILS"
        ("""I found an error for a file with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

and I was expecting its details to be:

  """ ++ formatDetails expectedDetails ++ """

but I found these details:

  """ ++ formatDetails (Rule.errorDetails error))


unexpectedGlobalErrorDetails : List String -> { message : String, details : List String } -> String
unexpectedGlobalErrorDetails expectedDetails error =
    failureMessage "UNEXPECTED GLOBAL ERROR DETAILS"
        ("""I found a global error with the following message:

  """ ++ wrapInQuotes error.message ++ """

and I was expecting its details to be:

  """ ++ formatDetails expectedDetails ++ """

but I found these details:

  """ ++ formatDetails error.details)


unexpectedConfigurationErrorDetails : List String -> { message : String, details : List String } -> String
unexpectedConfigurationErrorDetails expectedDetails error =
    failureMessage "UNEXPECTED CONFIGURATION ERROR DETAILS"
        ("""I found a configuration error with the following message:

  """ ++ wrapInQuotes error.message ++ """

and I was expecting its details to be:

  """ ++ formatDetails expectedDetails ++ """

but I found these details:

  """ ++ formatDetails error.details)


emptyDetails : String -> String
emptyDetails errorMessage =
    failureMessage "EMPTY ERROR DETAILS"
        ("""I found an error with the following message:

  """ ++ wrapInQuotes errorMessage ++ """

but its details were empty. I require having details as I believe they will
help the user who encounters the problem.

The details could:
- explain what the problem is
- explain the reasoning behind the problem
- give suggestions on how to solve the problem or alternatives""")


formatDetails : List String -> String
formatDetails details =
    case details of
        [ detail ] ->
            wrapInQuotes detail

        details_ ->
            details_
                |> List.map (\str -> "  " ++ str)
                |> String.join "\n\n"
                |> (\str -> "```\n" ++ str ++ "\n  ```")


wrongLocation : ReviewError -> Range -> String -> String
wrongLocation error range under =
    failureMessage "UNEXPECTED ERROR LOCATION"
        ("""I was looking for the error with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

under the following code:

  """ ++ formatSourceCode under ++ """

and I found it, but the exact location you specified is not the one I found.

I was expecting the error at:

  """ ++ rangeAsString range ++ """

but I found it at:

  """ ++ rangeAsString (Rule.errorRange error))


expectedMoreErrors : String -> Int -> List String -> String
expectedMoreErrors moduleName expectedNumberOfErrors missingExpectedErrorMessages =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length missingExpectedErrorMessages
    in
    failureMessage "RULE REPORTED LESS ERRORS THAN EXPECTED"
        ("I expected to see " ++ String.fromInt expectedNumberOfErrors ++ " errors for module `" ++ moduleName ++ "` but only found " ++ String.fromInt (expectedNumberOfErrors - numberOfErrors) ++ """.
Here are the """ ++ String.fromInt numberOfErrors ++ """ I could not find:

""")
        ++ (missingExpectedErrorMessages
                |> List.map expectedErrorToString
                |> String.join "\n"
           )


expectedMoreGlobalErrors : Int -> List String -> String
expectedMoreGlobalErrors expectedNumberOfErrors missingExpectedErrorMessages =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length missingExpectedErrorMessages
    in
    failureMessage "RULE REPORTED LESS GLOBAL ERRORS THAN EXPECTED"
        ("I expected to see " ++ String.fromInt expectedNumberOfErrors ++ " global errors but only found " ++ String.fromInt (expectedNumberOfErrors - numberOfErrors) ++ """.
Here are the """ ++ String.fromInt numberOfErrors ++ """ I could not find:

""")
        ++ (missingExpectedErrorMessages
                |> List.map expectedErrorToString
                |> String.join "\n"
           )


tooManyErrors : String -> List ReviewError -> String
tooManyErrors moduleName extraErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length extraErrors
    in
    failureMessage "RULE REPORTED MORE ERRORS THAN EXPECTED"
        ("I found "
            ++ (String.fromInt numberOfErrors ++ " " ++ pluralizeErrors numberOfErrors ++ " too many for module `" ++ moduleName ++ "`:\n\n")
            ++ listErrorMessagesAndPositions extraErrors
        )


tooManyGlobalErrors : List String -> String
tooManyGlobalErrors extraErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length extraErrors
    in
    failureMessage "RULE REPORTED MORE GLOBAL ERRORS THAN EXPECTED"
        ("I found "
            ++ (String.fromInt numberOfErrors ++ " global " ++ pluralizeErrors numberOfErrors ++ " too many:\n\n")
            ++ quotedBulletList extraErrors
        )


locationNotFound : ReviewError -> String
locationNotFound error =
    failureMessage "COULD NOT FIND LOCATION FOR ERROR"
        ("""I was looking for the error with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

and I found it, but the code it points to does not lead to anything:

  """ ++ rangeAsString (Rule.errorRange error) ++ """

Please try to have the error under the smallest region that makes sense.
This will be the most helpful for the person who reads the error message.""")


underMayNotBeEmpty : { message : String, codeAtLocation : String } -> String
underMayNotBeEmpty { message, codeAtLocation } =
    failureMessage "COULD NOT FIND LOCATION FOR ERROR"
        ("""I was looking for the error with the following message:

  """ ++ wrapInQuotes message ++ """

and I found it, but the expected error has an empty string for `under`. I
need to point somewhere, so as to best help the people who encounter this
error.

If this helps, this is where I found the error:

  """ ++ formatSourceCode codeAtLocation)


locationIsAmbiguousInSourceCode : SourceCode -> ReviewError -> String -> List Int -> String
locationIsAmbiguousInSourceCode sourceCode error under occurrencesInSourceCode =
    failureMessage "AMBIGUOUS ERROR LOCATION"
        ("""Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

and expecting to see it under:

  """ ++ formatSourceCode under ++ """

I found """ ++ String.fromInt (List.length occurrencesInSourceCode) ++ """ locations where that code appeared. Please use
`Review.Test.atExactly` to make the part you were targeting unambiguous.

Tip: I found them at:
""" ++ listOccurrencesAsLocations sourceCode under occurrencesInSourceCode)


needToUsedExpectErrorsForModules : String
needToUsedExpectErrorsForModules =
    failureMessage "AMBIGUOUS MODULE FOR ERROR"
        """You gave me several modules, and you expect some errors. I need to know for
which module you expect these errors to be reported.

You should use `Review.Test.expectErrorsForModules` to do this:

  test "..." <|
    \\() ->
      [ \"\"\"
module A exposing (..)
-- someCode
\"\"\", \"\"\"
module B exposing (..)
-- someCode
\"\"\" ]
      |> Review.Test.runOnModules rule
      |> Review.Test.expectErrorsForModules
          [ ( "B", [ Review.Test.error someError ] )
          ]"""


missingSources : String
missingSources =
    failureMessage "MISSING SOURCES"
        """You used `runOnModules` or `runOnModulesWithProjectData` with an empty list
of sources files.

I need sources to reviewing, because reviewing an empty project does not
make much sense to me."""


duplicateModuleName : List String -> String
duplicateModuleName moduleName =
    failureMessage "DUPLICATE MODULE NAMES"
        ("""I found several modules named `""" ++ String.join "." moduleName ++ """` in the test source codes.

I expect all modules to be able to exist together in the same project,
but having several modules with the same name is not allowed by the Elm
compiler.

Please rename the modules so that they all have different names.""")


unknownModulesInExpectedErrors : String -> String
unknownModulesInExpectedErrors moduleName =
    failureMessage "UNKNOWN MODULES IN EXPECTED ERRORS"
        ("""I expected errors for a module named `""" ++ moduleName ++ """` in the list passed to
`expectErrorsForModules`, but I couldn't find a module in the test source
codes named that way.

I assume that there was a mistake during the writing of the test. Please
match the names of the modules in the test source codes to the ones in the
expected errors list.""")


missingFixes : { target : Target, message : String, expectedFixedModules : List String } -> String
missingFixes { target, message, expectedFixedModules } =
    let
        targets : String
        targets =
            case List.reverse expectedFixedModules of
                [] ->
                    ""

                single :: [] ->
                    if Module single == target then
                        "itself"

                    else
                        wrapInQuotes single

                last :: previous ->
                    (previous |> List.reverse |> List.map wrapInQuotes |> String.join ", ")
                        ++ " and "
                        ++ wrapInQuotes last
    in
    failureMessage "MISSING FIXES"
        ("I expected that the " ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes message ++ """

would provide some fixes for """ ++ targets ++ """, but I didn't find any.

Hint: Maybe you forgot to call a function like `Rule.errorWithFix` or maybe
the list of provided fixes was empty.""")


unexpectedFixes : String -> String
unexpectedFixes errorMessage =
    failureMessage "UNEXPECTED FIXES"
        ("""I expected that the error with the following message:

  """ ++ wrapInQuotes errorMessage ++ """

would not have any fixes, but it provided some.

Because the error provides fixes, I require providing the expected result
of the automatic fix. Otherwise, there is no way to know whether the fix
will result in a correct and in the intended result.

To fix this, you can call `Review.Test.whenFixed` on your error:

  Review.Test.error
      { message = "<message>"
      , details = "<details>"
      , under = "<under>"
      }
      |> Review.Test.whenFixed "<source code>\"""")


unexpectedAdditionalFixes : { target : Target, message : String, nameOfFixedFile : String, fixedSource : String } -> String
unexpectedAdditionalFixes { target, message, nameOfFixedFile, fixedSource } =
    let
        recommendation : String
        recommendation =
            case target of
                Module _ ->
                    """

If this fix was expected, update the test by using `Review.Test.whenFixed`
or `Review.Test.shouldFixFiles`. If it isn't, then change the rule's
implementation to not provide a fix for this situation."""

                Global ->
                    """

If this fix was expected, update the test by using
`expectGlobalErrorsWithFixes` or `globalErrorsWithFixes`. If it isn't, then
change the rule's implementation to not provide a fix for this situation."""
    in
    failureMessage "UNEXPECTED FIXES"
        ("I expected that the " ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes message ++ """

would provide fixes, but I found an unexpected fix for """ ++ wrapInQuotes nameOfFixedFile ++ """.
This is what it gets fixed to:

  """ ++ formatSourceCode fixedSource ++ recommendation)


fixedCodeMismatch : SourceCode -> SourceCode -> ReviewError -> String
fixedCodeMismatch resultingSourceCode expectedSourceCode error =
    failureMessage "FIXED CODE MISMATCH"
        ("""I found a different fixed source code than expected for the error with the
following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

I expected the following result after the fixes have been applied:

  """ ++ formatSourceCode expectedSourceCode ++ """

but I found:

  """ ++ formatSourceCode resultingSourceCode)


type Target
    = Module String
    | Global


describeTarget : Target -> String
describeTarget target =
    case target of
        Module moduleName ->
            "error for module " ++ wrapInQuotes moduleName

        Global ->
            "global error"


fileWasEditedInsteadOfRemoved : { target : Target, message : String, nameOfFixedFile : String, fixedSource : String } -> String
fileWasEditedInsteadOfRemoved { target, message, nameOfFixedFile, fixedSource } =
    failureMessage "INCORRECT FIX TYPE"
        ("I expected that the " ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes message ++ """

would provide fixes, but I found an unexpected fix for """ ++ wrapInQuotes nameOfFixedFile ++ """.
I expected the file to be removed, but instead it gets edited to:

  """ ++ formatSourceCode fixedSource)


fileWasRemovedInsteadOfEdited : { target : Target, message : String, nameOfFixedFile : String } -> String
fileWasRemovedInsteadOfEdited { target, message, nameOfFixedFile } =
    failureMessage "INCORRECT FIX TYPE"
        ("I expected that the " ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes message ++ """

would provide fixes, but I found an unexpected fix for """ ++ wrapInQuotes nameOfFixedFile ++ """.
I expected the file to be edited, but instead it gets removed.""")


fixedCodeWhitespaceMismatch : SourceCode -> SourceCode -> ReviewError -> String
fixedCodeWhitespaceMismatch resultingSourceCode expectedSourceCode error =
    let
        ( expected, resulting ) =
            highlightDifferencesInSourceCodes resultingSourceCode expectedSourceCode
    in
    failureMessage "FIXED CODE MISMATCH (WHITESPACE ISSUE)"
        ("""I found a different fixed source code than expected for the error with the
following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

The problem is related to """ ++ Ansi.yellow (Ansi.bold "WHITESPACE!") ++ """
I expected the following result after the fixes have been applied:

  """ ++ expected ++ """

but I found:

  """ ++ resulting)


highlightDifferencesInSourceCodes : SourceCode -> SourceCode -> ( String, String )
highlightDifferencesInSourceCodes a b =
    let
        ( resA, resB ) =
            highlightWhiteSpaceDifferences a b
    in
    ( formatSourceCodeWithFormatter replaceWhitespace (String.lines resA), formatSourceCodeWithFormatter replaceWhitespace (String.lines resB) )


highlightWhiteSpaceDifferences : String -> String -> ( String, String )
highlightWhiteSpaceDifferences aString bString =
    Diff.diff (String.toList aString) (String.toList bString)
        |> List.foldl
            (\change ( a, b ) ->
                case change of
                    Diff.NoChange str ->
                        ( a ++ String.fromChar str, b ++ String.fromChar str )

                    Diff.Added '\n' ->
                        ( a ++ Ansi.backgroundRed "↵" ++ "\n", b )

                    Diff.Added str ->
                        ( a ++ Ansi.backgroundRed (String.fromChar str), b )

                    Diff.Removed '\n' ->
                        ( a, b ++ Ansi.backgroundRed "↵" ++ "\n" )

                    Diff.Removed str ->
                        ( a, b ++ Ansi.backgroundRed (String.fromChar str) )
            )
            ( "", "" )


replaceWhitespace : List String -> List String
replaceWhitespace lines =
    lines
        |> List.map (String.replace " " (Ansi.cyan "·"))
        |> String.join (Ansi.cyan "\n")
        |> String.split "\n"


fixProblem : Target -> FixProblem.FixProblem -> ReviewError -> String
fixProblem target fixProblem_ error_ =
    case fixProblem_ of
        FixProblem.Unchanged problem ->
            unchangedSourceAfterEdit target error_ problem

        FixProblem.InvalidElm problem ->
            invalidElmFileAfterFix target error_ problem

        FixProblem.InvalidJson problem ->
            invalidJsonAfterFix target error_ problem

        FixProblem.EditWithNegativeRange { filePath, edit } ->
            editWithNegativeRange target filePath error_ edit

        FixProblem.HasCollisionsInEditRanges { filePath, edits } ->
            hasCollisionsInEditRanges target filePath error_ edits

        FixProblem.CreatesImportCycle importCycleModuleNames ->
            importCycleAfterFix target importCycleModuleNames error_

        FixProblem.RemovesUnknownFile filePath ->
            removesUnknownFile target filePath error_

        FixProblem.Other problem ->
            otherProblem target problem error_


unchangedSourceAfterEdit : Target -> ReviewError -> { filePath : String, edits : List Edit } -> String
unchangedSourceAfterEdit target error problem =
    failureMessage "UNCHANGED SOURCE AFTER EDIT"
        ("""I got something unexpected when applying the fixes provided by the """ ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

After applying the fixes, """ ++ problem.filePath ++ """ was left unchanged.

Here are the individual edits for the file:

  [ """ ++ String.join "\n  , " (List.map (editToCode "      ") problem.edits) ++ """
  ]

I expected the fix to make some changes to the source code, but it resulted
in the same source code as before the fixes.

This is problematic because I will tell the user that this rule provides an
automatic fix, but I will have to disappoint them when I later find out it
doesn't do anything.

Hint: Maybe you inserted an empty string into the source code.""")


removesUnknownFile : Target -> String -> ReviewError -> String
removesUnknownFile target filePath error =
    failureMessage "FIX REMOVES UNKNOWN FILE"
        ("""I got something unexpected when applying the fixes provided by the """ ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

This fix attempts to remove the file """ ++ filePath ++ """
but I don't know this file.

This should not be possible in theory, so please open an issue so this
can be fixed.""")


otherProblem : Target -> String -> ReviewError -> String
otherProblem target problem error =
    failureMessage "PROBLEM OCCURRED WHEN APPLYING FIX"
        ("""I got something unexpected when applying the fixes provided by the """ ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

""" ++ problem)


invalidElmFileAfterFix : Target -> ReviewError -> { filePath : String, source : SourceCode, edits : List Edit, parsingErrors : List Parser.DeadEnd } -> String
invalidElmFileAfterFix target error { filePath, source, edits, parsingErrors } =
    failureMessage "INVALID SOURCE AFTER FIX"
        ("""I got something unexpected when applying the fixes provided by the """ ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

I was unable to parse the source code for """ ++ filePath ++ """ after applying the fixes:

""" ++ deadEndsToString parsingErrors ++ """

Here is the result of the automatic fixing:

  """ ++ formatSourceCode source ++ """

Here are the individual edits for the file:

  [ """ ++ String.join "\n  , " (List.map (editToCode "      ") edits) ++ """
  ]

This is problematic because fixes are meant to help the user, and applying
this fix will give them more work to do. After the fix has been applied,
the problem should be solved and the user should not have to think about it
anymore. If a fix can not be applied fully, it should not be applied at
all.""")


invalidJsonAfterFix : Target -> ReviewError -> { filePath : String, source : SourceCode, edits : List Edit, decodingError : Decode.Error } -> String
invalidJsonAfterFix target error { filePath, source, edits, decodingError } =
    failureMessage "INVALID JSON AFTER FIX"
        ("""I got something unexpected when applying the fixes provided by the """ ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

I was unable to parse/decode """ ++ filePath ++ """ after applying the fixes.

  """ ++ Decode.errorToString decodingError ++ """

Here is the result of the automatic fixing:

  """ ++ formatSourceCode source ++ """

Here are the individual edits for the file:

  [ """ ++ String.join "\n  , " (List.map (editToCode "      ") edits) ++ """
  ]

This is problematic because fixes are meant to help the user, and applying
this fix will give them more work to do. After the fix has been applied,
the problem should be solved and the user should not have to think about it
anymore. If a fix can not be applied fully, it should not be applied at
all.""")


importCycleAfterFix : Target -> List String -> ReviewError -> String
importCycleAfterFix target cycle error =
    failureMessage "IMPORT CYCLE AFTER FIX"
        ("I expected that the " ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

would provide some fixes, but they introduced an import cycle in the
project:

""" ++ ImportCycle.printCycle cycle)


editWithNegativeRange : Target -> String -> ReviewError -> Edit -> String
editWithNegativeRange target fileWithFixIssues error edit =
    failureMessage "FOUND NEGATIVE RANGE IN EDIT"
        ("""I got something unexpected when applying the fixes provided by the """ ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

When evaluating the edits for """ ++ fileWithFixIssues ++ """
I found an edit where the start is positioned after the end:

  """ ++ editToCode "    " edit ++ """

I don't know what to do with this edit.""")


hasCollisionsInEditRanges : Target -> String -> ReviewError -> List Edit -> String
hasCollisionsInEditRanges target fileWithFixIssues error edits =
    failureMessage "FOUND COLLISIONS IN EDIT RANGES"
        ("""I got something unexpected when applying the fixes provided by the """ ++ describeTarget target ++ """ with the following message:

  """ ++ wrapInQuotes (Rule.errorMessage error) ++ """

When evaluating the edits for """ ++ fileWithFixIssues ++ """
I found that some edits were targeting (partially or completely) the same
section of code:

  """ ++ String.join "\n\n  " (List.map (editToCode "    ") edits) ++ """

The problem is that I can't determine which fix to apply first, and the
result will be different and potentially invalid based on the order in
which I apply these fixes.

For this reason, I require that the ranges (for replacing and removing) and
the positions (for inserting) of every fix to be mutually exclusive.

Hint: Maybe you duplicated a fix, or you targeted the wrong node for one
of your fixes.""")


editToCode : String -> Edit -> String
editToCode indentation edit =
    case edit of
        Review.Fix.Edit.Removal range ->
            "Review.Fix.removeRange\n" ++ indentation ++ rangeAsString range

        Review.Fix.Edit.Replacement range replacement ->
            "Review.Fix.replaceRangeBy\n" ++ indentation ++ rangeAsString range ++ "\n" ++ indentation ++ wrapInDoubleOrTripleQuotes replacement

        Review.Fix.Edit.InsertAt location replacement ->
            "Review.Fix.insertAt\n" ++ indentation ++ locationAsString location ++ "\n" ++ indentation ++ wrapInDoubleOrTripleQuotes replacement


wrapInDoubleOrTripleQuotes : String -> String
wrapInDoubleOrTripleQuotes string =
    if String.contains "\n" string then
        "\"\"\"" ++ string ++ "\"\"\""

    else
        "\"" ++ string ++ "\""


unexpectedConfigurationError : { message : String, details : List String } -> String
unexpectedConfigurationError error =
    failureMessage "UNEXPECTED CONFIGURATION ERROR"
        ("""I found a configuration error for this test:

  """ ++ wrapInQuotes error.message ++ """

  """ ++ formatDetails error.details ++ """

You should use `Review.Test.expectConfigurationError` to expect this
configuration error.""")


missingConfigurationError : String -> String
missingConfigurationError errorMessage =
    failureMessage "MISSING CONFIGURATION ERROR"
        ("""I was expecting a configuration error with the message:

  """ ++ wrapInQuotes errorMessage ++ """

but I could not find it.""")


unexpectedExtract : Encode.Value -> String
unexpectedExtract value =
    failureMessage "UNEXPECTED EXTRACT"
        ("""This rule returned an extract but I did not expect one.

You should use `Review.Test.expectDataExtract` or
`Review.Test.expect`+`Review.Test.dataExtract` to assert that the extract
fits what you expected.

""" ++ formatJson value)


missingExtract : String
missingExtract =
    failureMessage "MISSING EXTRACT"
        """I expected that the rule would extract information using
`Rule.withDataExtractor`, but it doesn't seem that that function was used."""


invalidJsonForExpectedDataExtract : Decode.Error -> String
invalidJsonForExpectedDataExtract parsingError =
    failureMessage "INVALID JSON FOR EXPECTED DATA EXTRACT"
        ("""The string you passed to `expectDataExtract` can't be parsed as valid JSON.

""" ++ Decode.errorToString parsingError)


extractMismatch : Encode.Value -> Encode.Value -> List (Diff.Change String) -> String
extractMismatch actual expected differences =
    failureMessage "DATA EXTRACT MISMATCH"
        ("""I found a different extract than expected. I got the following:

""" ++ formatJson actual ++ """

when I was expecting the following:

""" ++ formatJson expected ++ """

Here are the differences:

""" ++ formatJsonDiff differences)


specifiedMultipleExtracts : String
specifiedMultipleExtracts =
    failureMessage "SPECIFIED MULTIPLE DATA EXTRACTS"
        """You specified multiple expectations for a data extract. Contrary to errors,
it is only possible to have a single data extract at most. Please remove
expectations so that you end up with only a single expectation about the
data extract."""


formatJsonDiff : List (Diff.Change String) -> String
formatJsonDiff differences =
    List.map
        (\difference ->
            case difference of
                Diff.NoChange str ->
                    str

                Diff.Added str ->
                    Ansi.green str

                Diff.Removed str ->
                    Ansi.red str
        )
        differences
        |> String.join "\n"


resultsAreDifferentWhenFilesAreIgnored : { ignoredFiles : List String, missing : List ReviewError, unexpected : List ReviewError } -> String
resultsAreDifferentWhenFilesAreIgnored { ignoredFiles, missing, unexpected } =
    let
        files : String
        files =
            List.map (\file -> "  - " ++ file) ignoredFiles |> String.join "\n"

        difference : String
        difference =
            List.filterMap identity
                [ if List.isEmpty missing then
                    Nothing

                  else
                    Just ("the following errors could not be found anymore:" ++ summarizeErrors missing)
                , if List.isEmpty unexpected then
                    Nothing

                  else
                    Just ("the following errors start appearing:" ++ summarizeErrors unexpected)
                ]
                |> String.join "\n\nand "
    in
    failureMessage "GOT DIFFERENT RESULTS WHEN IGNORING FILES"
        ("""This rule is using `Review.Rule.withIsFileIgnored`, which gives it the
information of which files are ignored.

Without further information, I assume that this information is used solely
to improve performance of the rule, not to change the behavior in any way.
If this is not the case for your rule, then please indicate so in your test
using the `Review.Test.ignoredFilesImpactResults` function.

With that in mind, I tried re-running the test while ignoring some files
and I got different results than before.

When I ignore these files:
""" ++ files ++ """

then """ ++ String.trim difference)


fixForUnknownFile : String -> String
fixForUnknownFile filePath =
    failureMessage "FOUND FIX FOR UNKNOWN FILE"
        ("The rule triggered a fix for " ++ wrapInQuotes filePath ++ """ which I surprisingly don't know at all.

This situation should in theory not happen, but here you are reading this.
Could you kindly open an issue at https://github.com/jfmengels/elm-review
and explain how you got to see this error? Thank you!""")


summarizeErrors : List ReviewError -> String
summarizeErrors errors =
    "\n" ++ (String.join "\n\n" <| List.map describeError errors)


describeError : ReviewError -> String
describeError error =
    """
    { message = """ ++ wrapInDoubleQuotes (Rule.errorMessage error) ++ """
    , filePath = """ ++ wrapInDoubleQuotes (Rule.errorFilePath error) ++ """
    , details = """ ++ formatDetailsForDescription error ++ """
    , range = """ ++ rangeAsStringOnMultipleLines (Rule.errorRange error) ++ """
    , fixes = """ ++ hasFixes error ++ """
    }"""


formatDetailsForDescription : ReviewError -> String
formatDetailsForDescription error =
    "[" ++ (Rule.errorDetails error |> List.map wrapInDoubleQuotes |> String.join ", ") ++ "]"


hasFixes : ReviewError -> String
hasFixes error =
    case Rule.errorFixes error of
        Just [] ->
            "false"

        Nothing ->
            "false"

        Just _ ->
            "true"



-- STYLIZING AND FORMATTING


failureMessage : String -> String -> String
failureMessage title content =
    Ansi.red (Ansi.bold title) ++ "\n\n" ++ content


formatSourceCode : String -> String
formatSourceCode string =
    formatSourceCodeWithFormatter identity (String.lines string)


formatSourceCodeWithFormatter : (List String -> List String) -> List String -> String
formatSourceCodeWithFormatter formatter lines =
    if List.length lines == 1 then
        formatter lines
            |> String.join "\n"
            |> wrapInQuotes

    else
        formatter lines
            |> List.map
                (\str ->
                    if str == "" then
                        ""

                    else
                        "    " ++ str
                )
            |> String.join "\n"
            |> wrapInTripleQuotes


listOccurrencesAsLocations : SourceCode -> String -> List Int -> String
listOccurrencesAsLocations sourceCode under occurrences =
    occurrences
        |> List.map
            (\occurrence ->
                occurrence
                    |> positionAsRange sourceCode under
                    |> rangeAsString
                    |> (++) "  - "
            )
        |> String.join "\n"


positionAsRange : SourceCode -> String -> Int -> Range
positionAsRange sourceCode under position =
    let
        linesBeforeAndIncludingPosition : List String
        linesBeforeAndIncludingPosition =
            sourceCode
                |> String.slice 0 position
                |> String.lines

        startRow : Int
        startRow =
            List.length linesBeforeAndIncludingPosition

        startColumn : Int
        startColumn =
            linesBeforeAndIncludingPosition
                |> ListExtra.last
                |> Maybe.withDefault ""
                |> String.length
                |> (+) 1

        linesInUnder : List String
        linesInUnder =
            String.lines under

        endRow : Int
        endRow =
            startRow + List.length linesInUnder - 1

        endColumn : Int
        endColumn =
            if startRow == endRow then
                startColumn + String.length under

            else
                linesInUnder
                    |> ListExtra.last
                    |> Maybe.withDefault ""
                    |> String.length
                    |> (+) 1
    in
    { start =
        { row = startRow
        , column = startColumn
        }
    , end =
        { row = endRow
        , column = endColumn
        }
    }


listErrorMessagesAndPositions : List ReviewError -> String
listErrorMessagesAndPositions errors =
    errors
        |> List.map errorMessageAndPosition
        |> String.join "\n"


errorMessageAndPosition : ReviewError -> String
errorMessageAndPosition error =
    "  - " ++ wrapInQuotes (Rule.errorMessage error) ++ "\n    at " ++ rangeAsString (Rule.errorRange error)


expectedErrorToString : String -> String
expectedErrorToString expectedErrorMessage =
    "  - " ++ wrapInQuotes expectedErrorMessage


wrapInQuotes : String -> String
wrapInQuotes string =
    "`" ++ string ++ "`"


wrapInDoubleQuotes : String -> String
wrapInDoubleQuotes string =
    "\"" ++ string ++ "\""


wrapInTripleQuotes : String -> String
wrapInTripleQuotes str =
    "```\n" ++ str ++ "\n  ```"


rangeAsString : Range -> String
rangeAsString { start, end } =
    "{ start = " ++ locationAsString start ++ ", end = " ++ locationAsString end ++ " }"


rangeAsStringOnMultipleLines : Range -> String
rangeAsStringOnMultipleLines { start, end } =
    "{ start = " ++ locationAsString start ++ "\n              , end = " ++ locationAsString end ++ "\n              }"


locationAsString : Location -> String
locationAsString location =
    "{ row = " ++ String.fromInt location.row ++ ", column = " ++ String.fromInt location.column ++ " }"


pluralizeErrors : Int -> String
pluralizeErrors n =
    if n == 1 then
        "error"

    else
        "errors"


formatJson : Encode.Value -> String
formatJson value =
    Encode.encode 2 value


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "\n" (List.map deadEndToString deadEnds))


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", column " ++ String.fromInt deadend.col


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting s ->
            "Expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol s ->
            "Expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "Expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "Expecting end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem s ->
            s

        Parser.BadRepeat ->
            "Bad repeat"
