module Lint.Test.ErrorMessage exposing
    ( ExpectedErrorData
    , parsingFailure, messageMismatch, wrongLocation, didNotExpectErrors
    , underMismatch, expectedMoreErrors, tooManyErrors, locationIsAmbiguousInSourceCode
    , impossibleState
    )

{-| Error messages for the `Lint.Test` module.


# Error messages

@docs ExpectedErrorData
@docs parsingFailure, messageMismatch, wrongLocation, didNotExpectErrors
@docs underMismatch, expectedMoreErrors, tooManyErrors, locationIsAmbiguousInSourceCode
@docs impossibleState

-}

import Elm.Syntax.Range exposing (Range)
import Lint.Rule as Rule exposing (Error, Rule)
import List.Extra


{-| An expectation for an error. Use [`error`](#error) to create one.
-}
type alias ExpectedErrorData =
    { message : String
    , under : String
    }


type alias SourceCode =
    String



-- ERROR MESSAGES


didNotExpectErrors : List Error -> String
didNotExpectErrors errors =
    """DID NOT EXPECT ERRORS

I expected no errors but found:

""" ++ listErrorMessagesAndPositions errors


parsingFailure : String
parsingFailure =
    """TEST SOURCE CODE PARSING ERROR

I could not parse the test source code, because it was not valid Elm code.

Hint: Maybe you forgot to add the module definition at the top, like:

  `module A exposing (..)`"""


messageMismatch : ExpectedErrorData -> Error -> String
messageMismatch expectedError error_ =
    """UNEXPECTED ERROR MESSAGE

I was looking for the error with the following message:

  `""" ++ expectedError.message ++ """`

but I found the following error message:

  `""" ++ Rule.errorMessage error_ ++ "`"


underMismatch : Error -> { under : String, codeAtLocation : String } -> String
underMismatch error_ { under, codeAtLocation } =
    """UNEXPECTED ERROR LOCATION

I found an error with the following message:

  `""" ++ Rule.errorMessage error_ ++ """`

which I was expecting, but I found it under:

  """ ++ formatSourceCode codeAtLocation ++ """

when I was expecting it under:

  """ ++ formatSourceCode under ++ """

Hint: Maybe you're passing the `Range` of a wrong node when
calling `Rule.error`"""


wrongLocation : Error -> Range -> String -> String
wrongLocation error_ range under =
    """UNEXPECTED ERROR LOCATION

I was looking for the error with the following message:

  `""" ++ Rule.errorMessage error_ ++ """`

under the following code:

  """ ++ formatSourceCode under ++ """

and I found it, but the exact location you specified is not the one I found.

I was expecting the error at:

  """ ++ rangeAsString range ++ """

but I found it at:

  """ ++ rangeAsString (Rule.errorRange error_)


expectedMoreErrors : List ExpectedErrorData -> String
expectedMoreErrors missingExpectedErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length missingExpectedErrors
    in
    """RULE REPORTED LESS ERRORS THAN EXPECTED

I expected to see """
        ++ (String.fromInt numberOfErrors ++ " more " ++ pluralizeErrors numberOfErrors ++ ":\n\n")
        ++ (missingExpectedErrors
                |> List.map expectedErrorToString
                |> String.join "\n"
           )


tooManyErrors : List Error -> String
tooManyErrors extraErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length extraErrors
    in
    """RULE REPORTED MORE ERRORS THAN EXPECTED

I found """
        ++ (String.fromInt numberOfErrors ++ " " ++ pluralizeErrors numberOfErrors ++ " too many:\n\n")
        ++ listErrorMessagesAndPositions extraErrors


locationIsAmbiguousInSourceCode : SourceCode -> Error -> String -> List Int -> String
locationIsAmbiguousInSourceCode sourceCode error_ under occurrencesInSourceCode =
    """AMBIGUOUS ERROR LOCATION

Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  `""" ++ Rule.errorMessage error_ ++ """`

and expecting to see it under:

  """ ++ formatSourceCode under ++ """

I found """ ++ String.fromInt (List.length occurrencesInSourceCode) ++ """ locations where that code appeared. Please
use `Lint.Rule.atExactly` to make the part you were targetting unambiguous.

Tip: I found them at:
""" ++ listOccurrencesAsLocations sourceCode under occurrencesInSourceCode


impossibleState : String
impossibleState =
    """ELM-LINT IMPOSSIBLE STATE

Oh no! I'm in an impossible state. I found an error at a location that I could not find back. Please let me know and give me an SSCCE (http://sscce.org/) here: https://github.com/jfmengels/elm-lint/issues."""



-- STYLIZING AND FORMATTING


formatSourceCode : String -> String
formatSourceCode string =
    let
        lines =
            String.lines string
    in
    if List.length lines == 1 then
        "`" ++ string ++ "`"

    else
        lines
            |> List.map (\str -> "    " ++ str)
            |> String.join "\n"
            |> (\str -> "```\n" ++ str ++ "\n  ```")


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
                |> List.Extra.last
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
                    |> List.Extra.last
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


listErrorMessagesAndPositions : List Error -> String
listErrorMessagesAndPositions errors =
    errors
        |> List.map errorMessageAndPosition
        |> String.join "\n"


errorMessageAndPosition : Error -> String
errorMessageAndPosition error_ =
    "  - " ++ wrapInQuotes (Rule.errorMessage error_) ++ "\n    at " ++ rangeAsString (Rule.errorRange error_)


expectedErrorToString : ExpectedErrorData -> String
expectedErrorToString expectedError =
    "  - " ++ wrapInQuotes expectedError.message


wrapInQuotes : String -> String
wrapInQuotes string =
    "`" ++ string ++ "`"


rangeAsString : Range -> String
rangeAsString { start, end } =
    "{ start = { row = " ++ String.fromInt start.row ++ ", column = " ++ String.fromInt start.column ++ " }, end = { row = " ++ String.fromInt end.row ++ ", column = " ++ String.fromInt end.column ++ " } }"


pluralizeErrors : Int -> String
pluralizeErrors n =
    if n == 1 then
        "error"

    else
        "errors"
