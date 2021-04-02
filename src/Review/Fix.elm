module Review.Fix exposing
    ( Fix, removeRange, replaceRangeBy, insertAt
    , FixResult(..), Problem(..), fix
    , toRecord
    )

{-| Tools to write automatic error fixes.

When creating a [`Review.Rule.Error`](./Review-Rule#Error), you can provide an automatic
fix for the error using [`Review.Rule.errorWithFix`](./Review-Rule#errorWithFix)
or other functions that end with "withFix" so that the user doesn't need to fix
the problem themselves.

In the [CLI], the user can ask to fix the errors automatically, and in doing so,
they will be presented by a fix which they can accept or refuse. If the fix gets
refused, then the next fixable error will be presented. Otherwise, if the fix
gets accepted, the file will be applied and the fixed file content get analyzed
again by the different rules in the user's configuration, and then another fix
will be presented. When there are no more fixable errors, the remaining errors
will be reported, just like when the user doesn't request errors to be automatically
fixed.

In summary, errors will be presented one by one and the user will validate them.
The [CLI] also proposes an option to fix all the errors, which applies each fix
one by one and then asks the user to confirm the cumulated fix.


# Guidelines

An automatic fix, when applied, should resolve the reported error completely.
This means that when the automatic fix is applied, the user should not have to
think about the error anymore or have to do additional work.

Imagine if the user applies a lot of automatic fixes all at once. We don't want them to have to
remember having to do something, otherwise we may have just offloaded a lot of
work that they may forget to do. In that case, it is better not to provide a fix
at all, so that they keep a reminder and the details of how to fix the problem.

An automatic fix should resolve only the reported error, not try to fix other
potential errors. By only fixing one error at a time, the fix will be easier for
the user to digest and understand. The file will be re-reviewed when the fix is
applied, and then another error can fix that one.


# When (not) to provide an automatic fix?

For users, having an automatic fix always feels like a nice-to-have and they may
request you to provide some, but they are not mandatory, and in some cases, it
is better not to have any.


## Reasons not to provide an automatic fix


### A complete automatic fix is not possible

Sometimes, just by going through the whole file, you are missing some of the
information needed to generate the right fix. Instead of providing a partial or
potentially incorrect fix, it would be better to provide more details, hints or
suggestions.


### The fix would result in a compiler error

An automatic fix should not cause changes that would break the file or the
project. In some cases, we can detect that the [fix will break things](#Problem),
like if the result of the fix is invalid Elm code (as in resulting in a parsing
error), but ultimately we can't detect that the project will still compile after
the fix is applied.

Users are notified that an automatic fix is available. For performance reasons,
we only check that a fix is valid before presenting it to the user and ignore it
if it turns out to be invalid. This means that the user will be disappointed or
confused when the error ends up not being enforced. The only way we have to
prevent this is to write tests, as fixes are applied in tests.


### The user should learn about problem and how to solve it

Sometimes problems are learning opportunities, and it is worth having the user
spend some time reading through the details of the error and trying several
alternatives in order to understand the problem and the tradeoffs of the
solutions. You can guide them by using great error details!


## Reasons to provide an automatic fix

The reasons to provide an automatic fix are basically the opposite of the
reasons not to provide an automatic fix:

  - We know how to fix the problem completely and accurately
  - The task is menial and the user will not learn anything by fixing the error
    themselves


# Strategies for writing automatic fixes effectively


### Write a lot of tests

Automatic fixes are more error-prone than rules, especially since we may work
with re-writing ports of the code, for which the AST does not provide the
current formatting of a file (there is no information about spacing,
line-breaks, ...). I suggest writing a lot of tests, and especially write tests
where the formatting of the original file is a bit odd, as you may for instance
unknowingly attempt to delete characters next to the thing you wanted to remove.


### Store ranges in the context if necessary

Fixes work with ranges or position. If the context of a different element is not
available in the scope of where you create the error, then you should store it
in the context of your rule.


# Creating a fix

@docs Fix, removeRange, replaceRangeBy, insertAt


# Applying fixes

@docs FixResult, Problem, fix

[CLI]: https://github.com/jfmengels/node-elm-review


# Tooling

@docs toRecord

-}

import Array
import Elm.Parser
import Elm.Project
import Elm.Syntax.Range exposing (Range)
import Json.Decode as Decode
import Review.Error as Error
import Vendor.ListExtra as ListExtra



-- DEFINITION


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type alias Fix =
    Error.Fix



-- CONSTRUCTORS


{-| Remove the code in between a range.
-}
removeRange : Range -> Fix
removeRange =
    Error.Removal


{-| Replace the code in between a range by some other code.
-}
replaceRangeBy : Range -> String -> Fix
replaceRangeBy =
    Error.Replacement


{-| Insert some code at the given position.
-}
insertAt : { row : Int, column : Int } -> String -> Fix
insertAt =
    Error.InsertAt



-- APPLYING FIXES


{-| Represents the result of having applied a list of fixes to a source code.
-}
type FixResult
    = Successful String
    | Errored Problem


{-| Represents a problem that may have occurred when attempting to apply a list
of fixes.
-}
type Problem
    = Unchanged
    | SourceCodeIsNotValid String
    | HasCollisionsInFixRanges


{-| Apply the changes on the source code.
-}
fix : Error.Target -> List Fix -> String -> FixResult
fix target fixes sourceCode =
    case target of
        Error.Module ->
            tryToApplyFix
                fixes
                sourceCode
                (\resultAfterFix -> (Elm.Parser.parse resultAfterFix |> Result.toMaybe) /= Nothing)

        Error.Readme ->
            tryToApplyFix
                fixes
                sourceCode
                (always True)

        Error.ElmJson ->
            tryToApplyFix
                fixes
                sourceCode
                (\resultAfterFix -> (Decode.decodeString Elm.Project.decoder resultAfterFix |> Result.toMaybe) /= Nothing)

        Error.Global ->
            Errored Unchanged

        Error.UserGlobal ->
            Errored Unchanged


tryToApplyFix : List Fix -> String -> (String -> Bool) -> FixResult
tryToApplyFix fixes sourceCode isValidSourceCode =
    if containRangeCollisions fixes then
        Errored HasCollisionsInFixRanges

    else
        let
            resultAfterFix : String
            resultAfterFix =
                fixes
                    |> List.sortBy (rangePosition >> negate)
                    |> List.foldl applyFix (String.lines sourceCode)
                    |> String.join "\n"
        in
        if sourceCode == resultAfterFix then
            Errored Unchanged

        else if isValidSourceCode resultAfterFix then
            Successful resultAfterFix

        else
            Errored (SourceCodeIsNotValid resultAfterFix)


containRangeCollisions : List Fix -> Bool
containRangeCollisions fixes =
    fixes
        |> List.map getFixRange
        |> ListExtra.uniquePairs
        |> List.any (\( a, b ) -> collide a b)


getFixRange : Fix -> Range
getFixRange fix_ =
    case fix_ of
        Error.Replacement range _ ->
            range

        Error.Removal range ->
            range

        Error.InsertAt position _ ->
            { start = position, end = position }


collide : Range -> Range -> Bool
collide a b =
    case comparePosition a.end b.start of
        LT ->
            False

        EQ ->
            False

        GT ->
            case comparePosition b.end a.start of
                LT ->
                    False

                EQ ->
                    False

                GT ->
                    True


rangePosition : Fix -> Int
rangePosition fix_ =
    positionAsInt <|
        case fix_ of
            Error.Replacement range _ ->
                range.start

            Error.Removal range ->
                range.start

            Error.InsertAt position _ ->
                position


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long. Then, as long as ranges don't overlap,
    -- this should work fine.
    row * 1000000 + column


comparePosition : { row : Int, column : Int } -> { row : Int, column : Int } -> Order
comparePosition a b =
    case compare a.row b.row of
        EQ ->
            compare a.column b.column

        order ->
            order


applyFix : Fix -> List String -> List String
applyFix fix_ lines =
    lines
        |> (case fix_ of
                Error.Replacement range replacement ->
                    applyReplace range replacement

                Error.Removal range ->
                    applyReplace range ""

                Error.InsertAt position insertion ->
                    applyReplace { start = position, end = position } insertion
           )


applyReplace : Range -> String -> List String -> List String
applyReplace range replacement lines =
    let
        linesBefore : List String
        linesBefore =
            lines
                |> List.take (range.start.row - 1)

        linesAfter : List String
        linesAfter =
            lines
                |> List.drop range.end.row

        startLine : String
        startLine =
            getRowAtLine lines (range.start.row - 1)
                |> String.slice 0 (range.start.column - 1)

        endLine : String
        endLine =
            getRowAtLine lines (range.end.row - 1)
                |> String.dropLeft (range.end.column - 1)
    in
    List.concat
        [ linesBefore
        , startLine ++ replacement ++ endLine |> String.lines
        , linesAfter
        ]


getRowAtLine : List String -> Int -> String
getRowAtLine lines rowIndex =
    case lines |> Array.fromList |> Array.get rowIndex of
        Just line ->
            if String.trim line /= "" then
                line

            else
                ""

        Nothing ->
            ""



-- TOOLING


{-| Describe a fix as a range to replace by something else.

This is meant for external tooling. You shouldn't have to care about this

-}
toRecord : Fix -> { range : Range, replacement : String }
toRecord fix_ =
    case fix_ of
        Error.Replacement range replacement ->
            { range = range
            , replacement = replacement
            }

        Error.Removal range ->
            { range = range
            , replacement = ""
            }

        Error.InsertAt position replacement ->
            { range = { start = position, end = position }
            , replacement = replacement
            }
