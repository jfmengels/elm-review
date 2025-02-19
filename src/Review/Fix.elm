module Review.Fix exposing
    ( Edit, Fix, removeRange, replaceRangeBy, insertAt
    , toRecord, FixResult(..), Problem(..), fix
    )

{-| Tools to write automatic error fixes.

When creating a [`Review.Rule.Error`](./Review-Rule#Error), you can provide an automatic
fix for the error using [`Review.Rule.errorWithFix`](./Review-Rule#errorWithFix)
or other functions that end with "withFix" so that the user doesn't need to fix
the problem themselves.

In the [CLI](https://github.com/jfmengels/node-elm-review), the user can ask to fix the errors automatically, and in doing so,
they will be presented a fix which they can accept or refuse. If the fix gets
refused, then the next fixable error will be presented. Otherwise, if the fix
gets accepted, the fix will be applied and the fixed file content get analyzed
again by the different rules in the user's configuration, and then another fix
will be presented. When there are no more fixable errors, the remaining errors
will be reported, just like when the user doesn't request errors to be
automatically fixed.

In summary, errors will be presented one by one and the user will validate them.
The [CLI] also offers the `--fix-all` flag an option to fix all the errors,
which applies each available fix and then asks the user to confirm the cumulated result.

**NOTE**: The API in this module is focused creating file edits as part of a fix.
If you're looking for ways to provide fixes across multiple files or removing files,
check out the documentation for [`FixV2`](./Review-Rule#FixV2).


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
project. In some cases, we can detect that the [fix will break things](./Review-Fix-FixProblem),
like if the result of the fix is invalid Elm code (as in resulting in a parsing
error), but ultimately we can't detect that the project will still compile after
the fix is applied.

Users are notified that an automatic fix is available. For performance reasons,
we only partially check that a fix is valid before presenting it to the user,
and ignore it if it turns out to be invalid. This means that the user might get
disappointed or confused when the error ends up not being enforced. The only way
we have to prevent this is to write tests, as fixes are validated thoroughly in tests.


### The user should learn about the problem and how to solve it

Sometimes problems are learning opportunities, and it is worth having the user
spend some time reading through the details of the error and trying several
alternatives in order to understand the problem and the tradeoffs of the
solutions. You can guide them by providing a helpful explanation in the error details.


## Reasons to provide an automatic fix

The reasons to provide an automatic fix are basically the opposite of the
reasons not to provide an automatic fix:

  - We know how to fix the problem completely and accurately
  - The task is menial and the user will not learn much by fixing the error
    themselves


# Strategies for writing automatic fixes effectively


### Write a lot of tests

Automatic fixes are more error-prone than rules, especially since we may work
with re-writing parts of the code, for which the AST does not provide the
current formatting of a file (there is no information about spacing,
line-breaks, ...). I suggest writing a lot of tests, and especially write tests
where the formatting of the original file is a bit odd, as you may for instance
unknowingly attempt to delete characters next to the thing you wanted to remove.


### Store ranges in the context if necessary

Edits work with ranges or position. If the context of a different element is not
available in the scope of where you create the error, then you should store it
in the context of your rule.


# Creating a fix

@docs Edit, Fix, removeRange, replaceRangeBy, insertAt


## Internals

@docs toRecord, FixResult, Problem, fix

[CLI]: https://github.com/jfmengels/node-elm-review

-}

import Elm.Syntax.Range exposing (Range)
import Review.Error as Error
import Review.Fix.Edit as Edit



-- DEFINITION


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.

This is the new name for [`Fix`](#Fix), prefer this one. The two types are interchangeable.

-}
type alias Edit =
    Edit.Edit


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.

This is the old name for [`Edit`](#Edit) and will be removed in a future major version.
The two types are interchangeable.

-}
type alias Fix =
    Edit.Fix



-- CONSTRUCTORS


{-| Remove the code in between a range.
-}
removeRange : Range -> Fix
removeRange =
    Edit.Removal


{-| Replace the code in between a range by some other code.
-}
replaceRangeBy : Range -> String -> Fix
replaceRangeBy range replacement =
    if replacement == "" then
        Edit.Removal range

    else
        Edit.Replacement range replacement


{-| Insert some code at the given position.
-}
insertAt : { row : Int, column : Int } -> String -> Fix
insertAt =
    Edit.InsertAt



-- APPLYING FIXES


{-| Represents the result of having applied a list of fixes to a source code.
-}
type FixResult
    = Successful String
    | Errored Problem


{-| Represents a problem that may have occurred when attempting to apply a list
of fixes.
-}
type
    Problem
    -- TODO Breaking change: Merge with Fix.Problem
    = Unchanged
    | SourceCodeIsNotValid String
    | HasCollisionsInFixRanges


{-| Apply the changes on the source code.
-}
fix : Error.Target -> List Fix -> String -> FixResult
fix _ _ _ =
    -- TODO Breaking change: Remove this function, and other types
    Errored Unchanged



-- TOOLING


{-| Describe a fix as a range to replace by something else.

This is meant for external tooling. You shouldn't have to care about this

-}
toRecord : Fix -> { range : Range, replacement : String }
toRecord fix_ =
    case fix_ of
        Edit.Replacement range replacement ->
            { range = range
            , replacement = replacement
            }

        Edit.Removal range ->
            { range = range
            , replacement = ""
            }

        Edit.InsertAt position replacement ->
            { range = { start = position, end = position }
            , replacement = replacement
            }
