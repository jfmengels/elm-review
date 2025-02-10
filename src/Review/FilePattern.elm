module Review.FilePattern exposing
    ( FilePattern
    , include, exclude, excludeDirectory
    , Summary, compact, match
    , toStrings
    )

{-| A module for selecting multiple files from the file system
using [`glob`] patterns and negated patterns.

    import Review.FilePattern as FilePattern exposing (FilePattern)

    filePatterns : List FilePattern
    filePatterns =
        [ FilePattern.include "**/*.css"
        , FilePattern.exclude "**/*-test.css"
        , FilePattern.excludeDirectory "ignore-folder/"
        ]

Some `elm-review` APIs require a `List FilePattern` as an argument to figure out a list of files to match or not match.

This list works similarly to [`.gitignore`] files: any matching file excluded by a previous pattern will become included again.
Files that are in [excluded directories](#excludeDirectory) are ignored entirely.

A file pattern is always relative to the project's `elm.json` file, is case-sensitive,
and should be written in the Unix style (`src/Some/File.elm`, not `src\Some\File.elm`).

@docs FilePattern
@docs include, exclude, excludeDirectory


## Supported patterns

The supported patterns are the following:

  - `?` matches an unknown single character (except `/`). "a?c" would match "abc" or "a5c", but not "ac".
  - `*` matches any number of unknown characters (except `/`). "some-\*.txt" would match "some-file.txt" and "some-other-file.txt", but not "other-file.txt" or "some-folder/file.txt".
  - `**` matches any number of sub-directories. "projects/\*\*/README.md" would match "projects/README.md", "projects/a/README.md" and "projects/a/b/README.md". If you desire to include all files in a folder, then you need to end the pattern with `/**/*` (eg "projects/\*\*/\*" or "projects/\*\*/\*.md").
  - `[characters]` matches one of the specified characters. `a[bc]d` would match "abc" and "acd", but not "axd".
  - `[^characters]` matches anything that is not one of the specified characters. `a[^bc]d` would match "axc", but not "abd" or "acd".
  - `[character1-character2]` matches a range of characters. `a[a-z]d` would match "aac" and "azd", but not "a5d".
  - `{string1|string2}` matches one of the provided strings. "file.{js|ts}" would match "file.js" and "file.ts" but not "file.md".


## Using FilePattern

@docs Summary, compact, match
@docs toStrings

[`glob`]: https://en.wikipedia.org/wiki/Glob_%28programming%29
[`.gitignore`]: https://git-scm.com/docs/gitignore#_pattern_format

-}

import Glob exposing (Glob)


{-| A pattern to include or exclude files from a selection.
-}
type FilePattern
    = Include String
    | Exclude String
    | ExcludeDirectory String


{-| Create a `FilePattern` that includes files that match a Glob-like pattern.

    [ FilePattern.include "CHANGELOG.md"
    , FilePattern.include "src/**/*.elm"
    , FilePattern.include "*.css"
    ]

-}
include : String -> FilePattern
include =
    Include


{-| Create a `FilePattern` that excludes files that match a Glob-like pattern.

    [ FilePattern.include "**/*.css"
    , FilePattern.exclude "exception.css"
    ]

Files that get excluded this way can be re-included through `FilePattern.include`.

    [ FilePattern.include "**/*.css"
    , FilePattern.exclude "exception-*.css"
    , FilePattern.include "exception-among-exceptions.css"
    ]

-}
exclude : String -> FilePattern
exclude =
    Exclude


{-| Create a `FilePattern` that excludes a whole directory.
Files that get excluded this way can't be re-included.

    [ FilePattern.include "**/*.css"
    , FilePattern.excludeDirectory "build/"
    ]

-}
excludeDirectory : String -> FilePattern
excludeDirectory =
    ExcludeDirectory


{-| Compiled version of a list of `FilePattern`s.
-}
type Summary
    = Summary SummaryInfo


type alias SummaryInfo =
    { includeExclude : List CompactFilePattern
    , excludedDirectories : List Glob
    , strings : List { pattern : String, included : Bool }
    , excludedDirectoriesStrings : List String
    }


{-| Compile a list of `FilePattern`s.
-}
compact : List FilePattern -> Result (List String) Summary
compact filePatterns =
    compactBase filePatterns
        { includeExclude = []
        , excludedDirectories = []
        , strings = []
        , excludedDirectoriesStrings = []
        }
        |> Result.map
            (\summary ->
                Summary
                    { includeExclude = summary.includeExclude
                    , excludedDirectories = summary.excludedDirectories
                    , strings = List.reverse summary.strings
                    , excludedDirectoriesStrings = List.reverse summary.excludedDirectoriesStrings
                    }
            )


compactBase : List FilePattern -> SummaryInfo -> Result (List String) SummaryInfo
compactBase filePatterns accSummary =
    case filePatterns of
        [] ->
            Ok
                { includeExclude = accSummary.includeExclude
                , excludedDirectories = accSummary.excludedDirectories
                , strings = List.reverse accSummary.strings
                , excludedDirectoriesStrings = List.reverse accSummary.excludedDirectoriesStrings
                }

        (Include raw) :: rest ->
            case Glob.fromString raw of
                Ok pattern ->
                    compactHelp rest [ pattern ] True (addRawIncludeExclude raw True accSummary)

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (Exclude raw) :: rest ->
            case Glob.fromString raw of
                Ok pattern ->
                    compactHelp rest [ pattern ] False (addRawIncludeExclude raw False accSummary)

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (ExcludeDirectory raw) :: rest ->
            case Glob.fromString (toDirectory raw) of
                Ok pattern ->
                    compactBase rest
                        { includeExclude = accSummary.includeExclude
                        , excludedDirectories = pattern :: accSummary.excludedDirectories
                        , strings = accSummary.strings
                        , excludedDirectoriesStrings = raw :: accSummary.excludedDirectoriesStrings
                        }

                Err _ ->
                    Err (compactErrors rest [ raw ])


type CompactFilePattern
    = CompactInclude (List Glob)
    | CompactExclude (List Glob)


compactHelp : List FilePattern -> List Glob -> Bool -> SummaryInfo -> Result (List String) SummaryInfo
compactHelp filePatterns accGlobs included accSummary =
    case filePatterns of
        [] ->
            Ok
                { includeExclude =
                    (if included then
                        CompactInclude accGlobs

                     else
                        CompactExclude accGlobs
                    )
                        :: accSummary.includeExclude
                , excludedDirectories = accSummary.excludedDirectories
                , strings = accSummary.strings
                , excludedDirectoriesStrings = accSummary.excludedDirectoriesStrings
                }

        (Include raw) :: rest ->
            case Glob.fromString raw of
                Ok pattern ->
                    if included then
                        compactHelp rest (pattern :: accGlobs) included (addRawIncludeExclude raw included accSummary)

                    else
                        compactHelp rest
                            [ pattern ]
                            True
                            { includeExclude = CompactExclude accGlobs :: accSummary.includeExclude
                            , excludedDirectories = accSummary.excludedDirectories
                            , strings = { pattern = raw, included = True } :: accSummary.strings
                            , excludedDirectoriesStrings = accSummary.excludedDirectoriesStrings
                            }

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (Exclude raw) :: rest ->
            case Glob.fromString raw of
                Ok pattern ->
                    if included then
                        compactHelp rest
                            [ pattern ]
                            False
                            { includeExclude = CompactInclude accGlobs :: accSummary.includeExclude
                            , excludedDirectories = accSummary.excludedDirectories
                            , strings = { pattern = raw, included = False } :: accSummary.strings
                            , excludedDirectoriesStrings = accSummary.excludedDirectoriesStrings
                            }

                    else
                        compactHelp rest (pattern :: accGlobs) included (addRawIncludeExclude raw included accSummary)

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (ExcludeDirectory raw) :: rest ->
            case Glob.fromString (toDirectory raw) of
                Ok pattern ->
                    compactHelp rest
                        accGlobs
                        included
                        { includeExclude = accSummary.includeExclude
                        , excludedDirectories = pattern :: accSummary.excludedDirectories
                        , strings = accSummary.strings
                        , excludedDirectoriesStrings = raw :: accSummary.excludedDirectoriesStrings
                        }

                Err _ ->
                    Err (compactErrors rest [ raw ])


toDirectory : String -> String
toDirectory globStr =
    if String.endsWith "/" globStr then
        globStr ++ "**/*"

    else
        globStr ++ "/**/*"


compactErrors : List FilePattern -> List String -> List String
compactErrors filePatterns accGlobStrings =
    case filePatterns of
        [] ->
            List.reverse accGlobStrings

        filePattern :: rest ->
            let
                raw : String
                raw =
                    case filePattern of
                        Include s ->
                            s

                        Exclude s ->
                            s

                        ExcludeDirectory s ->
                            s
            in
            case Glob.fromString raw of
                Ok _ ->
                    compactErrors rest accGlobStrings

                Err _ ->
                    compactErrors rest (raw :: accGlobStrings)


addRawIncludeExclude : String -> Bool -> SummaryInfo -> SummaryInfo
addRawIncludeExclude string included summary =
    { includeExclude = summary.includeExclude
    , excludedDirectories = summary.excludedDirectories
    , strings = { pattern = string, included = included } :: summary.strings
    , excludedDirectoriesStrings = summary.excludedDirectoriesStrings
    }


{-| Check if a file path matches file patterns.
-}
match : { includeByDefault : Bool } -> Summary -> String -> Bool
match { includeByDefault } (Summary summary) str =
    if List.any (\dirGlob -> Glob.match dirGlob str) summary.excludedDirectories then
        False

    else
        matchHelp includeByDefault summary.includeExclude str


matchHelp : Bool -> List CompactFilePattern -> String -> Bool
matchHelp includeByDefault filePatterns str =
    case filePatterns of
        [] ->
            includeByDefault

        (CompactInclude globs) :: rest ->
            if List.any (\glob -> Glob.match glob str) globs then
                True

            else
                matchHelp includeByDefault rest str

        (CompactExclude globs) :: rest ->
            if List.any (\glob -> Glob.match glob str) globs then
                False

            else
                matchHelp includeByDefault rest str


{-| Stringify the file patterns in a way that is easily understandable by a JavaScript Glob-like API.
-}
toStrings : Summary -> { files : List { pattern : String, included : Bool }, excludedDirectories : List String }
toStrings (Summary summary) =
    { files = summary.strings
    , excludedDirectories = summary.excludedDirectoriesStrings
    }
