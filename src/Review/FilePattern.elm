module Review.FilePattern exposing (FilePattern, compact, exclude, excludeFolder, include, match)

import Glob exposing (Glob)
import Review.Rule exposing (globalError)



{-

   TODO Compile List FilePattern to a new type that leads either a configuration error (directly or indirectly)
   or to a usable type.

   TODO We should be able to figure out whether the patterns include exclusion patterns. If there are none, then
   the matching becomes simpler because the first match will lead to returning True.

   We can probably do this partially as well: having a match-only after the last exclusion pattern.

   TODO Put excluded directories first
-}


type FilePattern
    = Include Glob
    | Exclude Glob
    | ExcludeFolder Glob
    | InvalidGlob String


type alias Summary =
    { folders : List Glob
    , includeExclude : List CompactFilePattern
    }


type CompactFilePattern
    = CompactInclude (List Glob)
    | CompactExclude (List Glob)


compact : List FilePattern -> Result (List String) Summary
compact filePatterns =
    compactHelp filePatterns { folders = [], includeExclude = [] }


compactHelp : List FilePattern -> Summary -> Result (List String) Summary
compactHelp filePatterns accSummary =
    case filePatterns of
        [] ->
            Ok accSummary

        (Include pattern) :: rest ->
            compactInclude rest [ pattern ] accSummary

        (Exclude pattern) :: rest ->
            compactExclude rest [ pattern ] accSummary

        (ExcludeFolder pattern) :: rest ->
            compactHelp rest { folders = pattern :: accSummary.folders, includeExclude = accSummary.includeExclude }

        (InvalidGlob pattern) :: rest ->
            Err (compactErrors rest [ pattern ])


compactInclude : List FilePattern -> List Glob -> Summary -> Result (List String) Summary
compactInclude filePatterns accGlobs accSummary =
    case filePatterns of
        [] ->
            Ok
                { folders = accSummary.folders
                , includeExclude = CompactInclude accGlobs :: accSummary.includeExclude
                }

        (Include pattern) :: rest ->
            compactInclude rest (pattern :: accGlobs) accSummary

        (Exclude pattern) :: rest ->
            compactExclude rest
                [ pattern ]
                { folders = accSummary.folders
                , includeExclude = CompactInclude accGlobs :: accSummary.includeExclude
                }

        (ExcludeFolder pattern) :: rest ->
            compactInclude rest
                accGlobs
                { folders = pattern :: accSummary.folders
                , includeExclude = accSummary.includeExclude
                }

        (InvalidGlob invalidGlobStr) :: rest ->
            Err (compactErrors rest [ invalidGlobStr ])


compactExclude : List FilePattern -> List Glob -> Summary -> Result (List String) Summary
compactExclude filePatterns accGlobs accSummary =
    case filePatterns of
        [] ->
            Ok { folders = accSummary.folders, includeExclude = CompactExclude accGlobs :: accSummary.includeExclude }

        (Include pattern) :: rest ->
            compactInclude rest
                [ pattern ]
                { folders = accSummary.folders
                , includeExclude = CompactExclude accGlobs :: accSummary.includeExclude
                }

        (Exclude pattern) :: rest ->
            compactExclude rest (pattern :: accGlobs) accSummary

        (ExcludeFolder pattern) :: rest ->
            compactExclude rest
                accGlobs
                { folders = pattern :: accSummary.folders
                , includeExclude = accSummary.includeExclude
                }

        (InvalidGlob invalidGlobStr) :: rest ->
            Err (compactErrors rest [ invalidGlobStr ])


compactErrors : List FilePattern -> List String -> List String
compactErrors filePatterns accGlobStrings =
    case filePatterns of
        [] ->
            List.reverse accGlobStrings

        (InvalidGlob invalidGlobStr) :: rest ->
            compactErrors rest (invalidGlobStr :: accGlobStrings)

        _ :: rest ->
            compactErrors rest accGlobStrings


include : String -> FilePattern
include globStr =
    case Glob.fromString globStr of
        Ok glob ->
            Include glob

        Err _ ->
            InvalidGlob globStr


exclude : String -> FilePattern
exclude globStr =
    case Glob.fromString globStr of
        Ok glob ->
            Exclude glob

        Err _ ->
            InvalidGlob globStr


excludeFolder : String -> FilePattern
excludeFolder globStr =
    case Glob.fromString (toFolder globStr) of
        Ok glob ->
            ExcludeFolder glob

        Err _ ->
            InvalidGlob globStr


toFolder : String -> String
toFolder globStr =
    if String.endsWith "/" globStr then
        globStr ++ "**/*"

    else
        globStr ++ "/**/*"


match : Summary -> String -> Bool
match summary str =
    if List.any (\folderGlob -> Glob.match folderGlob str) summary.folders then
        False

    else
        match2Help summary.includeExclude str


match2Help : List CompactFilePattern -> String -> Bool
match2Help filePatterns str =
    case filePatterns of
        [] ->
            False

        (CompactInclude globs) :: rest ->
            if List.any (\glob -> Glob.match glob str) globs then
                True

            else
                match2Help rest str

        (CompactExclude globs) :: rest ->
            if List.any (\glob -> Glob.match glob str) globs then
                False

            else
                match2Help rest str
