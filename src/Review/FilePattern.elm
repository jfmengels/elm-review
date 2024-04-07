module Review.FilePattern exposing (FilePattern, compact, exclude, excludeFolder, include, match)

import Glob exposing (Glob)



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
    { includeExclude : List CompactFilePattern
    , excludeFolders : List Glob
    }


type CompactFilePattern
    = CompactInclude (List Glob)
    | CompactExclude (List Glob)


compact : List FilePattern -> Result (List String) Summary
compact filePatterns =
    compactBase filePatterns
        { includeExclude = []
        , excludeFolders = []
        }


type AccumulatorType
    = IncludeAcc
    | ExcludeAcc


compactBase : List FilePattern -> Summary -> Result (List String) Summary
compactBase filePatterns accSummary =
    case filePatterns of
        [] ->
            Ok accSummary

        (Include pattern) :: rest ->
            compactHelp rest [ pattern ] IncludeAcc accSummary

        (Exclude pattern) :: rest ->
            compactHelp rest [ pattern ] ExcludeAcc accSummary

        (ExcludeFolder pattern) :: rest ->
            compactBase rest
                { includeExclude = accSummary.includeExclude
                , excludeFolders = pattern :: accSummary.excludeFolders
                }

        (InvalidGlob pattern) :: rest ->
            Err (compactErrors rest [ pattern ])


compactHelp : List FilePattern -> List Glob -> AccumulatorType -> Summary -> Result (List String) Summary
compactHelp filePatterns accGlobs accumulatorType accSummary =
    case filePatterns of
        [] ->
            Ok
                { includeExclude =
                    (case accumulatorType of
                        IncludeAcc ->
                            CompactInclude accGlobs

                        ExcludeAcc ->
                            CompactExclude accGlobs
                    )
                        :: accSummary.includeExclude
                , excludeFolders = accSummary.excludeFolders
                }

        (Include pattern) :: rest ->
            case accumulatorType of
                IncludeAcc ->
                    compactHelp rest (pattern :: accGlobs) accumulatorType accSummary

                ExcludeAcc ->
                    compactHelp rest
                        [ pattern ]
                        IncludeAcc
                        { includeExclude = CompactExclude accGlobs :: accSummary.includeExclude
                        , excludeFolders = accSummary.excludeFolders
                        }

        (Exclude pattern) :: rest ->
            case accumulatorType of
                IncludeAcc ->
                    compactHelp rest
                        [ pattern ]
                        ExcludeAcc
                        { includeExclude = CompactInclude accGlobs :: accSummary.includeExclude
                        , excludeFolders = accSummary.excludeFolders
                        }

                ExcludeAcc ->
                    compactHelp rest (pattern :: accGlobs) accumulatorType accSummary

        (ExcludeFolder pattern) :: rest ->
            compactHelp rest
                accGlobs
                accumulatorType
                { includeExclude = accSummary.includeExclude
                , excludeFolders = pattern :: accSummary.excludeFolders
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
    if List.any (\folderGlob -> Glob.match folderGlob str) summary.excludeFolders then
        False

    else
        matchHelp summary.includeExclude str


matchHelp : List CompactFilePattern -> String -> Bool
matchHelp filePatterns str =
    case filePatterns of
        [] ->
            False

        (CompactInclude globs) :: rest ->
            if List.any (\glob -> Glob.match glob str) globs then
                True

            else
                matchHelp rest str

        (CompactExclude globs) :: rest ->
            if List.any (\glob -> Glob.match glob str) globs then
                False

            else
                matchHelp rest str
