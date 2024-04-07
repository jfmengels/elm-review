module Review.FilePattern exposing (FilePattern, compact, exclude, excludeFolder, include, match, toStrings)

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
    = Include ( String, Glob )
    | Exclude ( String, Glob )
    | ExcludeFolder ( String, Glob )
    | InvalidGlob String


type alias Summary =
    { includeExclude : List CompactFilePattern
    , excludeFolders : List Glob
    , strings : List { string : String, included : Bool }
    , excludeFoldersStrings : List String
    }


type CompactFilePattern
    = CompactInclude (List Glob)
    | CompactExclude (List Glob)


compact : List FilePattern -> Result (List String) Summary
compact filePatterns =
    compactBase filePatterns
        { includeExclude = []
        , excludeFolders = []
        , strings = []
        , excludeFoldersStrings = []
        }


toStrings : Summary -> List { string : String, included : Bool }
toStrings summary =
    []


compactBase : List FilePattern -> Summary -> Result (List String) Summary
compactBase filePatterns accSummary =
    case filePatterns of
        [] ->
            Ok accSummary

        (Include ( raw, pattern )) :: rest ->
            compactHelp rest [ pattern ] True accSummary

        (Exclude ( raw, pattern )) :: rest ->
            compactHelp rest [ pattern ] False accSummary

        (ExcludeFolder ( raw, pattern )) :: rest ->
            compactBase rest
                { includeExclude = accSummary.includeExclude
                , excludeFolders = pattern :: accSummary.excludeFolders
                , strings = accSummary.strings
                , excludeFoldersStrings = accSummary.excludeFoldersStrings
                }

        (InvalidGlob pattern) :: rest ->
            Err (compactErrors rest [ pattern ])


compactHelp : List FilePattern -> List Glob -> Bool -> Summary -> Result (List String) Summary
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
                , excludeFolders = accSummary.excludeFolders
                , strings = accSummary.strings
                , excludeFoldersStrings = accSummary.excludeFoldersStrings
                }

        (Include ( raw, pattern )) :: rest ->
            if included then
                compactHelp rest (pattern :: accGlobs) included (addRawIncludeExclude raw included accSummary)

            else
                compactHelp rest
                    [ pattern ]
                    True
                    { includeExclude = CompactExclude accGlobs :: accSummary.includeExclude
                    , excludeFolders = accSummary.excludeFolders
                    , strings = { string = raw, included = included } :: accSummary.strings
                    , excludeFoldersStrings = accSummary.excludeFoldersStrings
                    }

        (Exclude ( raw, pattern )) :: rest ->
            if included then
                compactHelp rest
                    [ pattern ]
                    False
                    { includeExclude = CompactInclude accGlobs :: accSummary.includeExclude
                    , excludeFolders = accSummary.excludeFolders
                    , strings = { string = raw, included = included } :: accSummary.strings
                    , excludeFoldersStrings = accSummary.excludeFoldersStrings
                    }

            else
                compactHelp rest (pattern :: accGlobs) included (addRawIncludeExclude raw included accSummary)

        (ExcludeFolder ( raw, pattern )) :: rest ->
            compactHelp rest
                accGlobs
                included
                { includeExclude = accSummary.includeExclude
                , excludeFolders = pattern :: accSummary.excludeFolders
                , strings = accSummary.strings
                , excludeFoldersStrings = raw :: accSummary.excludeFoldersStrings
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


addRawIncludeExclude : String -> Bool -> Summary -> Summary
addRawIncludeExclude string included summary =
    { includeExclude = summary.includeExclude
    , excludeFolders = summary.excludeFolders
    , strings = { string = string, included = included } :: summary.strings
    , excludeFoldersStrings = summary.excludeFoldersStrings
    }


include : String -> FilePattern
include globStr =
    case Glob.fromString globStr of
        Ok glob ->
            Include ( globStr, glob )

        Err _ ->
            InvalidGlob globStr


exclude : String -> FilePattern
exclude globStr =
    case Glob.fromString globStr of
        Ok glob ->
            Exclude ( globStr, glob )

        Err _ ->
            InvalidGlob globStr


excludeFolder : String -> FilePattern
excludeFolder globStr =
    case Glob.fromString (toFolder globStr) of
        Ok glob ->
            ExcludeFolder ( globStr, glob )

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
