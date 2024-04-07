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
    , excludedFolders : List Glob
    , strings : List { string : String, included : Bool }
    , excludedFoldersStrings : List String
    }


toStrings : Summary -> { files : List { string : String, included : Bool }, excludedFolders : List String }
toStrings summary =
    { files = summary.strings
    , excludedFolders = summary.excludedFoldersStrings
    }


type CompactFilePattern
    = CompactInclude (List Glob)
    | CompactExclude (List Glob)


compact : List FilePattern -> Result (List String) Summary
compact filePatterns =
    compactBase filePatterns
        { includeExclude = []
        , excludedFolders = []
        , strings = []
        , excludedFoldersStrings = []
        }
        |> Result.map
            (\summary ->
                { includeExclude = summary.includeExclude
                , excludedFolders = summary.excludedFolders
                , strings = List.reverse summary.strings
                , excludedFoldersStrings = List.reverse summary.excludedFoldersStrings
                }
            )


compactBase : List FilePattern -> Summary -> Result (List String) Summary
compactBase filePatterns accSummary =
    case filePatterns of
        [] ->
            Ok accSummary

        (Include ( raw, pattern )) :: rest ->
            case Glob.fromString raw of
                Ok glob ->
                    compactHelp rest [ pattern ] True (addRawIncludeExclude raw True accSummary)

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (Exclude ( raw, pattern )) :: rest ->
            case Glob.fromString raw of
                Ok glob ->
                    compactHelp rest [ pattern ] False (addRawIncludeExclude raw False accSummary)

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (ExcludeFolder ( raw, pattern )) :: rest ->
            case Glob.fromString (toFolder raw) of
                Ok glob ->
                    compactBase rest
                        { includeExclude = accSummary.includeExclude
                        , excludedFolders = pattern :: accSummary.excludedFolders
                        , strings = accSummary.strings
                        , excludedFoldersStrings = raw :: accSummary.excludedFoldersStrings
                        }

                Err _ ->
                    Err (compactErrors rest [ raw ])

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
                , excludedFolders = accSummary.excludedFolders
                , strings = accSummary.strings
                , excludedFoldersStrings = accSummary.excludedFoldersStrings
                }

        (Include ( raw, pattern )) :: rest ->
            case Glob.fromString raw of
                Ok glob ->
                    if included then
                        compactHelp rest (pattern :: accGlobs) included (addRawIncludeExclude raw included accSummary)

                    else
                        compactHelp rest
                            [ pattern ]
                            True
                            { includeExclude = CompactExclude accGlobs :: accSummary.includeExclude
                            , excludedFolders = accSummary.excludedFolders
                            , strings = { string = raw, included = True } :: accSummary.strings
                            , excludedFoldersStrings = accSummary.excludedFoldersStrings
                            }

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (Exclude ( raw, pattern )) :: rest ->
            case Glob.fromString raw of
                Ok glob ->
                    if included then
                        compactHelp rest
                            [ pattern ]
                            False
                            { includeExclude = CompactInclude accGlobs :: accSummary.includeExclude
                            , excludedFolders = accSummary.excludedFolders
                            , strings = { string = raw, included = False } :: accSummary.strings
                            , excludedFoldersStrings = accSummary.excludedFoldersStrings
                            }

                    else
                        compactHelp rest (pattern :: accGlobs) included (addRawIncludeExclude raw included accSummary)

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (ExcludeFolder ( raw, pattern )) :: rest ->
            case Glob.fromString (toFolder raw) of
                Ok glob ->
                    compactHelp rest
                        accGlobs
                        included
                        { includeExclude = accSummary.includeExclude
                        , excludedFolders = pattern :: accSummary.excludedFolders
                        , strings = accSummary.strings
                        , excludedFoldersStrings = raw :: accSummary.excludedFoldersStrings
                        }

                Err _ ->
                    Err (compactErrors rest [ raw ])

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
    , excludedFolders = summary.excludedFolders
    , strings = { string = string, included = included } :: summary.strings
    , excludedFoldersStrings = summary.excludedFoldersStrings
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
    if List.any (\folderGlob -> Glob.match folderGlob str) summary.excludedFolders then
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
