module Review.FilePattern exposing (FilePattern, compact, exclude, excludeFolder, include, match, toStrings)

import Glob exposing (Glob)


type FilePattern
    = Include String
    | Exclude String
    | ExcludeFolder String


type alias Summary =
    { includeExclude : List CompactFilePattern
    , excludedFolders : List Glob
    , strings : List { pattern : String, included : Bool }
    , excludedFoldersStrings : List String
    }


toStrings : Summary -> { files : List { pattern : String, included : Bool }, excludedFolders : List String }
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

        (ExcludeFolder raw) :: rest ->
            case Glob.fromString (toFolder raw) of
                Ok pattern ->
                    compactBase rest
                        { includeExclude = accSummary.includeExclude
                        , excludedFolders = pattern :: accSummary.excludedFolders
                        , strings = accSummary.strings
                        , excludedFoldersStrings = raw :: accSummary.excludedFoldersStrings
                        }

                Err _ ->
                    Err (compactErrors rest [ raw ])


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
                            , excludedFolders = accSummary.excludedFolders
                            , strings = { pattern = raw, included = True } :: accSummary.strings
                            , excludedFoldersStrings = accSummary.excludedFoldersStrings
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
                            , excludedFolders = accSummary.excludedFolders
                            , strings = { pattern = raw, included = False } :: accSummary.strings
                            , excludedFoldersStrings = accSummary.excludedFoldersStrings
                            }

                    else
                        compactHelp rest (pattern :: accGlobs) included (addRawIncludeExclude raw included accSummary)

                Err _ ->
                    Err (compactErrors rest [ raw ])

        (ExcludeFolder raw) :: rest ->
            case Glob.fromString (toFolder raw) of
                Ok pattern ->
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

                        ExcludeFolder s ->
                            s
            in
            case Glob.fromString raw of
                Ok _ ->
                    compactErrors rest accGlobStrings

                Err _ ->
                    compactErrors rest (raw :: accGlobStrings)


addRawIncludeExclude : String -> Bool -> Summary -> Summary
addRawIncludeExclude string included summary =
    { includeExclude = summary.includeExclude
    , excludedFolders = summary.excludedFolders
    , strings = { pattern = string, included = included } :: summary.strings
    , excludedFoldersStrings = summary.excludedFoldersStrings
    }


include : String -> FilePattern
include =
    Include


exclude : String -> FilePattern
exclude =
    Exclude


excludeFolder : String -> FilePattern
excludeFolder =
    ExcludeFolder


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
