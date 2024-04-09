module Review.FilePattern exposing
    ( FilePattern
    , include, exclude, excludeDirectory
    , compact, match, toStrings
    )

{-| REPLACEME

@docs FilePattern
@docs include, exclude, excludeDirectory
@docs compact, match, toStrings

-}

import Glob exposing (Glob)


{-| REPLACEME
-}
type FilePattern
    = Include String
    | Exclude String
    | ExcludeDirectory String


{-| REPLACEME
-}
type alias Summary =
    { includeExclude : List CompactFilePattern
    , excludedDirectories : List Glob
    , strings : List { pattern : String, included : Bool }
    , excludedDirectoriesStrings : List String
    }


{-| REPLACEME
-}
toStrings : Summary -> { files : List { pattern : String, included : Bool }, excludedDirectories : List String }
toStrings summary =
    { files = summary.strings
    , excludedDirectories = summary.excludedDirectoriesStrings
    }


type CompactFilePattern
    = CompactInclude (List Glob)
    | CompactExclude (List Glob)


{-| REPLACEME
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
                { includeExclude = summary.includeExclude
                , excludedDirectories = summary.excludedDirectories
                , strings = List.reverse summary.strings
                , excludedDirectoriesStrings = List.reverse summary.excludedDirectoriesStrings
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


addRawIncludeExclude : String -> Bool -> Summary -> Summary
addRawIncludeExclude string included summary =
    { includeExclude = summary.includeExclude
    , excludedDirectories = summary.excludedDirectories
    , strings = { pattern = string, included = included } :: summary.strings
    , excludedDirectoriesStrings = summary.excludedDirectoriesStrings
    }


{-| REPLACEME
-}
include : String -> FilePattern
include =
    Include


{-| REPLACEME
-}
exclude : String -> FilePattern
exclude =
    Exclude


{-| REPLACEME
-}
excludeDirectory : String -> FilePattern
excludeDirectory =
    ExcludeDirectory


toDirectory : String -> String
toDirectory globStr =
    if String.endsWith "/" globStr then
        globStr ++ "**/*"

    else
        globStr ++ "/**/*"


{-| REPLACEME
-}
match : Summary -> String -> Bool
match summary str =
    if List.any (\dirGlob -> Glob.match dirGlob str) summary.excludedDirectories then
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
