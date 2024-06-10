module Review.FilePatternTest exposing (all)

import Expect
import Fuzz
import Review.FilePattern as FilePattern exposing (FilePattern)
import Test exposing (Test, describe, fuzz, test)


all : Test
all =
    describe "Review.FilePattern"
        [ matchTest
        , toStringsTest
        ]


matchTest : Test
matchTest =
    describe "match"
        [ test "should return False when the list is empty and includeByDefault is False" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    []
                    "some/file/path.ext"
                    |> Expect.equal False
        , test "should return True when the list is empty and includeByDefault is True" <|
            \() ->
                matchAgainst
                    { includeByDefault = True }
                    []
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including the target file" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including through *" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.include "some/file/**/*"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including through * with an extension" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.include "some/file/**/*.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including through **/*" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.include "some/file/**/*"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return False when including through **/* but excluding the target file" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.include "some/file/**/*"
                    , FilePattern.exclude "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        , test "should return True when excluding through **/* but re-including the target file" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.exclude "some/file/**/*"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including then excluding then including again" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.include "some/file/**/*"
                    , FilePattern.exclude "some/file/path.*"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return False when including then excluding then including again" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.exclude "some/file/**/*"
                    , FilePattern.include "some/file/path.*"
                    , FilePattern.exclude "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        , test "should return False when excluding the folder even when re-including the target file" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.excludeDirectory "some"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        , test "should return False when excluding the folder (with trailing /) even when re-including the target file" <|
            \() ->
                matchAgainst
                    { includeByDefault = False }
                    [ FilePattern.excludeDirectory "some/"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        ]


matchAgainst : { includeByDefault : Bool } -> List FilePattern -> String -> Bool
matchAgainst includeByDefault filePatterns str =
    case FilePattern.compact filePatterns of
        Ok filePatternCompact ->
            FilePattern.match includeByDefault filePatternCompact str

        Err globs ->
            Debug.todo ("Invalid globs:\n" ++ String.join "\n" globs)


toStringsTest : Test
toStringsTest =
    describe "toStrings"
        [ fuzz
            (Fuzz.list (Fuzz.map2 (\str included -> { pattern = str, included = included }) Fuzz.string Fuzz.bool))
            "files should stay as before"
          <|
            \list ->
                case
                    list
                        |> List.map
                            (\{ pattern, included } ->
                                if included then
                                    FilePattern.include pattern

                                else
                                    FilePattern.exclude pattern
                            )
                        |> FilePattern.compact
                of
                    Ok summary ->
                        summary
                            |> FilePattern.toStrings
                            |> .files
                            |> Expect.equal list

                    Err _ ->
                        Expect.pass
        , fuzz
            (Fuzz.list Fuzz.string |> Fuzz.map List.sort)
            "excluded folders should stay as before"
          <|
            \list ->
                case
                    list
                        |> List.map FilePattern.excludeDirectory
                        |> FilePattern.compact
                of
                    Ok summary ->
                        summary
                            |> FilePattern.toStrings
                            |> .excludedDirectories
                            |> List.sort
                            |> Expect.equal list

                    Err _ ->
                        Expect.pass
        ]
