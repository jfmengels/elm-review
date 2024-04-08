module Review.FilePatternTest exposing (all)

import Expect
import Fuzz
import Review.FilePattern as FilePattern exposing (FilePattern, excludeDirectory, include)
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
        [ test "should return False when the list is empty" <|
            \() ->
                matchAgainst [] "some/file/path.ext"
                    |> Expect.equal False
        , test "should return True when including the target file" <|
            \() ->
                matchAgainst
                    [ FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including through *" <|
            \() ->
                matchAgainst
                    [ FilePattern.include "some/file/**/*"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including through * with an extension" <|
            \() ->
                matchAgainst
                    [ FilePattern.include "some/file/**/*.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when including through **/*" <|
            \() ->
                matchAgainst
                    [ FilePattern.include "some/file/**/*"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return False when including through **/* but excluding the target file" <|
            \() ->
                matchAgainst
                    [ FilePattern.include "some/file/**/*"
                    , FilePattern.exclude "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        , test "should return True when excluding through **/* but re-including the target file" <|
            \() ->
                matchAgainst
                    [ FilePattern.exclude "some/file/**/*"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return False when excluding the folder even when re-including the target file" <|
            \() ->
                matchAgainst
                    [ FilePattern.excludeDirectory "some"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        , test "should return False when excluding the folder (with trailing /) even when re-including the target file" <|
            \() ->
                matchAgainst
                    [ FilePattern.excludeDirectory "some/"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        ]


matchAgainst : List FilePattern -> String -> Bool
matchAgainst filePatterns str =
    case FilePattern.compact filePatterns of
        Ok filePatternCompact ->
            FilePattern.match filePatternCompact str

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
            (Fuzz.list Fuzz.string)
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
                            |> Expect.equal list

                    Err _ ->
                        Expect.pass
        ]
