module Review.FilePatternTest exposing (all)

import Expect
import Fuzz
import Review.FilePattern as FilePattern exposing (FilePattern, include)
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
                    [ FilePattern.excludeFolder "some"
                    , FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        , test "should return False when excluding the folder (with trailing /) even when re-including the target file" <|
            \() ->
                matchAgainst
                    [ FilePattern.excludeFolder "some/"
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
    fuzz (Fuzz.list (Fuzz.map2 Tuple.pair Fuzz.string Fuzz.bool)) "toStrings" <|
        \list ->
            case
                list
                    |> List.map
                        (\( str, included ) ->
                            if included then
                                FilePattern.include str

                            else
                                FilePattern.exclude str
                        )
                    |> FilePattern.compact
            of
                Ok summary ->
                    summary
                        |> FilePattern.toStrings
                        |> Expect.equal []

                Err _ ->
                    Expect.pass
