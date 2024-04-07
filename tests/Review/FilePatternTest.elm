module Review.FilePatternTest exposing (all)

import Expect
import Review.FilePattern as FilePattern exposing (FilePattern)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "FilePattern.match"
        [ test "should return False when the list is empty" <|
            \() ->
                FilePattern.match [] "some/file/path.ext"
                    |> Expect.equal False
        , test "should return True when the list contains the target file" <|
            \() ->
                FilePattern.match
                    [ FilePattern.include "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return True when the list contains a Glob matching a parent" <|
            \() ->
                FilePattern.match
                    [ FilePattern.include "some/file/**/*"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal True
        , test "should return False when the list a Glob matching a parent but exclude the file itself" <|
            \() ->
                FilePattern.match
                    [ FilePattern.include "some/file/**"
                    , FilePattern.exclude "some/file/path.ext"
                    ]
                    "some/file/path.ext"
                    |> Expect.equal False
        ]
