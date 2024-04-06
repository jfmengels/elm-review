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
        ]
