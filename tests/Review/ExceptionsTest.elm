module Review.ExceptionsTest exposing (all)

import Expect
import Review.Exceptions as Exceptions exposing (Exceptions)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Exceptions"
        [ test "should remove files using Exceptions.ignoreFiles" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be removed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm" -- should be removed
                , "src/File.elm"
                , "src-other/Ignored/File.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Foo/Bar.elm"
                        , "src/IgnoredFile.elm"
                        , "src/File.elm"
                        , "src-other/Ignored/File.elm"
                        ]
        , test "should remove files using Exceptions.ignoreDirectories" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreDirectories [ "src/Ignored/", "src/Foo/Thing" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be removed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm"
                , "src/Foo/Thing/SubThing.elm" -- should be removed
                , "src/File.elm"
                , "src-other/Ignored/File.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Foo/Bar.elm"
                        , "src/IgnoredFile.elm"
                        , "src/Foo/Thing.elm"
                        , "src/File.elm"
                        , "src-other/Ignored/File.elm"
                        ]
        , test "should remove files using Exceptions.ignoreFiles and Exceptions.ignoreDirectories" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
                            |> Exceptions.ignoreDirectories [ "src/Ignored/", "src-other" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be removed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm" -- should be removed
                , "src/Foo/Thing/SubThing.elm"
                , "src/File.elm"
                , "src-other/Ignored/File.elm" -- should be removed
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Foo/Bar.elm"
                        , "src/IgnoredFile.elm"
                        , "src/Foo/Thing/SubThing.elm"
                        , "src/File.elm"
                        ]
        , test "should keep files using Exceptions.allowFiles" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.allowFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be allowed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm" -- should be allowed
                , "src/File.elm"
                , "src-other/Ignored/File.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Ignored/File.elm"
                        , "src/Foo/Thing.elm"
                        ]
        , test "should keep files using Exceptions.allowDirectories" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.allowDirectories [ "src/Ignored/", "src/Foo/Thing" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be allowed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm"
                , "src/Foo/Thing/SubThing.elm" -- should be allowed
                , "src/File.elm"
                , "src-other/Ignored/File.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Ignored/File.elm"
                        , "src/Foo/Thing/SubThing.elm"
                        ]
        , test "should keep files using Exceptions.allowFiles and Exceptions.allowDirectories" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.allowFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
                            |> Exceptions.allowDirectories [ "src/Ignored/", "src-other" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be allowed
                , "src/Ignored/OtherFile.elm" -- should be allowed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm" -- should be allowed
                , "src/Foo/Thing/SubThing.elm"
                , "src/File.elm"
                , "src-other/Ignored/File.elm" -- should be allowed
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Ignored/File.elm"
                        , "src/Ignored/OtherFile.elm"
                        , "src/Foo/Thing.elm"
                        , "src-other/Ignored/File.elm"
                        ]
        , test "should keep files using several Exceptions.allowFiles and Exceptions.allowDirectories" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.allowFiles [ "src/Ignored/File.elm" ]
                            |> Exceptions.allowFiles [ "src/Foo/Thing.elm" ]
                            |> Exceptions.allowDirectories [ "src/Ignored/" ]
                            |> Exceptions.allowDirectories [ "src-other" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be allowed
                , "src/Ignored/OtherFile.elm" -- should be allowed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm" -- should be allowed
                , "src/Foo/Thing/SubThing.elm"
                , "src/File.elm"
                , "src-other/Ignored/File.elm" -- should be allowed
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Ignored/File.elm"
                        , "src/Ignored/OtherFile.elm"
                        , "src/Foo/Thing.elm"
                        , "src-other/Ignored/File.elm"
                        ]
        , test "should allow some files then ignore a subset of them" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.allowFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
                            |> Exceptions.allowDirectories [ "src/Ignored/", "src-other" ]
                            |> Exceptions.ignoreFiles [ "src/Ignored/OtherFile.elm" ]
                            |> Exceptions.ignoreDirectories [ "src-other" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be allowed
                , "src/Ignored/OtherFile.elm" -- allowed, but ignored
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm" -- should be allowed
                , "src/Foo/Thing/SubThing.elm"
                , "src/File.elm"
                , "src-other/Ignored/File.elm" -- allowed, but ignored
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Ignored/File.elm"
                        , "src/Foo/Thing.elm"
                        ]
        , test "ignoring then allowing should be the same as allowing then ignoring" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreFiles [ "src/Ignored/OtherFile.elm" ]
                            |> Exceptions.ignoreDirectories [ "src-other" ]
                            |> Exceptions.allowFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
                            |> Exceptions.allowDirectories [ "src/Ignored/", "src-other" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be allowed
                , "src/Ignored/OtherFile.elm" -- allowed, but ignored
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm" -- should be allowed
                , "src/Foo/Thing/SubThing.elm"
                , "src/File.elm"
                , "src-other/Ignored/File.elm" -- allowed, but ignored
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Ignored/File.elm"
                        , "src/Foo/Thing.elm"
                        ]
        , test "should be backslash-insensitive in the ignored paths" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreDirectories [ "src\\Ignored", "src\\Foo\\Thing" ]
                in
                [ "src/Foo/Bar.elm"
                , "src/Ignored/File.elm" -- should be removed
                , "src/IgnoredFile.elm"
                , "src/Foo/Thing.elm"
                , "src/Foo/Thing/SubThing.elm" -- should be removed
                , "src/File.elm"
                , "src-other/Ignored/File.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/Foo/Bar.elm"
                        , "src/IgnoredFile.elm"
                        , "src/Foo/Thing.elm"
                        , "src/File.elm"
                        , "src-other/Ignored/File.elm"
                        ]
        , test "should be backslash-insensitive at the end of the path the ignored paths" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreDirectories [ "src\\Ignored\\" ]
                in
                [ "src/Ignored/File.elm"
                , "src/IgnoredFile.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src/IgnoredFile.elm"
                        ]
        , test "should be backslash-insensitive in the files (ignoring files)" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
                in
                [ "src\\Foo\\Bar.elm"
                , "src\\Ignored\\File.elm" -- should be removed
                , "src\\IgnoredFile.elm"
                , "src\\Foo\\Thing.elm" -- should be removed
                , "src\\File.elm"
                , "src-other\\Ignored\\File.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src\\Foo\\Bar.elm"
                        , "src\\IgnoredFile.elm"
                        , "src\\File.elm"
                        , "src-other\\Ignored\\File.elm"
                        ]
        , test "should be backslash-insensitive in the files (ignoring directories)" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.ignoreDirectories [ "src/Ignored/", "src/Foo/Thing" ]
                in
                [ "src\\Foo\\Bar.elm"
                , "src\\Ignored\\File.elm" -- should be removed
                , "src\\IgnoredFile.elm"
                , "src\\Foo\\Thing.elm"
                , "src\\Foo\\Thing\\SubThing.elm" -- should be removed
                , "src\\File.elm"
                , "src-other\\Ignored\\File.elm"
                ]
                    |> Exceptions.apply exceptions identity
                    |> Expect.equal
                        [ "src\\Foo\\Bar.elm"
                        , "src\\IgnoredFile.elm"
                        , "src\\Foo\\Thing.elm"
                        , "src\\File.elm"
                        , "src-other\\Ignored\\File.elm"
                        ]
        ]
