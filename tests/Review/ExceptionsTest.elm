module Review.ExceptionsTest exposing (all)

import Expect
import Review.Exceptions as Exceptions exposing (Exceptions)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Exceptions"
        [ test "should remove files using Exceptions.addFiles" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.addFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
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
        , test "should remove files using Exceptions.addDirectories" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.addDirectories [ "src/Ignored/", "src/Foo/Thing" ]
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
        , test "should be backslash-insensitive in the ignored paths" <|
            \() ->
                let
                    exceptions : Exceptions
                    exceptions =
                        Exceptions.init
                            |> Exceptions.addDirectories [ "src\\Ignored", "src\\Foo\\Thing" ]
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
                            |> Exceptions.addDirectories [ "src\\Ignored\\" ]
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
                            |> Exceptions.addFiles [ "src/Ignored/File.elm", "src/Foo/Thing.elm" ]
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
                            |> Exceptions.addDirectories [ "src/Ignored/", "src/Foo/Thing" ]
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
