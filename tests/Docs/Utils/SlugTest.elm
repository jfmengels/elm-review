module Docs.Utils.SlugTest exposing (all)

import Docs.Utils.Slug as Slug
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Id slugs"
        [ test "should slugify single-word section" <|
            \() ->
                "Section"
                    |> Slug.toSlug
                    |> Expect.equal "section"
        , test "should slugify section with spaces" <|
            \() ->
                "Some Section"
                    |> Slug.toSlug
                    |> Expect.equal "some-section"
        , test "should slugify section with back-ticks" <|
            \() ->
                "`section`"
                    |> Slug.toSlug
                    |> Expect.equal "-section-"
        , test "should slugify section with question mark" <|
            \() ->
                "Section?"
                    |> Slug.toSlug
                    |> Expect.equal "section-"
        , test "should slugify complex example" <|
            \() ->
                "Section *with* ~some~ _spaces_ and\\_ $thi.ngs . [`links`](foo)"
                    |> Slug.toSlug
                    |> Expect.equal "section-_with_-some-_spaces_-and-_-thi-ngs-links-foo-"
        ]
