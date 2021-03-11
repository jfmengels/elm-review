module Review.Test.Dependencies.ElmUrl exposing (dependency)

import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode as Decode
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create
        "elm/url"
        (createElmJsonProject elmJson)
        dependencyModules


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

        Err error ->
            Debug.todo ("Failed to decode elm.json for elm/url: " ++ Debug.toString error)


elmJson : String
elmJson =
    """{
    "type": "package",
    "name": "elm/url",
    "summary": "Create and parse URLs. Use for HTTP and "routing" in single-page apps (SPAs)",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Url",
        "Url.Builder",
        "Url.Parser",
        "Url.Parser.Query"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}
"""


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Url"
      , comment = """

# URLs
@docs Url, Protocol, toString, fromString

# Percent-Encoding
@docs percentEncode, percentDecode

"""
      , unions =
            [ { name = "Protocol"
              , comment = """ Is the URL served over a secure connection or not?
"""
              , args = []
              , tags =
                    [ ( "Http", [] )
                    , ( "Https", [] )
                    ]
              }
            ]
      , aliases =
            [ { name = "Url"
              , comment = """ In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \\___/   \\______________/\\_________/ \\_________/ \\__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

When you are creating a single-page app with [`Browser.fullscreen`][fs], you
use the [`Url.Parser`](Url-Parser) module to turn a `Url` into even nicer data.

If you want to create your own URLs, check out the [`Url.Builder`](Url-Builder)
module as well!

[fs]: /packages/elm/browser/latest/Browser#fullscreen

**Note:** This is a subset of all the full possibilities listed in the URI
spec. Specifically, it does not accept the `userinfo` segment you see in email
addresses like `tom@example.com`.
"""
              , args = []
              , tipe = decodeType "{ protocol : Url.Protocol, host : String.String, port_ : Maybe.Maybe Basics.Int, path : String.String, query : Maybe.Maybe String.String, fragment : Maybe.Maybe String.String }"
              }
            ]
      , values =
            [ { name = "fromString"
              , comment = """ Attempt to break a URL up into [`Url`](#Url). This is useful in
single-page apps when you want to parse certain chunks of a URL to figure out
what to show on screen.

    fromString "https://example.com:443"
    -- Just
    --   { protocol = Https
    --   , host = "example.com"
    --   , port_ = Just 443
    --   , path = "/"
    --   , query = Nothing
    --   , fragment = Nothing
    --   }

    fromString "https://example.com/hats?q=top%20hat"
    -- Just
    --   { protocol = Https
    --   , host = "example.com"
    --   , port_ = Nothing
    --   , path = "/hats"
    --   , query = Just "q=top%20hat"
    --   , fragment = Nothing
    --   }

    fromString "http://example.com/core/List/#map"
    -- Just
    --   { protocol = Http
    --   , host = "example.com"
    --   , port_ = Nothing
    --   , path = "/core/List/"
    --   , query = Nothing
    --   , fragment = Just "map"
    --   }

The conversion to segments can fail in some cases as well:

    fromString "example.com:443"        == Nothing  -- no protocol
    fromString "http://tom@example.com" == Nothing  -- userinfo disallowed
    fromString "http://#cats"           == Nothing  -- no host

**Note:** This function does not use [`percentDecode`](#percentDecode) anything.
It just splits things up. [`Url.Parser`](Url-Parser) actually _needs_ the raw
`query` string to parse it properly. Otherwise it could get confused about `=`
and `&` characters!
"""
              , tipe = decodeType "String.String -> Maybe.Maybe Url.Url"
              }
            , { name = "percentDecode"
              , comment = """ **Use [Url.Parser](Url-Parser) instead!** It will decode query
parameters appropriately already! `percentDecode` is only available so that
extremely custom cases are possible, if needed.

Check out the `percentEncode` function to learn about percent-encoding.
This function does the opposite! Here are the reverse examples:

    -- ASCII
    percentDecode "99%25"     == Just "hat"
    percentDecode "to%20be"   == Just "to be"
    percentDecode "hat"       == Just "99%"

    -- UTF-8
    percentDecode "%24"       == Just "$"
    percentDecode "%C2%A2"    == Just "¢"
    percentDecode "%E2%82%AC" == Just "€"

Why is it a `Maybe` though? Well, these strings come from strangers on the
internet as a bunch of bits and may have encoding problems. For example:

    percentDecode "%"   == Nothing  -- not followed by two hex digits
    percentDecode "%XY" == Nothing  -- not followed by two HEX digits
    percentDecode "%C2" == Nothing  -- half of the "¢" encoding "%C2%A2"

This is the same behavior as JavaScript's [`decodeURIComponent`][js] function.

[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent
"""
              , tipe = decodeType "String.String -> Maybe.Maybe String.String"
              }
            , { name = "percentEncode"
              , comment = """ **Use [Url.Builder](Url-Builder) instead!** Functions like `absolute`,
`relative`, and `crossOrigin` already do this automatically! `percentEncode`
is only available so that extremely custom cases are possible, if needed.

Percent-encoding is how [the official URI spec][uri] “escapes” special
characters. You can still represent a `?` even though it is reserved for
queries.

This function exists in case you want to do something extra custom. Here are
some examples:

    -- standard ASCII encoding
    percentEncode "hat"   == "hat"
    percentEncode "to be" == "to%20be"
    percentEncode "99%"   == "99%25"

    -- non-standard, but widely accepted, UTF-8 encoding
    percentEncode "$" == "%24"
    percentEncode "¢" == "%C2%A2"
    percentEncode "€" == "%E2%82%AC"

This is the same behavior as JavaScript's [`encodeURIComponent`][js] function,
and the rules are described in more detail officially [here][s2] and with some
notes about Unicode [here][wiki].

[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
[uri]: https://tools.ietf.org/html/rfc3986
[s2]: https://tools.ietf.org/html/rfc3986#section-2.1
[wiki]: https://en.wikipedia.org/wiki/Percent-encoding
"""
              , tipe = decodeType "String.String -> String.String"
              }
            , { name = "toString"
              , comment = """ Turn a [`Url`](#Url) into a `String`.
"""
              , tipe = decodeType "Url.Url -> String.String"
              }
            ]
      , binops = []
      }
    , { name = "Url.Builder"
      , comment = """ In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \\___/   \\______________/\\_________/ \\_________/ \\__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

This module helps you create these!


# Builders
@docs absolute, relative, crossOrigin, custom, Root

# Queries
@docs QueryParameter, string, int, toQuery

"""
      , unions =
            [ { name = "QueryParameter"
              , comment = """ Represents query parameter. Builder functions like `absolute` percent-encode
all the query parameters they get, so you do not need to worry about it!
"""
              , args = []
              , tags = []
              }
            , { name = "Root"
              , comment = """ Specify whether a [`custom`](#custom) URL is absolute, relative, or
cross-origin.
"""
              , args = []
              , tags =
                    [ ( "Absolute", [] )
                    , ( "Relative", [] )
                    , ( "CrossOrigin", [ decodeType "String.String" ] )
                    ]
              }
            ]
      , aliases = []
      , values =
            [ { name = "absolute"
              , comment = """ Create an absolute URL:

    absolute [] []
    -- "/"

    absolute [ "packages", "elm", "core" ] []
    -- "/packages/elm/core"

    absolute [ "blog", String.fromInt 42 ] []
    -- "/blog/42"

    absolute [ "products" ] [ string "search" "hat", int "page" 2 ]
    -- "/products?search=hat&page=2"

Notice that the URLs start with a slash!
"""
              , tipe = decodeType "List.List String.String -> List.List Url.Builder.QueryParameter -> String.String"
              }
            , { name = "crossOrigin"
              , comment = """ Create a cross-origin URL.

    crossOrigin "https://example.com" [ "products" ] []
    -- "https://example.com/products"

    crossOrigin "https://example.com" [] []
    -- "https://example.com/"

    crossOrigin
      "https://example.com:8042"
      [ "over", "there" ]
      [ string "name" "ferret" ]
    -- "https://example.com:8042/over/there?name=ferret"

**Note:** Cross-origin requests are slightly restricted for security.
For example, the [same-origin policy][sop] applies when sending HTTP requests,
so the appropriate `Access-Control-Allow-Origin` header must be enabled on the
*server* to get things working. Read more about the security rules [here][cors].

[sop]: https://developer.mozilla.org/en-US/docs/Web/Security/Same-origin_policy
[cors]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
"""
              , tipe = decodeType "String.String -> List.List String.String -> List.List Url.Builder.QueryParameter -> String.String"
              }
            , { name = "custom"
              , comment = """ Create custom URLs that may have a hash on the end:

    custom Absolute
      [ "packages", "elm", "core", "latest", "String" ]
      []
      (Just "length")
    -- "/packages/elm/core/latest/String#length"

    custom Relative [ "there" ] [ string "name" "ferret" ] Nothing
    -- "there?name=ferret"

    custom
      (CrossOrigin "https://example.com:8042")
      [ "over", "there" ]
      [ string "name" "ferret" ]
      (Just "nose")
    -- "https://example.com:8042/over/there?name=ferret#nose"
"""
              , tipe = decodeType "Url.Builder.Root -> List.List String.String -> List.List Url.Builder.QueryParameter -> Maybe.Maybe String.String -> String.String"
              }
            , { name = "int"
              , comment = """ Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat", int "page" 2 ]
    -- "/products?search=hat&page=2"

Writing `int key n` is the same as writing `string key (String.fromInt n)`.
So this is just a convenience function, making your code a bit shorter!
"""
              , tipe = decodeType "String.String -> Basics.Int -> Url.Builder.QueryParameter"
              }
            , { name = "relative"
              , comment = """ Create a relative URL:

    relative [] []
    -- ""

    relative [ "elm", "core" ] []
    -- "elm/core"

    relative [ "blog", String.fromInt 42 ] []
    -- "blog/42"

    relative [ "products" ] [ string "search" "hat", int "page" 2 ]
    -- "products?search=hat&page=2"

Notice that the URLs **do not** start with a slash!
"""
              , tipe = decodeType "List.List String.String -> List.List Url.Builder.QueryParameter -> String.String"
              }
            , { name = "string"
              , comment = """ Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat" ]
    -- "/products?search=hat"

    absolute ["products"] [ string "search" "coffee table" ]
    -- "/products?search=coffee%20table"
"""
              , tipe = decodeType "String.String -> String.String -> Url.Builder.QueryParameter"
              }
            , { name = "toQuery"
              , comment = """ Convert a list of query parameters to a percent-encoded query. This
function is used by `absolute`, `relative`, etc.

    toQuery [ string "search" "hat" ]
    -- "?search=hat"

    toQuery [ string "search" "coffee table" ]
    -- "?search=coffee%20table"

    toQuery [ string "search" "hat", int "page" 2 ]
    -- "?search=hat&page=2"

    toQuery []
    -- ""
"""
              , tipe = decodeType "List.List Url.Builder.QueryParameter -> String.String"
              }
            ]
      , binops = []
      }
    , { name = "Url.Parser"
      , comment = """ In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \\___/   \\______________/\\_________/ \\_________/ \\__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

This module is primarily for parsing the `path` part.


# Primitives
@docs Parser, string, int, s

# Path
@docs (</>), map, oneOf, top, custom

# Query
@docs (<?>), query

# Fragment
@docs fragment

# Run Parsers
@docs parse

"""
      , unions =
            [ { name = "Parser"
              , comment = """ Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data.
"""
              , args = [ "a", "b" ]
              , tags = []
              }
            ]
      , aliases = []
      , values =
            [ { name = "custom"
              , comment = """ Create a custom path segment parser. Here is how it is used to define the
`int` parser:

    int : Parser (Int -> a) a
    int =
      custom "NUMBER" String.toInt

You can use it to define something like “only CSS files” like this:

    css : Parser (String -> a) a
    css =
      custom "CSS_FILE" <| \\segment ->
        if String.endsWith ".css" segment then
          Just segment
        else
          Nothing
"""
              , tipe = decodeType "String.String -> (String.String -> Maybe.Maybe a) -> Url.Parser.Parser (a -> b) b"
              }
            , { name = "fragment"
              , comment = """ Create a parser for the URL fragment, the stuff after the `#`. This can
be handy for handling links to DOM elements within a page. Pages like this one!

    type alias Docs =
      (String, Maybe String)

    docs : Parser (Docs -> a) a
    docs =
      map Tuple.pair (string </> fragment identity)

    -- /List/map   ==>  Nothing
    -- /List/#map  ==>  Just ("List", Just "map")
    -- /List#map   ==>  Just ("List", Just "map")
    -- /List#      ==>  Just ("List", Just "")
    -- /List       ==>  Just ("List", Nothing)
    -- /           ==>  Nothing

"""
              , tipe = decodeType "(Maybe.Maybe String.String -> fragment) -> Url.Parser.Parser (fragment -> a) a"
              }
            , { name = "int"
              , comment = """ Parse a segment of the path as an `Int`.

    -- /alice/  ==>  Nothing
    -- /bob     ==>  Nothing
    -- /42/     ==>  Just 42
    -- /        ==>  Nothing
"""
              , tipe = decodeType "Url.Parser.Parser (Basics.Int -> a) a"
              }
            , { name = "map"
              , comment = """ Transform a path parser.

    type alias Comment = { user : String, id : Int }

    userAndId : Parser (String -> Int -> a) a
    userAndId =
      s "user" </> string </> s "comment" </> int

    comment : Parser (Comment -> a) a
    comment =
      map Comment userAndId

    -- /user/bob/comment/42  ==>  Just { user = "bob", id = 42 }
    -- /user/tom/comment/35  ==>  Just { user = "tom", id = 35 }
    -- /user/sam/             ==>  Nothing
"""
              , tipe = decodeType "a -> Url.Parser.Parser a b -> Url.Parser.Parser (b -> c) c"
              }
            , { name = "oneOf"
              , comment = """ Try a bunch of different path parsers.

    type Route
      = Topic String
      | Blog Int
      | User String
      | Comment String Int

    route : Parser (Route -> a) a
    route =
      oneOf
        [ map Topic   (s "topic" </> string)
        , map Blog    (s "blog" </> int)
        , map User    (s "user" </> string)
        , map Comment (s "user" </> string </> s "comment" </> int)
        ]

    -- /topic/wolf           ==>  Just (Topic "wolf")
    -- /topic/               ==>  Nothing

    -- /blog/42               ==>  Just (Blog 42)
    -- /blog/wolf             ==>  Nothing

    -- /user/sam/             ==>  Just (User "sam")
    -- /user/bob/comment/42  ==>  Just (Comment "bob" 42)
    -- /user/tom/comment/35  ==>  Just (Comment "tom" 35)
    -- /user/                 ==>  Nothing

If there are multiple parsers that could succeed, the first one wins.
"""
              , tipe = decodeType "List.List (Url.Parser.Parser a b) -> Url.Parser.Parser a b"
              }
            , { name = "parse"
              , comment = """ Actually run a parser! You provide some [`Url`](Url#Url) that
represent a valid URL. From there `parse` runs your parser on the path, query
parameters, and fragment.

    import Url
    import Url.Parser exposing (Parser, parse, int, map, oneOf, s, top)

    type Route = Home | Blog Int | NotFound

    route : Parser (Route -> a) a
    route =
      oneOf
        [ map Home top
        , map Blog (s "blog" </> int)
        ]

    toRoute : String -> Route
    toRoute string =
      case Url.fromString string of
        Nothing ->
          NotFound

        Just url ->
          Maybe.withDefault NotFound (parse route url)

    -- toRoute "/blog/42"                            ==  NotFound
    -- toRoute "https://example.com/"                ==  Home
    -- toRoute "https://example.com/blog"            ==  NotFound
    -- toRoute "https://example.com/blog/42"         ==  Blog 42
    -- toRoute "https://example.com/blog/42/"        ==  Blog 42
    -- toRoute "https://example.com/blog/42#wolf"    ==  Blog 42
    -- toRoute "https://example.com/blog/42?q=wolf"  ==  Blog 42
    -- toRoute "https://example.com/settings"        ==  NotFound

Functions like `toRoute` are useful when creating single-page apps with
[`Browser.fullscreen`][fs]. I use them in `init` and `onNavigation` to handle
the initial URL and any changes.

[fs]: /packages/elm/browser/latest/Browser#fullscreen
"""
              , tipe = decodeType "Url.Parser.Parser (a -> a) a -> Url.Url -> Maybe.Maybe a"
              }
            , { name = "query"
              , comment = """ The [`Url.Parser.Query`](Url-Parser-Query) module defines its own
[`Parser`](Url-Parser-Query#Parser) type. This function is a helper to convert
those into normal parsers.

    import Url.Parser.Query as Query

    -- the following expressions are both the same!
    --
    -- s "blog" <?> Query.string "search"
    -- s "blog" </> query (Query.string "search")

This may be handy if you need query parameters but are not parsing any path
segments.
"""
              , tipe = decodeType "Url.Parser.Query.Parser query -> Url.Parser.Parser (query -> a) a"
              }
            , { name = "s"
              , comment = """ Parse a segment of the path if it matches a given string. It is almost
always used with [`</>`](#</>) or [`oneOf`](#oneOf). For example:

    blog : Parser (Int -> a) a
    blog =
      s "blog" </> int

    -- /blog/42  ==>  Just 42
    -- /tree/42  ==>  Nothing

The path segment must be an exact match!
"""
              , tipe = decodeType "String.String -> Url.Parser.Parser a a"
              }
            , { name = "string"
              , comment = """ Parse a segment of the path as a `String`.

    -- /alice/  ==>  Just "alice"
    -- /bob     ==>  Just "bob"
    -- /42/     ==>  Just "42"
    -- /        ==>  Nothing
"""
              , tipe = decodeType "Url.Parser.Parser (String.String -> a) a"
              }
            , { name = "top"
              , comment = """ A parser that does not consume any path segments.

    type Route = Overview | Post Int

    blog : Parser (BlogRoute -> a) a
    blog =
      s "blog" </>
        oneOf
          [ map Overview top
          , map Post (s "post" </> int)
          ]

    -- /blog/         ==>  Just Overview
    -- /blog/post/42  ==>  Just (Post 42)
"""
              , tipe = decodeType "Url.Parser.Parser a a"
              }
            ]
      , binops =
            [ { name = "</>"
              , comment = """ Parse a path with multiple segments.

    blog : Parser (Int -> a) a
    blog =
      s "blog" </> int

    -- /blog/35/  ==>  Just 35
    -- /blog/42   ==>  Just 42
    -- /blog/     ==>  Nothing
    -- /42/       ==>  Nothing

    search : Parser (String -> a) a
    search =
      s "search" </> string

    -- /search/wolf/  ==>  Just "wolf"
    -- /search/frog   ==>  Just "frog"
    -- /search/       ==>  Nothing
    -- /wolf/         ==>  Nothing
"""
              , tipe = decodeType "Url.Parser.Parser a b -> Url.Parser.Parser b c -> Url.Parser.Parser a c"
              , associativity = Elm.Docs.Right
              , precedence = 7
              }
            , { name = "<?>"
              , comment = """ The [`Url.Parser.Query`](Url-Parser-Query) module defines its own
[`Parser`](Url-Parser-Query#Parser) type. This function helps you use those
with normal parsers. For example, maybe you want to add a search feature to
your blog website:

    import Url.Parser.Query as Query

    type Route
      = Overview (Maybe String)
      | Post Int

    blog : Parser (Route -> a) a
    blog =
      oneOf
        [ map Overview (s "blog" <?> Query.string "q")
        , map Post (s "blog" </> int)
        ]

    -- /blog/           ==>  Just (Overview Nothing)
    -- /blog/?q=wolf    ==>  Just (Overview (Just "wolf"))
    -- /blog/wolf       ==>  Nothing
    -- /blog/42         ==>  Just (Post 42)
    -- /blog/42?q=wolf  ==>  Just (Post 42)
    -- /blog/42/wolf    ==>  Nothing
"""
              , tipe = decodeType "Url.Parser.Parser a (query -> b) -> Url.Parser.Query.Parser query -> Url.Parser.Parser a b"
              , associativity = Elm.Docs.Left
              , precedence = 8
              }
            ]
      }
    , { name = "Url.Parser.Query"
      , comment = """ In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \\___/   \\______________/\\_________/ \\_________/ \\__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

This module is for parsing the `query` part.

In this library, a valid query looks like `?search=hats&page=2` where each
query parameter has the format `key=value` and is separated from the next
parameter by the `&` character.

# Parse Query Parameters
@docs Parser, string, int, enum, custom

# Mapping
@docs map, map2, map3, map4, map5, map6, map7, map8

"""
      , unions = []
      , aliases =
            [ { name = "Parser"
              , comment = """ Parse a query like `?search=hat&page=2` into nice Elm data.
"""
              , args = [ "a" ]
              , tipe = decodeType "Url.Parser.Internal.QueryParser a"
              }
            ]
      , values =
            [ { name = "custom"
              , comment = """ Create a custom query parser. The [`string`](#string), [`int`](#int), and
[`enum`](#enum) parsers are defined using this function. It can help you handle
anything though!

Say you are unlucky enough to need to handle `?post=2&post=7` to show a couple
posts on screen at once. You could say:

    posts : Parser (Maybe (List Int))
    posts =
      custom "post" (List.maybeMap String.toInt)

    -- ?post=2        == [2]
    -- ?post=2&post=7 == [2, 7]
    -- ?post=2&post=x == [2]
    -- ?hats=2        == []
"""
              , tipe = decodeType "String.String -> (List.List String.String -> a) -> Url.Parser.Query.Parser a"
              }
            , { name = "enum"
              , comment = """ Handle enumerated parameters. Maybe you want a true-or-false parameter:

    import Dict

    debug : Parser (Maybe Bool)
    debug =
      enum "debug" (Dict.fromList [ ("true", True), ("false", False) ])

    -- ?debug=true            == Just True
    -- ?debug=false           == Just False
    -- ?debug=1               == Nothing
    -- ?debug=0               == Nothing
    -- ?true=true             == Nothing
    -- ?debug=true&debug=true == Nothing

You could add `0` and `1` to the dictionary if you want to handle those as
well. You can also use [`map`](#map) to say `map (Result.withDefault False) debug`
to get a parser of type `Parser Bool` that swallows any errors and defaults to
`False`.

**Note:** Parameters like `?debug` with no `=` are not supported by this library.
"""
              , tipe = decodeType "String.String -> Dict.Dict String.String a -> Url.Parser.Query.Parser (Maybe.Maybe a)"
              }
            , { name = "int"
              , comment = """ Handle `Int` parameters. Maybe you want to show paginated search results:

    page : Parser (Maybe Int)
    page =
      int "page"

    -- ?page=2        == Just 2
    -- ?page=17       == Just 17
    -- ?page=two      == Nothing
    -- ?sort=date     == Nothing
    -- ?page=2&page=3 == Nothing

Check out [`custom`](#custom) if you need to handle multiple `page` parameters
or something like that.
"""
              , tipe = decodeType "String.String -> Url.Parser.Query.Parser (Maybe.Maybe Basics.Int)"
              }
            , { name = "map"
              , comment = """ Transform a parser in some way. Maybe you want your `page` query parser to
default to `1` if there is any problem?

    page : Parser Int
    page =
      map (Result.withDefault 1) (int "page")

"""
              , tipe = decodeType "(a -> b) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b"
              }
            , { name = "map2"
              , comment = """ Combine two parsers. A query like `?search=hats&page=2` could be parsed
with something like this:

    type alias Query =
      { search : Maybe String
      , page : Maybe Int
      }

    query : Parser Query
    query =
      map2 Query (string "search") (int "page")

"""
              , tipe = decodeType "(a -> b -> result) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b -> Url.Parser.Query.Parser result"
              }
            , { name = "map3"
              , comment = """ Combine three parsers. A query like `?search=hats&page=2&sort=ascending`
could be parsed with something like this:

    import Dict

    type alias Query =
      { search : Maybe String
      , page : Maybe Int
      , sort : Maybe Order
      }

    type Order = Ascending | Descending

    query : Parser Query
    query =
      map3 Query (string "search") (int "page") (enum "sort" order)

    order : Dict.Dict String Order
    order =
      Dict.fromList
        [ ( "ascending", Ascending )
        , ( "descending", Descending )
        ]
"""
              , tipe = decodeType "(a -> b -> c -> result) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b -> Url.Parser.Query.Parser c -> Url.Parser.Query.Parser result"
              }
            , { name = "map4"
              , comment = """"""
              , tipe = decodeType "(a -> b -> c -> d -> result) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b -> Url.Parser.Query.Parser c -> Url.Parser.Query.Parser d -> Url.Parser.Query.Parser result"
              }
            , { name = "map5"
              , comment = """"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> result) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b -> Url.Parser.Query.Parser c -> Url.Parser.Query.Parser d -> Url.Parser.Query.Parser e -> Url.Parser.Query.Parser result"
              }
            , { name = "map6"
              , comment = """"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> f -> result) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b -> Url.Parser.Query.Parser c -> Url.Parser.Query.Parser d -> Url.Parser.Query.Parser e -> Url.Parser.Query.Parser f -> Url.Parser.Query.Parser result"
              }
            , { name = "map7"
              , comment = """"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> f -> g -> result) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b -> Url.Parser.Query.Parser c -> Url.Parser.Query.Parser d -> Url.Parser.Query.Parser e -> Url.Parser.Query.Parser f -> Url.Parser.Query.Parser g -> Url.Parser.Query.Parser result"
              }
            , { name = "map8"
              , comment = """ If you need higher than eight, you can define a function like this:

    apply : Parser a -> Parser (a -> b) -> Parser b
    apply argParser funcParser =
      map2 (<|) funcParser argParser

And then you can chain it to do as many of these as you would like:

    map func (string "search")
      |> apply (int "page")
      |> apply (int "per-page")

"""
              , tipe = decodeType "(a -> b -> c -> d -> e -> f -> g -> h -> result) -> Url.Parser.Query.Parser a -> Url.Parser.Query.Parser b -> Url.Parser.Query.Parser c -> Url.Parser.Query.Parser d -> Url.Parser.Query.Parser e -> Url.Parser.Query.Parser f -> Url.Parser.Query.Parser g -> Url.Parser.Query.Parser h -> Url.Parser.Query.Parser result"
              }
            , { name = "string"
              , comment = """ Handle `String` parameters.

    search : Parser (Maybe String)
    search =
      string "search"

    -- ?search=cats             == Just "cats"
    -- ?search=42               == Just "42"
    -- ?branch=left             == Nothing
    -- ?search=cats&search=dogs == Nothing

Check out [`custom`](#custom) if you need to handle multiple `search`
parameters for some reason.
"""
              , tipe = decodeType "String.String -> Url.Parser.Query.Parser (Maybe.Maybe String.String)"
              }
            ]
      , binops = []
      }
    ]


decodeType : String -> Elm.Type.Type
decodeType type_ =
    case Decode.decodeString Elm.Type.decoder type_ of
        Ok resultType ->
            resultType

        Err _ ->
            Elm.Type.Var "unknown"
