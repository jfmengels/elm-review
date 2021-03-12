module Review.Test.Dependencies.ElmUrl exposing (dependency)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type exposing (Type(..))
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create "elm/url"
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ unsafeModuleName "Url", unsafeModuleName "Url.Builder", unsafeModuleName "Url.Parser", unsafeModuleName "Url.Parser.Query" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName "elm/url"
        , summary = "Create and parse URLs. Use for HTTP and \"routing\" in single-page apps (SPAs)"
        , deps = [ ( unsafePackageName "elm/core", unsafeConstraint "1.0.0 <= v < 2.0.0" ) ]
        , testDeps = []
        , version = Elm.Version.fromString "1.0.0" |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Url"
      , comment = """

# URLs
@docs Url, Protocol, toString, fromString

# Percent-Encoding
@docs percentEncode, percentDecode

"""
      , aliases =
            [ { name = "Url"
              , args = []
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
              , tipe = Record [ ( "protocol", Type "Url.Protocol" [] ), ( "host", Type "String.String" [] ), ( "port_", Type "Maybe.Maybe" [ Type "Basics.Int" [] ] ), ( "path", Type "String.String" [] ), ( "query", Type "Maybe.Maybe" [ Type "String.String" [] ] ), ( "fragment", Type "Maybe.Maybe" [ Type "String.String" [] ] ) ] Nothing
              }
            ]
      , unions =
            [ { name = "Protocol"
              , args = []
              , comment = """ Is the URL served over a secure connection or not?
"""
              , tags =
                    [ ( "Http", [] )
                    , ( "Https", [] )
                    ]
              }
            ]
      , binops = []
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
              , tipe = Lambda (Type "String.String" []) (Type "Maybe.Maybe" [ Type "Url.Url" [] ])
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
              , tipe = Lambda (Type "String.String" []) (Type "Maybe.Maybe" [ Type "String.String" [] ])
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
              , tipe = Lambda (Type "String.String" []) (Type "String.String" [])
              }
            , { name = "toString"
              , comment = """ Turn a [`Url`](#Url) into a `String`.
"""
              , tipe = Lambda (Type "Url.Url" []) (Type "String.String" [])
              }
            ]
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
      , aliases = []
      , unions =
            [ { name = "QueryParameter"
              , args = []
              , comment = """ Represents query parameter. Builder functions like `absolute` percent-encode
all the query parameters they get, so you do not need to worry about it!
"""
              , tags = []
              }
            , { name = "Root"
              , args = []
              , comment = """ Specify whether a [`custom`](#custom) URL is absolute, relative, or
cross-origin.
"""
              , tags =
                    [ ( "Absolute", [] )
                    , ( "Relative", [] )
                    , ( "CrossOrigin", [ Type "String.String" [] ] )
                    ]
              }
            ]
      , binops = []
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
              , tipe = Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "List.List" [ Type "Url.Builder.QueryParameter" [] ]) (Type "String.String" []))
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
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "List.List" [ Type "Url.Builder.QueryParameter" [] ]) (Type "String.String" [])))
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
              , tipe = Lambda (Type "Url.Builder.Root" []) (Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "List.List" [ Type "Url.Builder.QueryParameter" [] ]) (Lambda (Type "Maybe.Maybe" [ Type "String.String" [] ]) (Type "String.String" []))))
              }
            , { name = "int"
              , comment = """ Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat", int "page" 2 ]
    -- "/products?search=hat&page=2"

Writing `int key n` is the same as writing `string key (String.fromInt n)`.
So this is just a convenience function, making your code a bit shorter!
"""
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "Basics.Int" []) (Type "Url.Builder.QueryParameter" []))
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
              , tipe = Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "List.List" [ Type "Url.Builder.QueryParameter" [] ]) (Type "String.String" []))
              }
            , { name = "string"
              , comment = """ Create a percent-encoded query parameter.

    absolute ["products"] [ string "search" "hat" ]
    -- "/products?search=hat"

    absolute ["products"] [ string "search" "coffee table" ]
    -- "/products?search=coffee%20table"
"""
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "String.String" []) (Type "Url.Builder.QueryParameter" []))
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
              , tipe = Lambda (Type "List.List" [ Type "Url.Builder.QueryParameter" [] ]) (Type "String.String" [])
              }
            ]
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
      , aliases = []
      , unions =
            [ { name = "Parser"
              , args = [ "a", "b" ]
              , comment = """ Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data.
"""
              , tags = []
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
              , tipe = Lambda (Type "Url.Parser.Parser" [ Var "a", Var "b" ]) (Lambda (Type "Url.Parser.Parser" [ Var "b", Var "c" ]) (Type "Url.Parser.Parser" [ Var "a", Var "c" ]))
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
              , tipe = Lambda (Type "Url.Parser.Parser" [ Var "a", Lambda (Var "query") (Var "b") ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "query" ]) (Type "Url.Parser.Parser" [ Var "a", Var "b" ]))
              , associativity = Elm.Docs.Left
              , precedence = 8
              }
            ]
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
              , tipe = Lambda (Type "String.String" []) (Lambda (Lambda (Type "String.String" []) (Type "Maybe.Maybe" [ Var "a" ])) (Type "Url.Parser.Parser" [ Lambda (Var "a") (Var "b"), Var "b" ]))
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
              , tipe = Lambda (Lambda (Type "Maybe.Maybe" [ Type "String.String" [] ]) (Var "fragment")) (Type "Url.Parser.Parser" [ Lambda (Var "fragment") (Var "a"), Var "a" ])
              }
            , { name = "int"
              , comment = """ Parse a segment of the path as an `Int`.

    -- /alice/  ==>  Nothing
    -- /bob     ==>  Nothing
    -- /42/     ==>  Just 42
    -- /        ==>  Nothing
"""
              , tipe = Type "Url.Parser.Parser" [ Lambda (Type "Basics.Int" []) (Var "a"), Var "a" ]
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
              , tipe = Lambda (Var "a") (Lambda (Type "Url.Parser.Parser" [ Var "a", Var "b" ]) (Type "Url.Parser.Parser" [ Lambda (Var "b") (Var "c"), Var "c" ]))
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
              , tipe = Lambda (Type "List.List" [ Type "Url.Parser.Parser" [ Var "a", Var "b" ] ]) (Type "Url.Parser.Parser" [ Var "a", Var "b" ])
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
              , tipe = Lambda (Type "Url.Parser.Parser" [ Lambda (Var "a") (Var "a"), Var "a" ]) (Lambda (Type "Url.Url" []) (Type "Maybe.Maybe" [ Var "a" ]))
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
              , tipe = Lambda (Type "Url.Parser.Query.Parser" [ Var "query" ]) (Type "Url.Parser.Parser" [ Lambda (Var "query") (Var "a"), Var "a" ])
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
              , tipe = Lambda (Type "String.String" []) (Type "Url.Parser.Parser" [ Var "a", Var "a" ])
              }
            , { name = "string"
              , comment = """ Parse a segment of the path as a `String`.

    -- /alice/  ==>  Just "alice"
    -- /bob     ==>  Just "bob"
    -- /42/     ==>  Just "42"
    -- /        ==>  Nothing
"""
              , tipe = Type "Url.Parser.Parser" [ Lambda (Type "String.String" []) (Var "a"), Var "a" ]
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
              , tipe = Type "Url.Parser.Parser" [ Var "a", Var "a" ]
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
      , aliases =
            [ { name = "Parser"
              , args = [ "a" ]
              , comment = """ Parse a query like `?search=hat&page=2` into nice Elm data.
"""
              , tipe = Type "Url.Parser.Internal.QueryParser" [ Var "a" ]
              }
            ]
      , unions = []
      , binops = []
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
              , tipe = Lambda (Type "String.String" []) (Lambda (Lambda (Type "List.List" [ Type "String.String" [] ]) (Var "a")) (Type "Url.Parser.Query.Parser" [ Var "a" ]))
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
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "Dict.Dict" [ Type "String.String" [], Var "a" ]) (Type "Url.Parser.Query.Parser" [ Type "Maybe.Maybe" [ Var "a" ] ]))
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
              , tipe = Lambda (Type "String.String" []) (Type "Url.Parser.Query.Parser" [ Type "Maybe.Maybe" [ Type "Basics.Int" [] ] ])
              }
            , { name = "map"
              , comment = """ Transform a parser in some way. Maybe you want your `page` query parser to
default to `1` if there is any problem?

    page : Parser Int
    page =
      map (Result.withDefault 1) (int "page")

"""
              , tipe = Lambda (Lambda (Var "a") (Var "b")) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Type "Url.Parser.Query.Parser" [ Var "b" ]))
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
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Var "result"))) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "b" ]) (Type "Url.Parser.Query.Parser" [ Var "result" ])))
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
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Var "result")))) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "b" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "c" ]) (Type "Url.Parser.Query.Parser" [ Var "result" ]))))
              }
            , { name = "map4"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Var "result"))))) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "b" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "c" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "d" ]) (Type "Url.Parser.Query.Parser" [ Var "result" ])))))
              }
            , { name = "map5"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Var "result")))))) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "b" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "c" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "d" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "e" ]) (Type "Url.Parser.Query.Parser" [ Var "result" ]))))))
              }
            , { name = "map6"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Var "result"))))))) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "b" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "c" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "d" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "e" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "f" ]) (Type "Url.Parser.Query.Parser" [ Var "result" ])))))))
              }
            , { name = "map7"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Lambda (Var "g") (Var "result")))))))) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "b" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "c" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "d" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "e" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "f" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "g" ]) (Type "Url.Parser.Query.Parser" [ Var "result" ]))))))))
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
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Lambda (Var "g") (Lambda (Var "h") (Var "result"))))))))) (Lambda (Type "Url.Parser.Query.Parser" [ Var "a" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "b" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "c" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "d" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "e" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "f" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "g" ]) (Lambda (Type "Url.Parser.Query.Parser" [ Var "h" ]) (Type "Url.Parser.Query.Parser" [ Var "result" ])))))))))
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
              , tipe = Lambda (Type "String.String" []) (Type "Url.Parser.Query.Parser" [ Type "Maybe.Maybe" [ Type "String.String" [] ] ])
              }
            ]
      }
    ]


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafePackageName packageName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeModuleName : String -> Elm.Module.Name
unsafeModuleName moduleName =
    case Elm.Module.fromString moduleName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeModuleName moduleName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeConstraint : String -> Elm.Constraint.Constraint
unsafeConstraint constraint =
    case Elm.Constraint.fromString constraint of
        Just constr ->
            constr

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeConstraint constraint
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity
