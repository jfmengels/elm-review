module Review.Test.Dependencies.ElmParser exposing (dependency)

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
    Dependency.create "elm/parser"
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ unsafeModuleName "Parser", unsafeModuleName "Parser.Advanced" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName "elm/parser"
        , summary = "a parsing library, focused on simplicity and great error messages"
        , deps = [ ( unsafePackageName "elm/core", unsafeConstraint "1.0.0 <= v < 2.0.0" ) ]
        , testDeps = []
        , version = Elm.Version.fromString "1.1.0" |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Parser"
      , comment = """

# Parsers
@docs Parser, run

# Building Blocks
@docs int, float, number, symbol, keyword, variable, end

# Pipelines
@docs succeed, (|=), (|.), lazy, andThen, problem

# Branches
@docs oneOf, map, backtrackable, commit, token

# Loops
@docs sequence, Trailing, loop, Step

# Whitespace
@docs spaces, lineComment, multiComment, Nestable

# Chompers
@docs getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString

# Errors
@docs DeadEnd, Problem, deadEndsToString

# Indentation
@docs withIndent, getIndent

# Positions
@docs getPosition, getRow, getCol, getOffset, getSource
"""
      , aliases =
            [ { name = "DeadEnd"
              , args = []
              , comment = """ A parser can run into situations where there is no way to make progress.
When that happens, I record the `row` and `col` where you got stuck and the
particular `problem` you ran into. That is a `DeadEnd`!

**Note:** I count rows and columns like a text editor. The beginning is `row=1`
and `col=1`. As I chomp characters, the `col` increments. When I reach a `\\n`
character, I increment the `row` and set `col=1`.
"""
              , tipe = Record [ ( "row", Type "Basics.Int" [] ), ( "col", Type "Basics.Int" [] ), ( "problem", Type "Parser.Problem" [] ) ] Nothing
              }
            , { name = "Parser"
              , args = [ "a" ]
              , comment = """ A `Parser` helps turn a `String` into nicely structured data. For example,
we can [`run`](#run) the [`int`](#int) parser to turn `String` to `Int`:

    run int "123456" == Ok 123456
    run int "3.1415" == Err ...

The cool thing is that you can combine `Parser` values to handle much more
complex scenarios.
"""
              , tipe = Type "Parser.Advanced.Parser" [ Type "Basics.Never" [], Type "Parser.Problem" [], Var "a" ]
              }
            ]
      , unions =
            [ { name = "Nestable"
              , args = []
              , comment = """ Not all languages handle multi-line comments the same. Multi-line comments
in C-style syntax are `NotNestable`, meaning they can be implemented like this:

    js : Parser ()
    js =
      symbol "/*"
        |. chompUntil "*/"

In fact, `multiComment "/*" "*/" NotNestable` *is* implemented like that! It is
very simple, but it does not allow you to nest comments like this:

```javascript
/*
line1
/* line2 */
line3
*/
```

It would stop on the first `*/`, eventually throwing a syntax error on the
second `*/`. This can be pretty annoying in long files.

Languages like Elm allow you to nest multi-line comments, but your parser needs
to be a bit fancier to handle this. After you start a comment, you have to
detect if there is another one inside it! And then you have to make sure all
the `{-` and `-}` match up properly! Saying `multiComment "{-" "-}" Nestable`
does all that for you.
"""
              , tags =
                    [ ( "NotNestable", [] )
                    , ( "Nestable", [] )
                    ]
              }
            , { name = "Problem"
              , args = []
              , comment = """ When you run into a `DeadEnd`, I record some information about why you
got stuck. This data is useful for producing helpful error messages. This is
how [`deadEndsToString`](#deadEndsToString) works!

**Note:** If you feel limited by this type (i.e. having to represent custom
problems as strings) I highly recommend switching to `Parser.Advanced`. It
lets you define your own `Problem` type. It can also track "context" which
can improve error messages a ton! This is how the Elm compiler produces
relatively nice parse errors, and I am excited to see those techniques applied
elsewhere!
"""
              , tags =
                    [ ( "Expecting", [ Type "String.String" [] ] )
                    , ( "ExpectingInt", [] )
                    , ( "ExpectingHex", [] )
                    , ( "ExpectingOctal", [] )
                    , ( "ExpectingBinary", [] )
                    , ( "ExpectingFloat", [] )
                    , ( "ExpectingNumber", [] )
                    , ( "ExpectingVariable", [] )
                    , ( "ExpectingSymbol", [ Type "String.String" [] ] )
                    , ( "ExpectingKeyword", [ Type "String.String" [] ] )
                    , ( "ExpectingEnd", [] )
                    , ( "UnexpectedChar", [] )
                    , ( "Problem", [ Type "String.String" [] ] )
                    , ( "BadRepeat", [] )
                    ]
              }
            , { name = "Step"
              , args = [ "state", "a" ]
              , comment = """ Decide what steps to take next in your [`loop`](#loop).

If you are `Done`, you give the result of the whole `loop`. If you decide to
`Loop` around again, you give a new state to work from. Maybe you need to add
an item to a list? Or maybe you need to track some information about what you
just saw?

**Note:** It may be helpful to learn about [finite-state machines][fsm] to get
a broader intuition about using `state`. I.e. You may want to create a `type`
that describes four possible states, and then use `Loop` to transition between
them as you consume characters.

[fsm]: https://en.wikipedia.org/wiki/Finite-state_machine
"""
              , tags =
                    [ ( "Loop", [ Var "state" ] )
                    , ( "Done", [ Var "a" ] )
                    ]
              }
            , { name = "Trailing"
              , args = []
              , comment = """ What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](https://poorlydrawnlines.com/comic/shapes-club/)!
"""
              , tags =
                    [ ( "Forbidden", [] )
                    , ( "Optional", [] )
                    , ( "Mandatory", [] )
                    ]
              }
            ]
      , binops =
            [ { name = "|."
              , comment = """ **Skip** values in a parser pipeline. For example, maybe we want to parse
some JavaScript variables:

    var : Parser String
    var =
      getChompedString <|
        succeed ()
          |. chompIf isStartChar
          |. chompWhile isInnerChar

    isStartChar : Char -> Bool
    isStartChar char =
      Char.isAlpha char || char == '_' || char == '$'

    isInnerChar : Char -> Bool
    isInnerChar char =
      isStartChar char || Char.isDigit char

`chompIf isStartChar` can chomp one character and produce a `()` value.
`chompWhile isInnerChar` can chomp zero or more characters and produce a `()`
value. The `(|.)` operators are saying to still chomp all the characters, but
skip the two `()` values that get produced. No one cares about them.
"""
              , tipe = Lambda (Type "Parser.Parser" [ Var "keep" ]) (Lambda (Type "Parser.Parser" [ Var "ignore" ]) (Type "Parser.Parser" [ Var "keep" ]))
              , associativity = Elm.Docs.Left
              , precedence = 6
              }
            , { name = "|="
              , comment = """ **Keep** values in a parser pipeline. For example, we could say:

    type alias Point = { x : Float, y : Float }

    point : Parser Point
    point =
      succeed Point
        |. symbol "("
        |. spaces
        |= float
        |. spaces
        |. symbol ","
        |. spaces
        |= float
        |. spaces
        |. symbol ")"

All the parsers in this pipeline will chomp characters and produce values. So
`symbol "("` will chomp one paren and produce a `()` value. Similarly, `float`
will chomp some digits and produce a `Float` value. The `(|.)` and `(|=)`
operators just decide whether we give the values to the `Point` function.

So in this case, we skip the `()` from `symbol "("`, we skip the `()` from
`spaces`, we keep the `Float` from `float`, etc.
"""
              , tipe = Lambda (Type "Parser.Parser" [ Lambda (Var "a") (Var "b") ]) (Lambda (Type "Parser.Parser" [ Var "a" ]) (Type "Parser.Parser" [ Var "b" ]))
              , associativity = Elm.Docs.Left
              , precedence = 5
              }
            ]
      , values =
            [ { name = "andThen"
              , comment = """ Parse one thing `andThen` parse another thing. This is useful when you want
to check on what you just parsed. For example, maybe you want U.S. zip codes
and `int` is not suitable because it does not allow leading zeros. You could
say:

    zipCode : Parser String
    zipCode =
      getChompedString (chompWhile Char.isDigit)
        |> andThen checkZipCode

    checkZipCode : String -> Parser String
    checkZipCode code =
      if String.length code == 5 then
        succeed code
      else
        problem "a U.S. zip code has exactly 5 digits"

First we chomp digits `andThen` we check if it is a valid U.S. zip code. We
`succeed` if it has exactly five digits and report a `problem` if not.

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example, this time using `andThen` to verify unicode code points.

**Note:** If you are using `andThen` recursively and blowing the stack, check
out the [`loop`](#loop) function to limit stack usage.
"""
              , tipe = Lambda (Lambda (Var "a") (Type "Parser.Parser" [ Var "b" ])) (Lambda (Type "Parser.Parser" [ Var "a" ]) (Type "Parser.Parser" [ Var "b" ]))
              }
            , { name = "backtrackable"
              , comment = """ It is quite tricky to use `backtrackable` well! It can be very useful, but
also can degrade performance and error message quality.

Read [this document](https://github.com/elm/parser/blob/master/semantics.md)
to learn how `oneOf`, `backtrackable`, and `commit` work and interact with
each other. It is subtle and important!
"""
              , tipe = Lambda (Type "Parser.Parser" [ Var "a" ]) (Type "Parser.Parser" [ Var "a" ])
              }
            , { name = "chompIf"
              , comment = """ Chomp one character if it passes the test.

    chompUpper : Parser ()
    chompUpper =
      chompIf Char.isUpper

So this can chomp a character like `T` and produces a `()` value.
"""
              , tipe = Lambda (Lambda (Type "Char.Char" []) (Type "Basics.Bool" [])) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "chompUntil"
              , comment = """ Chomp until you see a certain string. You could define C-style multi-line
comments like this:

    comment : Parser ()
    comment =
      symbol "/*"
        |. chompUntil "*/"

I recommend using [`multiComment`](#multiComment) for this particular scenario
though. It can be trickier than it looks!
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "chompUntilEndOr"
              , comment = """ Chomp until you see a certain string or until you run out of characters to
chomp! You could define single-line comments like this:

    elm : Parser ()
    elm =
      symbol "--"
        |. chompUntilEndOr "\\n"

A file may end with a single-line comment, so the file can end before you see
a newline. Tricky!

I recommend just using [`lineComment`](#lineComment) for this particular
scenario.
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "chompWhile"
              , comment = """ Chomp zero or more characters if they pass the test. This is commonly
useful for chomping whitespace or variable names:

    whitespace : Parser ()
    whitespace =
      chompWhile (\\c -> c == ' ' || c == '\\t' || c == '\\n' || c == '\\r')

    elmVar : Parser String
    elmVar =
      getChompedString <|
        succeed ()
          |. chompIf Char.isLower
          |. chompWhile (\\c -> Char.isAlphaNum c || c == '_')

**Note:** a `chompWhile` parser always succeeds! This can lead to tricky
situations, especially if you define your whitespace with it. In that case,
you could accidentally interpret `letx` as the keyword `let` followed by
"spaces" followed by the variable `x`. This is why the `keyword` and `number`
parsers peek ahead, making sure they are not followed by anything unexpected.
"""
              , tipe = Lambda (Lambda (Type "Char.Char" []) (Type "Basics.Bool" [])) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "commit"
              , comment = """ `commit` is almost always paired with `backtrackable` in some way, and it
is tricky to use well.

Read [this document](https://github.com/elm/parser/blob/master/semantics.md)
to learn how `oneOf`, `backtrackable`, and `commit` work and interact with
each other. It is subtle and important!
"""
              , tipe = Lambda (Var "a") (Type "Parser.Parser" [ Var "a" ])
              }
            , { name = "deadEndsToString"
              , comment = """ Turn all the `DeadEnd` data into a string that is easier for people to
read.

**Note:** This is just a baseline of quality. It cannot do anything with colors.
It is not interactivite. It just turns the raw data into strings. I really hope
folks will check out the source code for some inspiration on how to turn errors
into `Html` with nice colors and interaction! The `Parser.Advanced` module lets
you work with context as well, which really unlocks another level of quality!
The "context" technique is how the Elm compiler can say "I think I am parsing a
list, so I was expecting a closing `]` here." Telling users what the parser
_thinks_ is happening can be really helpful!
"""
              , tipe = Lambda (Type "List.List" [ Type "Parser.DeadEnd" [] ]) (Type "String.String" [])
              }
            , { name = "end"
              , comment = """ Check if you have reached the end of the string you are parsing.

    justAnInt : Parser Int
    justAnInt =
      succeed identity
        |= int
        |. end

    -- run justAnInt "90210" == Ok 90210
    -- run justAnInt "1 + 2" == Err ...
    -- run int       "1 + 2" == Ok 1

Parsers can succeed without parsing the whole string. Ending your parser
with `end` guarantees that you have successfully parsed the whole string.
"""
              , tipe = Type "Parser.Parser" [ Tuple [] ]
              }
            , { name = "float"
              , comment = """ Parse floats.

    run float "123"       == Ok 123
    run float "3.1415"    == Ok 3.1415
    run float "0.1234"    == Ok 0.1234
    run float ".1234"     == Ok 0.1234
    run float "1e-42"     == Ok 1e-42
    run float "6.022e23"  == Ok 6.022e23
    run float "6.022E23"  == Ok 6.022e23
    run float "6.022e+23" == Ok 6.022e23

If you want to disable literals like `.123` (like in Elm) you could write
something like this:

    elmFloat : Parser Float
    elmFloat =
      oneOf
        [ symbol "."
            |. problem "floating point numbers must start with a digit, like 0.25"
        , float
        ]

**Note:** If you want a parser for both `Int` and `Float` literals, check out
[`number`](#number) below. It will be faster than using `oneOf` to combining
`int` and `float` yourself.
"""
              , tipe = Type "Parser.Parser" [ Type "Basics.Float" [] ]
              }
            , { name = "getChompedString"
              , comment = """ Sometimes parsers like `int` or `variable` cannot do exactly what you
need. The "chomping" family of functions is meant for that case! Maybe you
need to parse [valid PHP variables][php] like `$x` and `$txt`:

    php : Parser String
    php =
      getChompedString <|
        succeed ()
          |. chompIf (\\c -> c == '$')
          |. chompIf (\\c -> Char.isAlpha c || c == '_')
          |. chompWhile (\\c -> Char.isAlphaNum c || c == '_')

The idea is that you create a bunch of chompers that validate the underlying
characters. Then `getChompedString` extracts the underlying `String` efficiently.

**Note:** Maybe it is helpful to see how you can use [`getOffset`](#getOffset)
and [`getSource`](#getSource) to implement this function:

    getChompedString : Parser a -> Parser String
    getChompedString parser =
      succeed String.slice
        |= getOffset
        |. parser
        |= getOffset
        |= getSource

[php]: https://www.w3schools.com/php/php_variables.asp
"""
              , tipe = Lambda (Type "Parser.Parser" [ Var "a" ]) (Type "Parser.Parser" [ Type "String.String" [] ])
              }
            , { name = "getCol"
              , comment = """ This is a more efficient version of `map Tuple.second getPosition`. This
can be useful in combination with [`withIndent`](#withIndent) and
[`getIndent`](#getIndent), like this:

    checkIndent : Parser ()
    checkIndent =
      succeed (\\indent column -> indent <= column)
        |= getIndent
        |= getCol
        |> andThen checkIndentHelp

    checkIndentHelp : Bool -> Parser ()
    checkIndentHelp isIndented =
      if isIndented then
        succeed ()
      else
        problem "expecting more spaces"

So the `checkIndent` parser only succeeds when you are "deeper" than the
current indent level. You could use this to parse Elm-style `let` expressions.
"""
              , tipe = Type "Parser.Parser" [ Type "Basics.Int" [] ]
              }
            , { name = "getIndent"
              , comment = """ When someone said `withIndent` earlier, what number did they put in there?

- `getIndent` results in `0`, the default value
- `withIndent 4 getIndent` results in `4`

So you are just asking about things you said earlier. These numbers do not leak
out of `withIndent`, so say we have:

    succeed Tuple.pair
      |= withIndent 4 getIndent
      |= getIndent

Assuming there are no `withIndent` above this, you would get `(4,0)` from this.
"""
              , tipe = Type "Parser.Parser" [ Type "Basics.Int" [] ]
              }
            , { name = "getOffset"
              , comment = """ Editors think of code as a grid, but behind the scenes it is just a flat
array of UTF-16 characters. `getOffset` tells you your index in that flat
array. So if you chomp `"\\n\\n\\n\\n"` you are on row 5, column 1, and offset 4.

**Note:** JavaScript uses a somewhat odd version of UTF-16 strings, so a single
character may take two slots. So in JavaScript, `'abc'.length === 3` but
`'🙈🙉🙊'.length === 6`. Try it out! And since Elm runs in JavaScript, the offset
moves by those rules.
"""
              , tipe = Type "Parser.Parser" [ Type "Basics.Int" [] ]
              }
            , { name = "getPosition"
              , comment = """ Code editors treat code like a grid, with rows and columns. The start is
`row=1` and `col=1`. As you chomp characters, the `col` increments. When you
run into a `\\n` character, the `row` increments and `col` goes back to `1`.

In the Elm compiler, I track the start and end position of every expression
like this:

    type alias Located a =
      { start : (Int, Int)
      , value : a
      , end : (Int, Int)
      }

    located : Parser a -> Parser (Located a)
    located parser =
      succeed Located
        |= getPosition
        |= parser
        |= getPosition

So if there is a problem during type inference, I use this saved position
information to underline the exact problem!

**Note:** Tabs count as one character, so if you are parsing something like
Python, I recommend sorting that out *after* parsing. So if I wanted the `^^^^`
underline like in Elm, I would find the `row` in the source code and do
something like this:

    makeUnderline : String -> Int -> Int -> String
    makeUnderline row minCol maxCol =
      String.toList row
        |> List.indexedMap (toUnderlineChar minCol maxCol)
        |> String.fromList

    toUnderlineChar : Int -> Int -> Int -> Char -> Char
    toUnderlineChar minCol maxCol col char =
      if minCol <= col && col <= maxCol then
        '^'
      else if char == '\\t' then
        '\\t'
      else
        ' '

So it would preserve any tabs from the source line. There are tons of other
ways to do this though. The point is just that you handle the tabs after
parsing but before anyone looks at the numbers in a context where tabs may
equal 2, 4, or 8.
"""
              , tipe = Type "Parser.Parser" [ Tuple [ Type "Basics.Int" [], Type "Basics.Int" [] ] ]
              }
            , { name = "getRow"
              , comment = """ This is a more efficient version of `map Tuple.first getPosition`. Maybe
you just want to track the line number for some reason? This lets you do that.

See [`getPosition`](#getPosition) for an explanation of rows and columns.
"""
              , tipe = Type "Parser.Parser" [ Type "Basics.Int" [] ]
              }
            , { name = "getSource"
              , comment = """ Get the full string that is being parsed. You could use this to define
`getChompedString` or `mapChompedString` if you wanted:

    getChompedString : Parser a -> Parser String
    getChompedString parser =
      succeed String.slice
        |= getOffset
        |. parser
        |= getOffset
        |= getSource
"""
              , tipe = Type "Parser.Parser" [ Type "String.String" [] ]
              }
            , { name = "int"
              , comment = """ Parse integers.

    run int "1"    == Ok 1
    run int "1234" == Ok 1234

    run int "-789" == Err ...
    run int "0123" == Err ...
    run int "1.34" == Err ...
    run int "1e31" == Err ...
    run int "123a" == Err ...
    run int "0x1A" == Err ...

If you want to handle a leading `+` or `-` you should do it with a custom
parser like this:

    myInt : Parser Int
    myInt =
      oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]

**Note:** If you want a parser for both `Int` and `Float` literals, check out
[`number`](#number) below. It will be faster than using `oneOf` to combining
`int` and `float` yourself.
"""
              , tipe = Type "Parser.Parser" [ Type "Basics.Int" [] ]
              }
            , { name = "keyword"
              , comment = """ Parse keywords like `let`, `case`, and `type`.

    run (keyword "let") "let"     == Ok ()
    run (keyword "let") "var"     == Err ... (ExpectingKeyword "let") ...
    run (keyword "let") "letters" == Err ... (ExpectingKeyword "let") ...

**Note:** Notice the third case there! `keyword` actually looks ahead one
character to make sure it is not a letter, number, or underscore. The goal is
to help with parsers like this:

    succeed identity
      |. keyword "let"
      |. spaces
      |= elmVar
      |. spaces
      |. symbol "="

The trouble is that `spaces` may chomp zero characters (to handle expressions
like `[1,2]` and `[ 1 , 2 ]`) and in this case, it would mean `letters` could
be parsed as `let ters` and then wonder where the equals sign is! Check out the
[`token`](#token) docs if you need to customize this!
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "lazy"
              , comment = """ Helper to define recursive parsers. Say we want a parser for simple
boolean expressions:

    true
    false
    (true || false)
    (true || (true || false))

Notice that a boolean expression might contain *other* boolean expressions.
That means we will want to define our parser in terms of itself:

    type Boolean
      = MyTrue
      | MyFalse
      | MyOr Boolean Boolean

    boolean : Parser Boolean
    boolean =
      oneOf
        [ succeed MyTrue
            |. keyword "true"
        , succeed MyFalse
            |. keyword "false"
        , succeed MyOr
            |. symbol "("
            |. spaces
            |= lazy (\\_ -> boolean)
            |. spaces
            |. symbol "||"
            |. spaces
            |= lazy (\\_ -> boolean)
            |. spaces
            |. symbol ")"
        ]

**Notice that `boolean` uses `boolean` in its definition!** In Elm, you can
only define a value in terms of itself it is behind a function call. So
`lazy` helps us define these self-referential parsers. (`andThen` can be used
for this as well!)
"""
              , tipe = Lambda (Lambda (Tuple []) (Type "Parser.Parser" [ Var "a" ])) (Type "Parser.Parser" [ Var "a" ])
              }
            , { name = "lineComment"
              , comment = """ Parse single-line comments:

    elm : Parser ()
    elm =
      lineComment "--"

    js : Parser ()
    js =
      lineComment "//"

    python : Parser ()
    python =
      lineComment "#"

This parser is defined like this:

    lineComment : String -> Parser ()
    lineComment str =
      symbol str
        |. chompUntilEndOr "\\n"

So it will consume the remainder of the line. If the file ends before you see
a newline, that is fine too.
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "loop"
              , comment = """ A parser that can loop indefinitely. This can be helpful when parsing
repeated structures, like a bunch of statements:

    statements : Parser (List Stmt)
    statements =
      loop [] statementsHelp

    statementsHelp : List Stmt -> Parser (Step (List Stmt) (List Stmt))
    statementsHelp revStmts =
      oneOf
        [ succeed (\\stmt -> Loop (stmt :: revStmts))
            |= statement
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed ()
            |> map (\\_ -> Done (List.reverse revStmts))
        ]

    -- statement : Parser Stmt

Notice that the statements are tracked in reverse as we `Loop`, and we reorder
them only once we are `Done`. This is a very common pattern with `loop`!

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example.

**IMPORTANT NOTE:** Parsers like `succeed ()` and `chompWhile Char.isAlpha` can
succeed without consuming any characters. So in some cases you may want to use
[`getOffset`](#getOffset) to ensure that each step actually consumed characters.
Otherwise you could end up in an infinite loop!

**Note:** Anything you can write with `loop`, you can also write as a parser
that chomps some characters `andThen` calls itself with new arguments. The
problem with calling `andThen` recursively is that it grows the stack, so you
cannot do it indefinitely. So `loop` is important because enables tail-call
elimination, allowing you to parse however many repeats you want.
"""
              , tipe = Lambda (Var "state") (Lambda (Lambda (Var "state") (Type "Parser.Parser" [ Type "Parser.Step" [ Var "state", Var "a" ] ])) (Type "Parser.Parser" [ Var "a" ]))
              }
            , { name = "map"
              , comment = """ Transform the result of a parser. Maybe you have a value that is
an integer or `null`:

    nullOrInt : Parser (Maybe Int)
    nullOrInt =
      oneOf
        [ map Just int
        , map (\\_ -> Nothing) (keyword "null")
        ]

    -- run nullOrInt "0"    == Ok (Just 0)
    -- run nullOrInt "13"   == Ok (Just 13)
    -- run nullOrInt "null" == Ok Nothing
    -- run nullOrInt "zero" == Err ...
"""
              , tipe = Lambda (Lambda (Var "a") (Var "b")) (Lambda (Type "Parser.Parser" [ Var "a" ]) (Type "Parser.Parser" [ Var "b" ]))
              }
            , { name = "mapChompedString"
              , comment = """ This works just like [`getChompedString`](#getChompedString) but gives
a bit more flexibility. For example, maybe you want to parse Elm doc comments
and get (1) the full comment and (2) all of the names listed in the docs.

You could implement `mapChompedString` like this:

    mapChompedString : (String -> a -> b) -> Parser a -> Parser String
    mapChompedString func parser =
      succeed (\\start value end src -> func (String.slice start end src) value)
        |= getOffset
        |= parser
        |= getOffset
        |= getSource

"""
              , tipe = Lambda (Lambda (Type "String.String" []) (Lambda (Var "a") (Var "b"))) (Lambda (Type "Parser.Parser" [ Var "a" ]) (Type "Parser.Parser" [ Var "b" ]))
              }
            , { name = "multiComment"
              , comment = """ Parse multi-line comments. So if you wanted to parse Elm whitespace or
JS whitespace, you could say:

    elm : Parser ()
    elm =
      loop 0 <| ifProgress <|
        oneOf
          [ lineComment "--"
          , multiComment "{-" "-}" Nestable
          , spaces
          ]

    js : Parser ()
    js =
      loop 0 <| ifProgress <|
        oneOf
          [ lineComment "//"
          , multiComment "/*" "*/" NotNestable
          , chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r' || c == '\\t')
          ]

    ifProgress : Parser a -> Int -> Parser (Step Int ())
    ifProgress parser offset =
      succeed identity
        |. parser
        |= getOffset
        |> map (\\newOffset -> if offset == newOffset then Done () else Loop newOffset)

**Note:** The fact that `spaces` comes last in the definition of `elm` is very
important! It can succeed without consuming any characters, so if it were the
first option, it would always succeed and bypass the others! (Same is true of
`chompWhile` in `js`.) This possibility of success without consumption is also
why wee need the `ifProgress` helper. It detects if there is no more whitespace
to consume.
"""
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "String.String" []) (Lambda (Type "Parser.Nestable" []) (Type "Parser.Parser" [ Tuple [] ])))
              }
            , { name = "number"
              , comment = """ Parse a bunch of different kinds of numbers without backtracking. A parser
for Elm would need to handle integers, floats, and hexadecimal like this:

    type Expr
      = Variable String
      | Int Int
      | Float Float
      | Apply Expr Expr

    elmNumber : Parser Expr
    elmNumber =
      number
        { int = Just Int
        , hex = Just Int    -- 0x001A is allowed
        , octal = Nothing   -- 0o0731 is not
        , binary = Nothing  -- 0b1101 is not
        , float = Just Float
        }

If you wanted to implement the [`float`](#float) parser, it would be like this:

    float : Parser Float
    float =
      number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }

Notice that it actually is processing `int` results! This is because `123`
looks like an integer to me, but maybe it looks like a float to you. If you had
`int = Nothing`, floats would need a decimal like `1.0` in every case. If you
like explicitness, that may actually be preferable!

**Note:** This function does not check for weird trailing characters in the
current implementation, so parsing `123abc` can succeed up to `123` and then
move on. This is helpful for people who want to parse things like `40px` or
`3m`, but it requires a bit of extra code to rule out trailing characters in
other cases.
"""
              , tipe = Lambda (Record [ ( "int", Type "Maybe.Maybe" [ Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "hex", Type "Maybe.Maybe" [ Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "octal", Type "Maybe.Maybe" [ Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "binary", Type "Maybe.Maybe" [ Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "float", Type "Maybe.Maybe" [ Lambda (Type "Basics.Float" []) (Var "a") ] ) ] Nothing) (Type "Parser.Parser" [ Var "a" ])
              }
            , { name = "oneOf"
              , comment = """ If you are parsing JSON, the values can be strings, floats, booleans,
arrays, objects, or null. You need a way to pick `oneOf` them! Here is a
sample of what that code might look like:

    type Json
      = Number Float
      | Boolean Bool
      | Null

    json : Parser Json
    json =
      oneOf
        [ map Number float
        , map (\\_ -> Boolean True) (keyword "true")
        , map (\\_ -> Boolean False) (keyword "false")
        , map (\\_ -> Null) keyword "null"
        ]

This parser will keep trying parsers until `oneOf` them starts chomping
characters. Once a path is chosen, it does not come back and try the others.

**Note:** I highly recommend reading [this document][semantics] to learn how
`oneOf` and `backtrackable` interact. It is subtle and important!

[semantics]: https://github.com/elm/parser/blob/master/semantics.md
"""
              , tipe = Lambda (Type "List.List" [ Type "Parser.Parser" [ Var "a" ] ]) (Type "Parser.Parser" [ Var "a" ])
              }
            , { name = "problem"
              , comment = """ Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the [`andThen`](#andThen) docs to see
an example usage.
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Parser" [ Var "a" ])
              }
            , { name = "run"
              , comment = """ Try a parser. Here are some examples using the [`keyword`](#keyword)
parser:

    run (keyword "true") "true"  == Ok ()
    run (keyword "true") "True"  == Err ...
    run (keyword "true") "false" == Err ...
    run (keyword "true") "true!" == Ok ()

Notice the last case! A `Parser` will chomp as much as possible and not worry
about the rest. Use the [`end`](#end) parser to ensure you made it to the end
of the string!
"""
              , tipe = Lambda (Type "Parser.Parser" [ Var "a" ]) (Lambda (Type "String.String" []) (Type "Result.Result" [ Type "List.List" [ Type "Parser.DeadEnd" [] ], Var "a" ]))
              }
            , { name = "sequence"
              , comment = """ Handle things like lists and records, but you can customize the details
however you need. Say you want to parse C-style code blocks:

    import Parser exposing (Parser, Trailing(..))

    block : Parser (List Stmt)
    block =
      Parser.sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = statement
        , trailing = Mandatory -- demand a trailing semi-colon
        }

    -- statement : Parser Stmt

**Note:** If you need something more custom, do not be afraid to check
out the implementation and customize it for your case. It is better to
get nice error messages with a lower-level implementation than to try
to hack high-level parsers to do things they are not made for.
"""
              , tipe = Lambda (Record [ ( "start", Type "String.String" [] ), ( "separator", Type "String.String" [] ), ( "end", Type "String.String" [] ), ( "spaces", Type "Parser.Parser" [ Tuple [] ] ), ( "item", Type "Parser.Parser" [ Var "a" ] ), ( "trailing", Type "Parser.Trailing" [] ) ] Nothing) (Type "Parser.Parser" [ Type "List.List" [ Var "a" ] ])
              }
            , { name = "spaces"
              , comment = """ Parse zero or more `' '`, `'\\n'`, and `'\\r'` characters.

The implementation is pretty simple:

    spaces : Parser ()
    spaces =
      chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')

So if you need something different (like tabs) just define an alternative with
the necessary tweaks! Check out [`lineComment`](#lineComment) and
[`multiComment`](#multiComment) for more complex situations.
"""
              , tipe = Type "Parser.Parser" [ Tuple [] ]
              }
            , { name = "succeed"
              , comment = """ A parser that succeeds without chomping any characters.

    run (succeed 90210  ) "mississippi" == Ok 90210
    run (succeed 3.141  ) "mississippi" == Ok 3.141
    run (succeed ()     ) "mississippi" == Ok ()
    run (succeed Nothing) "mississippi" == Ok Nothing

Seems weird on its own, but it is very useful in combination with other
functions. The docs for [`(|=)`](#|=) and [`andThen`](#andThen) have some neat
examples.
"""
              , tipe = Lambda (Var "a") (Type "Parser.Parser" [ Var "a" ])
              }
            , { name = "symbol"
              , comment = """ Parse symbols like `(` and `,`.

    run (symbol "[") "[" == Ok ()
    run (symbol "[") "4" == Err ... (ExpectingSymbol "[") ...

**Note:** This is good for stuff like brackets and semicolons, but it probably
should not be used for binary operators like `+` and `-` because you can find
yourself in weird situations. For example, is `3--4` a typo? Or is it `3 - -4`?
I have had better luck with `chompWhile isSymbol` and sorting out which
operator it is afterwards.
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "token"
              , comment = """ Parse exactly the given string, without any regard to what comes next.

A potential pitfall when parsing keywords is getting tricked by variables that
start with a keyword, like `let` in `letters` or `import` in `important`. This
is especially likely if you have a whitespace parser that can consume zero
charcters. So the [`keyword`](#keyword) parser is defined with `token` and a
trick to peek ahead a bit:

    keyword : String -> Parser ()
    keyword kwd =
      succeed identity
        |. backtrackable (token kwd)
        |= oneOf
            [ map (\\_ -> True) (backtrackable (chompIf isVarChar))
            , succeed False
            ]
        |> andThen (checkEnding kwd)

    checkEnding : String -> Bool -> Parser ()
    checkEnding kwd isBadEnding =
      if isBadEnding then
        problem ("expecting the `" ++ kwd ++ "` keyword")
      else
        commit ()

    isVarChar : Char -> Bool
    isVarChar char =
      Char.isAlphaNum char || char == '_'

This definition is specially designed so that (1) if you really see `let` you
commit to that path and (2) if you see `letters` instead you can backtrack and
try other options. If I had just put a `backtrackable` around the whole thing
you would not get (1) anymore.
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Parser" [ Tuple [] ])
              }
            , { name = "variable"
              , comment = """ Create a parser for variables. If we wanted to parse type variables in Elm,
we could try something like this:

    import Char
    import Parser exposing (..)
    import Set

    typeVar : Parser String
    typeVar =
      variable
        { start = Char.isLower
        , inner = \\c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "let", "in", "case", "of" ]
        }

This is saying it _must_ start with a lower-case character. After that,
characters can be letters, numbers, or underscores. It is also saying that if
you run into any of these reserved names, it is definitely not a variable.
"""
              , tipe = Lambda (Record [ ( "start", Lambda (Type "Char.Char" []) (Type "Basics.Bool" []) ), ( "inner", Lambda (Type "Char.Char" []) (Type "Basics.Bool" []) ), ( "reserved", Type "Set.Set" [ Type "String.String" [] ] ) ] Nothing) (Type "Parser.Parser" [ Type "String.String" [] ])
              }
            , { name = "withIndent"
              , comment = """ Some languages are indentation sensitive. Python cares about tabs. Elm
cares about spaces sometimes. `withIndent` and `getIndent` allow you to manage
"indentation state" yourself, however is necessary in your scenario.
"""
              , tipe = Lambda (Type "Basics.Int" []) (Lambda (Type "Parser.Parser" [ Var "a" ]) (Type "Parser.Parser" [ Var "a" ]))
              }
            ]
      }
    , { name = "Parser.Advanced"
      , comment = """

# Parsers
@docs Parser, run, DeadEnd, inContext, Token

* * *
**Everything past here works just like in the
[`Parser`](/packages/elm/parser/latest/Parser) module, except that `String`
arguments become `Token` arguments, and you need to provide a `Problem` for
certain scenarios.**
* * *

# Building Blocks
@docs int, float, number, symbol, keyword, variable, end

# Pipelines
@docs succeed, (|=), (|.), lazy, andThen, problem

# Branches
@docs oneOf, map, backtrackable, commit, token

# Loops
@docs sequence, Trailing, loop, Step

# Whitespace
@docs spaces, lineComment, multiComment, Nestable

# Chompers
@docs getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString

# Indentation
@docs withIndent, getIndent

# Positions
@docs getPosition, getRow, getCol, getOffset, getSource
"""
      , aliases =
            [ { name = "DeadEnd"
              , args = [ "context", "problem" ]
              , comment = """ Say you are parsing a function named `viewHealthData` that contains a list.
You might get a `DeadEnd` like this:

```elm
{ row = 18
, col = 22
, problem = UnexpectedComma
, contextStack =
    [ { row = 14
      , col = 1
      , context = Definition "viewHealthData"
      }
    , { row = 15
      , col = 4
      , context = List
      }
    ]
}
```

We have a ton of information here! So in the error message, we can say that “I
ran into an issue when parsing a list in the definition of `viewHealthData`. It
looks like there is an extra comma.” Or maybe something even better!

Furthermore, many parsers just put a mark where the problem manifested. By
tracking the `row` and `col` of the context, we can show a much larger region
as a way of indicating “I thought I was parsing this thing that starts over
here.” Otherwise you can get very confusing error messages on a missing `]` or
`}` or `)` because “I need more indentation” on something unrelated.

**Note:** Rows and columns are counted like a text editor. The beginning is `row=1`
and `col=1`. The `col` increments as characters are chomped. When a `\\n` is chomped,
`row` is incremented and `col` starts over again at `1`.
"""
              , tipe = Record [ ( "row", Type "Basics.Int" [] ), ( "col", Type "Basics.Int" [] ), ( "problem", Var "problem" ), ( "contextStack", Type "List.List" [ Record [ ( "row", Type "Basics.Int" [] ), ( "col", Type "Basics.Int" [] ), ( "context", Var "context" ) ] Nothing ] ) ] Nothing
              }
            ]
      , unions =
            [ { name = "Nestable"
              , args = []
              , comment = """ Works just like [`Parser.Nestable`](Parser#nestable) to help distinguish
between unnestable `/*` `*/` comments like in JS and nestable `{-` `-}`
comments like in Elm.
"""
              , tags =
                    [ ( "NotNestable", [] )
                    , ( "Nestable", [] )
                    ]
              }
            , { name = "Parser"
              , args = [ "context", "problem", "value" ]
              , comment = """ An advanced `Parser` gives two ways to improve your error messages:

- `problem` &mdash; Instead of all errors being a `String`, you can create a
custom type like `type Problem = BadIndent | BadKeyword String` and track
problems much more precisely.
- `context` &mdash; Error messages can be further improved when precise
problems are paired with information about where you ran into trouble. By
tracking the context, instead of saying “I found a bad keyword” you can say
“I found a bad keyword when parsing a list” and give folks a better idea of
what the parser thinks it is doing.

I recommend starting with the simpler [`Parser`][parser] module though, and
when you feel comfortable and want better error messages, you can create a type
alias like this:

```elm
import Parser.Advanced

type alias MyParser a =
  Parser.Advanced.Parser Context Problem a

type Context = Definition String | List | Record

type Problem = BadIndent | BadKeyword String
```

All of the functions from `Parser` should exist in `Parser.Advanced` in some
form, allowing you to switch over pretty easily.

[parser]: /packages/elm/parser/latest/Parser
"""
              , tags = []
              }
            , { name = "Step"
              , args = [ "state", "a" ]
              , comment = """ Just like [`Parser.Step`](Parser#Step)
"""
              , tags =
                    [ ( "Loop", [ Var "state" ] )
                    , ( "Done", [ Var "a" ] )
                    ]
              }
            , { name = "Token"
              , args = [ "x" ]
              , comment = """ With the simpler `Parser` module, you could just say `symbol ","` and
parse all the commas you wanted. But now that we have a custom type for our
problems, we actually have to specify that as well. So anywhere you just used
a `String` in the simpler module, you now use a `Token Problem` in the advanced
module:

    type Problem
      = ExpectingComma
      | ExpectingListEnd

    comma : Token Problem
    comma =
      Token "," ExpectingComma

    listEnd : Token Problem
    listEnd =
      Token "]" ExpectingListEnd

You can be creative with your custom type. Maybe you want a lot of detail.
Maybe you want looser categories. It is a custom type. Do what makes sense for
you!
"""
              , tags = [ ( "Token", [ Type "String.String" [], Var "x" ] ) ]
              }
            , { name = "Trailing"
              , args = []
              , comment = """ What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](https://poorlydrawnlines.com/comic/shapes-club/)!
"""
              , tags =
                    [ ( "Forbidden", [] )
                    , ( "Optional", [] )
                    , ( "Mandatory", [] )
                    ]
              }
            ]
      , binops =
            [ { name = "|."
              , comment = """ Just like the [`(|.)`](Parser#|.) from the `Parser` module.
"""
              , tipe = Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "keep" ]) (Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "ignore" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "keep" ]))
              , associativity = Elm.Docs.Left
              , precedence = 6
              }
            , { name = "|="
              , comment = """ Just like the [`(|=)`](Parser#|=) from the `Parser` module.
"""
              , tipe = Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Lambda (Var "a") (Var "b") ]) (Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "b" ]))
              , associativity = Elm.Docs.Left
              , precedence = 5
              }
            ]
      , values =
            [ { name = "andThen"
              , comment = """ Just like [`Parser.andThen`](Parser#andThen)
"""
              , tipe = Lambda (Lambda (Var "a") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "b" ])) (Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "b" ]))
              }
            , { name = "backtrackable"
              , comment = """ Just like [`Parser.backtrackable`](Parser#backtrackable)
"""
              , tipe = Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])
              }
            , { name = "chompIf"
              , comment = """ Just like [`Parser.chompIf`](Parser#chompIf) except you provide a problem
in case a character cannot be chomped.
"""
              , tipe = Lambda (Lambda (Type "Char.Char" []) (Type "Basics.Bool" [])) (Lambda (Var "x") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ]))
              }
            , { name = "chompUntil"
              , comment = """ Just like [`Parser.chompUntil`](Parser#chompUntil) except you provide a
`Token` in case you chomp all the way to the end of the input without finding
what you need.
"""
              , tipe = Lambda (Type "Parser.Advanced.Token" [ Var "x" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "chompUntilEndOr"
              , comment = """ Just like [`Parser.chompUntilEndOr`](Parser#chompUntilEndOr)
"""
              , tipe = Lambda (Type "String.String" []) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "chompWhile"
              , comment = """ Just like [`Parser.chompWhile`](Parser#chompWhile)
"""
              , tipe = Lambda (Lambda (Type "Char.Char" []) (Type "Basics.Bool" [])) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "commit"
              , comment = """ Just like [`Parser.commit`](Parser#commit)
"""
              , tipe = Lambda (Var "a") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])
              }
            , { name = "end"
              , comment = """ Just like [`Parser.end`](Parser#end) except you provide the problem that
arises when the parser is not at the end of the input.
"""
              , tipe = Lambda (Var "x") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "float"
              , comment = """ Just like [`Parser.float`](Parser#float) where you have to handle negation
yourself. The only difference is that you provide a two potential problems:

    float : x -> x -> Parser c x Float
    float expecting invalid =
      number
        { int = Ok toFloat
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Ok identity
        , invalid = invalid
        , expecting = expecting
        }

You can use problems like `ExpectingFloat` and `InvalidNumber`.
"""
              , tipe = Lambda (Var "x") (Lambda (Var "x") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "Basics.Float" [] ]))
              }
            , { name = "getChompedString"
              , comment = """ Just like [`Parser.getChompedString`](Parser#getChompedString)
"""
              , tipe = Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "String.String" [] ])
              }
            , { name = "getCol"
              , comment = """ Just like [`Parser.getCol`](Parser#getCol)
"""
              , tipe = Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "Basics.Int" [] ]
              }
            , { name = "getIndent"
              , comment = """ Just like [`Parser.getIndent`](Parser#getIndent)
"""
              , tipe = Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "Basics.Int" [] ]
              }
            , { name = "getOffset"
              , comment = """ Just like [`Parser.getOffset`](Parser#getOffset)
"""
              , tipe = Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "Basics.Int" [] ]
              }
            , { name = "getPosition"
              , comment = """ Just like [`Parser.getPosition`](Parser#getPosition)
"""
              , tipe = Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [ Type "Basics.Int" [], Type "Basics.Int" [] ] ]
              }
            , { name = "getRow"
              , comment = """ Just like [`Parser.getRow`](Parser#getRow)
"""
              , tipe = Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "Basics.Int" [] ]
              }
            , { name = "getSource"
              , comment = """ Just like [`Parser.getSource`](Parser#getSource)
"""
              , tipe = Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "String.String" [] ]
              }
            , { name = "inContext"
              , comment = """ This is how you mark that you are in a certain context. For example, here
is a rough outline of some code that uses `inContext` to mark when you are
parsing a specific definition:

    import Char
    import Parser.Advanced exposing (..)
    import Set

    type Context
      = Definition String
      | List

    definition : Parser Context Problem Expr
    definition =
      functionName
        |> andThen definitionBody

    definitionBody : String -> Parser Context Problem Expr
    definitionBody name =
      inContext (Definition name) <|
        succeed (Function name)
          |= arguments
          |. symbol (Token "=" ExpectingEquals)
          |= expression

    functionName : Parser c Problem String
    functionName =
      variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList ["let","in"]
        , expecting = ExpectingFunctionName
        }

First we parse the function name, and then we parse the rest of the definition.
Importantly, we call `inContext` so that any dead end that occurs in
`definitionBody` will get this extra context information. That way you can say
things like, “I was expecting an equals sign in the `view` definition.” Context!
"""
              , tipe = Lambda (Var "context") (Lambda (Type "Parser.Advanced.Parser" [ Var "context", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "context", Var "x", Var "a" ]))
              }
            , { name = "int"
              , comment = """ Just like [`Parser.int`](Parser#int) where you have to handle negation
yourself. The only difference is that you provide a two potential problems:

    int : x -> x -> Parser c x Int
    int expecting invalid =
      number
        { int = Ok identity
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Err invalid
        , invalid = invalid
        , expecting = expecting
        }

You can use problems like `ExpectingInt` and `InvalidNumber`.
"""
              , tipe = Lambda (Var "x") (Lambda (Var "x") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "Basics.Int" [] ]))
              }
            , { name = "keyword"
              , comment = """ Just like [`Parser.keyword`](Parser#keyword) except you provide a `Token`
to clearly indicate your custom type of problems:

    let_ : Parser Context Problem ()
    let_ =
      symbol (Token "let" ExpectingLet)

Note that this would fail to chomp `letter` because of the subsequent
characters. Use `token` if you do not want that last letter check.
"""
              , tipe = Lambda (Type "Parser.Advanced.Token" [ Var "x" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "lazy"
              , comment = """ Just like [`Parser.lazy`](Parser#lazy)
"""
              , tipe = Lambda (Lambda (Tuple []) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])
              }
            , { name = "lineComment"
              , comment = """ Just like [`Parser.lineComment`](Parser#lineComment) except you provide a
`Token` describing the starting symbol.
"""
              , tipe = Lambda (Type "Parser.Advanced.Token" [ Var "x" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "loop"
              , comment = """ Just like [`Parser.loop`](Parser#loop)
"""
              , tipe = Lambda (Var "state") (Lambda (Lambda (Var "state") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "Parser.Advanced.Step" [ Var "state", Var "a" ] ])) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]))
              }
            , { name = "map"
              , comment = """ Just like [`Parser.map`](Parser#map)
"""
              , tipe = Lambda (Lambda (Var "a") (Var "b")) (Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "b" ]))
              }
            , { name = "mapChompedString"
              , comment = """ Just like [`Parser.mapChompedString`](Parser#mapChompedString)
"""
              , tipe = Lambda (Lambda (Type "String.String" []) (Lambda (Var "a") (Var "b"))) (Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "b" ]))
              }
            , { name = "multiComment"
              , comment = """ Just like [`Parser.multiComment`](Parser#multiComment) except with a
`Token` for the open and close symbols.
"""
              , tipe = Lambda (Type "Parser.Advanced.Token" [ Var "x" ]) (Lambda (Type "Parser.Advanced.Token" [ Var "x" ]) (Lambda (Type "Parser.Advanced.Nestable" []) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])))
              }
            , { name = "number"
              , comment = """ Just like [`Parser.number`](Parser#number) where you have to handle
negation yourself. The only difference is that you provide all the potential
problems.
"""
              , tipe = Lambda (Record [ ( "int", Type "Result.Result" [ Var "x", Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "hex", Type "Result.Result" [ Var "x", Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "octal", Type "Result.Result" [ Var "x", Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "binary", Type "Result.Result" [ Var "x", Lambda (Type "Basics.Int" []) (Var "a") ] ), ( "float", Type "Result.Result" [ Var "x", Lambda (Type "Basics.Float" []) (Var "a") ] ), ( "invalid", Var "x" ), ( "expecting", Var "x" ) ] Nothing) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])
              }
            , { name = "oneOf"
              , comment = """ Just like [`Parser.oneOf`](Parser#oneOf)
"""
              , tipe = Lambda (Type "List.List" [ Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ] ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])
              }
            , { name = "problem"
              , comment = """ Just like [`Parser.problem`](Parser#problem) except you provide a custom
type for your problem.
"""
              , tipe = Lambda (Var "x") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])
              }
            , { name = "run"
              , comment = """ This works just like [`Parser.run`](/packages/elm/parser/latest/Parser#run).
The only difference is that when it fails, it has much more precise information
for each dead end.
"""
              , tipe = Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Lambda (Type "String.String" []) (Type "Result.Result" [ Type "List.List" [ Type "Parser.Advanced.DeadEnd" [ Var "c", Var "x" ] ], Var "a" ]))
              }
            , { name = "sequence"
              , comment = """ Just like [`Parser.sequence`](Parser#sequence) except with a `Token` for
the start, separator, and end. That way you can specify your custom type of
problem for when something is not found.
"""
              , tipe = Lambda (Record [ ( "start", Type "Parser.Advanced.Token" [ Var "x" ] ), ( "separator", Type "Parser.Advanced.Token" [ Var "x" ] ), ( "end", Type "Parser.Advanced.Token" [ Var "x" ] ), ( "spaces", Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ] ), ( "item", Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ] ), ( "trailing", Type "Parser.Advanced.Trailing" [] ) ] Nothing) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "List.List" [ Var "a" ] ])
              }
            , { name = "spaces"
              , comment = """ Just like [`Parser.spaces`](Parser#spaces)
"""
              , tipe = Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ]
              }
            , { name = "succeed"
              , comment = """ Just like [`Parser.succeed`](Parser#succeed)
"""
              , tipe = Lambda (Var "a") (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ])
              }
            , { name = "symbol"
              , comment = """ Just like [`Parser.symbol`](Parser#symbol) except you provide a `Token` to
clearly indicate your custom type of problems:

    comma : Parser Context Problem ()
    comma =
      symbol (Token "," ExpectingComma)

"""
              , tipe = Lambda (Type "Parser.Advanced.Token" [ Var "x" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "token"
              , comment = """ Just like [`Parser.token`](Parser#token) except you provide a `Token`
specifying your custom type of problems.
"""
              , tipe = Lambda (Type "Parser.Advanced.Token" [ Var "x" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Tuple [] ])
              }
            , { name = "variable"
              , comment = """ Just like [`Parser.variable`](Parser#variable) except you specify the
problem yourself.
"""
              , tipe = Lambda (Record [ ( "start", Lambda (Type "Char.Char" []) (Type "Basics.Bool" []) ), ( "inner", Lambda (Type "Char.Char" []) (Type "Basics.Bool" []) ), ( "reserved", Type "Set.Set" [ Type "String.String" [] ] ), ( "expecting", Var "x" ) ] Nothing) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Type "String.String" [] ])
              }
            , { name = "withIndent"
              , comment = """ Just like [`Parser.withIndent`](Parser#withIndent)
"""
              , tipe = Lambda (Type "Basics.Int" []) (Lambda (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]) (Type "Parser.Advanced.Parser" [ Var "c", Var "x", Var "a" ]))
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
