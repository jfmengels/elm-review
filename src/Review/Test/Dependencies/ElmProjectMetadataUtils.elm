module Review.Test.Dependencies.ElmProjectMetadataUtils exposing (dependency)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create "elm/project-metadata-utils"
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedDict [ ( "Metadata", [ unsafeModuleName "Elm.Docs", unsafeModuleName "Elm.Project", unsafeModuleName "Elm.Error" ] ), ( "Helpers", [ unsafeModuleName "Elm.Type", unsafeModuleName "Elm.Module", unsafeModuleName "Elm.Package", unsafeModuleName "Elm.Version", unsafeModuleName "Elm.Constraint", unsafeModuleName "Elm.License" ] ) ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName "elm/project-metadata-utils"
        , summary = "Work with elm.json and docs.json files in Elm"
        , deps =
            [ ( unsafePackageName "elm/core", unsafeConstraint "1.0.0 <= v < 2.0.0" )
            , ( unsafePackageName "elm/json", unsafeConstraint "1.0.0 <= v < 2.0.0" )
            , ( unsafePackageName "elm/parser", unsafeConstraint "1.0.0 <= v < 2.0.0" )
            ]
        , testDeps = []
        , version = Elm.Version.fromString "1.0.1" |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Elm.Constraint"
      , comment = """ Helpers for working with version constraint strings in `elm.json` files.

# Constraint
@docs Constraint, check

# String Conversions
@docs toString, fromString

# JSON Conversions
@docs encode, decoder

"""
      , aliases = []
      , unions =
            [ { name = "Constraint"
              , args = []
              , comment = """ A guaranteed valid Elm constraint. That means the lower bound `v1` and
the upper bound `v2` are both valid `Elm.Version` versions, and `v1 <= v2` is
guaranteed.
"""
              , tags = []
              }
            ]
      , binops = []
      , values =
            [ { name = "check"
              , comment = """ Check if a version is within the given constraint:

    import Elm.Version as V

    oneToTwo = fromString "1.0.0 <= v < 2.0.0"
    sixToTen = fromString "6.0.0 <= v < 10.0.0"

    -- Maybe.map (check V.one) oneToTwo == Just True
    -- Maybe.map (check V.one) sixToTen == Just False
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Version.Version" []) (Elm.Type.Lambda (Elm.Type.Type "Elm.Constraint.Constraint" []) (Elm.Type.Type "Basics.Bool" []))
              }
            , { name = "decoder"
              , comment = """ Decode the constraint strings that appear in `elm.json`
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Constraint.Constraint" [] ]
              }
            , { name = "encode"
              , comment = """ Turn a `Constraint` into a string for use in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Constraint.Constraint" []) (Elm.Type.Type "Json.Encode.Value" [])
              }
            , { name = "fromString"
              , comment = """ Try to convert a `String` into a `Constraint`:

    fromString "1.0.0 <= v < 2.0.0"   == Just ...
    fromString "1.0.0 <= v < 10.0.0"  == Just ...
    fromString "1.0.0 <= v <= 1.0.0"  == Just ...

    fromString "1.0.0"                == Nothing
    fromString "1.0.0 <= 2.0.0"       == Nothing
    fromString "1.0.0 <=  v  < 2.0.0" == Nothing -- extra spaces
    fromString "1.0.0 <= vsn < 2.0.0" == Nothing -- not "v" only
    fromString "2.0.0 <= v < 1.0.0"   == Nothing -- unsatisfiable
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "String.String" []) (Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "Elm.Constraint.Constraint" [] ])
              }
            , { name = "toString"
              , comment = """ Convert a `Constraint` to a `String` that works in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Constraint.Constraint" []) (Elm.Type.Type "String.String" [])
              }
            ]
      }
    , { name = "Elm.Docs"
      , comment = """ When packages are published to `package.elm-lang.org`, documentation
is generated for all of the exposed modules (and all of the exposed values).
These docs are formatted as JSON for easy consumption by anyone.

This module helps you decode the JSON docs into nice Elm values! It is
currently used by `package.elm-lang.org` to help turn JSON into nice
web pages!

# Decode Docs
@docs decoder

# Work with Docs
@docs Module, Alias, Union, Value, Binop, Associativity

# Split Docs into Blocks
@docs toBlocks, Block

"""
      , aliases =
            [ { name = "Alias"
              , args = []
              , comment = """ Documentation for a type alias. For example, if you had the source code:

    {-| pair of values -}
    type alias Pair a = ( a, a )

When it became an `Alias` it would be like this:

    { name = "Pair"
    , comment = " pair of values "
    , args = ["a"]
    , tipe = Tuple [ Var "a", Var "a" ]
    }
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "name", Elm.Type.Type "String.String" [] )
                        , ( "comment", Elm.Type.Type "String.String" [] )
                        , ( "args", Elm.Type.Type "List.List" [ Elm.Type.Type "String.String" [] ] )
                        , ( "tipe", Elm.Type.Type "Elm.Type.Type" [] )
                        ]
                        Nothing
              }
            , { name = "Binop"
              , args = []
              , comment = """ Documentation for binary operators. The content for `(+)` might look
something like this:

    { name = "+"
    , comment = "Add numbers"
    , tipe = Lambda (Var "number") (Lambda (Var "number") (Var "number"))
    , associativity = Left
    , precedence = 6
    }
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "name", Elm.Type.Type "String.String" [] )
                        , ( "comment", Elm.Type.Type "String.String" [] )
                        , ( "tipe", Elm.Type.Type "Elm.Type.Type" [] )
                        , ( "associativity", Elm.Type.Type "Elm.Docs.Associativity" [] )
                        , ( "precedence", Elm.Type.Type "Basics.Int" [] )
                        ]
                        Nothing
              }
            , { name = "Module"
              , args = []
              , comment = """ All the documentation for a particular module.

  * `name` is the module name
  * `comment` is the module comment

The actual exposed stuff is broken into categories.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "name", Elm.Type.Type "String.String" [] )
                        , ( "comment", Elm.Type.Type "String.String" [] )
                        , ( "unions", Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Docs.Union" [] ] )
                        , ( "aliases", Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Docs.Alias" [] ] )
                        , ( "values", Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Docs.Value" [] ] )
                        , ( "binops", Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Docs.Binop" [] ] )
                        ]
                        Nothing
              }
            , { name = "Union"
              , args = []
              , comment = """ Documentation for a union type. For example, if you had the source code:

    {-| maybe -}
    type Maybe a = Nothing | Just a

When it became a `Union` it would be like this:

    { name = "Maybe"
    , comment = " maybe "
    , args = ["a"]
    , tipe =
        [ ("Nothing", [])
        , ("Just", [Var "a"])
        ]
    }
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "name", Elm.Type.Type "String.String" [] )
                        , ( "comment", Elm.Type.Type "String.String" [] )
                        , ( "args", Elm.Type.Type "List.List" [ Elm.Type.Type "String.String" [] ] )
                        , ( "tags"
                          , Elm.Type.Type "List.List"
                                [ Elm.Type.Tuple
                                    [ Elm.Type.Type "String.String" []
                                    , Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Type.Type" [] ]
                                    ]
                                ]
                          )
                        ]
                        Nothing
              }
            , { name = "Value"
              , args = []
              , comment = """ Documentation for values and functions. For example, if you had the source
code:

    {-| do not do anything -}
    identity : a -> a
    identity value =
      value

The `Value` would look like this:

    { name = "identity"
    , comment = " do not do anything "
    , tipe = Lambda (Var "a") (Var "a")
    }
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "name", Elm.Type.Type "String.String" [] )
                        , ( "comment", Elm.Type.Type "String.String" [] )
                        , ( "tipe", Elm.Type.Type "Elm.Type.Type" [] )
                        ]
                        Nothing
              }
            ]
      , unions =
            [ { name = "Associativity"
              , args = []
              , comment = """ The [associativity][] of an infix operator. This determines how we add
parentheses around everything. Here are some examples:

    1 + 2 + 3 + 4

We have to do the operations in *some* order, so which of these interpretations
should we choose?

    ((1 + 2) + 3) + 4   -- left-associative
    1 + (2 + (3 + 4))   -- right-associative

This is really important for operators like `(|>)`!

Some operators are non-associative though, meaning we do not try to add
missing parentheses. `(==)` is a nice example. `1 == 2 == 3` just is not
allowed!

[associativity]: https://en.wikipedia.org/wiki/Operator_associativity

"""
              , tags =
                    [ ( "Left", [] )
                    , ( "None", [] )
                    , ( "Right", [] )
                    ]
              }
            , { name = "Block"
              , args = []
              , comment = """ This type represents a `Block` of documentation to show to the user.
After getting a `List Block` from `toBlocks`, everything is in the right order
and you can focus on turning the blocks into HTML exactly how you want.

**Note:** This should never produce an `UnknownBlock` but I figured it
would be better to let the block visualizer decide what to do in that case.
"""
              , tags =
                    [ ( "MarkdownBlock", [ Elm.Type.Type "String.String" [] ] )
                    , ( "UnionBlock", [ Elm.Type.Type "Elm.Docs.Union" [] ] )
                    , ( "AliasBlock", [ Elm.Type.Type "Elm.Docs.Alias" [] ] )
                    , ( "ValueBlock", [ Elm.Type.Type "Elm.Docs.Value" [] ] )
                    , ( "BinopBlock", [ Elm.Type.Type "Elm.Docs.Binop" [] ] )
                    , ( "UnknownBlock", [ Elm.Type.Type "String.String" [] ] )
                    ]
              }
            ]
      , binops = []
      , values =
            [ { name = "decoder"
              , comment = """ Decode the JSON documentation produced by `elm-make` for an individual
module. The documentation for a whole package is an array of module docs,
so you may need to say `(Decode.list Docs.decoder)` depending on what you
want to do.
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Docs.Module" [] ]
              }
            , { name = "toBlocks"
              , comment = """ The module comment describes exactly how the generated docs should look.
It is a mix of markdown and `@docs` declarations that specify when other
documentation should appear. Matching all this information up is somewhat
tricky though.

So calling `toBlocks` on a `Module` gives you a `List Block` with all the
information necessary to visualize the docs as intended.
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Docs.Module" []) (Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Docs.Block" [] ])
              }
            ]
      }
    , { name = "Elm.Error"
      , comment = """ When `elm make --report=json` fails, this module helps you turn the
resulting JSON into HTML.

# Compile Errors
@docs decoder, Error, BadModule, Problem

# Styled Text
@docs Chunk, Style, Color

# Code Regions
@docs Region, Position

"""
      , aliases =
            [ { name = "BadModule"
              , args = []
              , comment = """ When I cannot compile a module, I am able to report a bunch of problems at
once. So you may see a bunch of naming errors or type errors.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "path", Elm.Type.Type "String.String" [] )
                        , ( "name", Elm.Type.Type "String.String" [] )
                        , ( "problems", Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Error.Problem" [] ] )
                        ]
                        Nothing
              }
            , { name = "Position"
              , args = []
              , comment = """ A line and column in the source file. Both are one-indexed, so every file
starts at `{ line = 1, column = 1 }` and increases from there.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "line", Elm.Type.Type "Basics.Int" [] )
                        , ( "column", Elm.Type.Type "Basics.Int" [] )
                        ]
                        Nothing
              }
            , { name = "Problem"
              , args = []
              , comment = """ A problem in an Elm module.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "title", Elm.Type.Type "String.String" [] )
                        , ( "region", Elm.Type.Type "Elm.Error.Region" [] )
                        , ( "message", Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Error.Chunk" [] ] )
                        ]
                        Nothing
              }
            , { name = "Region"
              , args = []
              , comment = """ Every `Problem` is caused by code in a specific `Region`.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "start", Elm.Type.Type "Elm.Error.Position" [] )
                        , ( "end", Elm.Type.Type "Elm.Error.Position" [] )
                        ]
                        Nothing
              }
            , { name = "Style"
              , args = []
              , comment = """ Widely supported styles for ANSI text. Bold and underline are used very
rarely in Elm output. Mainly for a `Note` or a `Hint` about something. Colors
are used relatively infrequently, primarily to draw attention to the most
important information. Red is the problem, yellow is distilled advice, etc.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "bold", Elm.Type.Type "Basics.Bool" [] )
                        , ( "underline", Elm.Type.Type "Basics.Bool" [] )
                        , ( "color", Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "Elm.Error.Color" [] ] )
                        ]
                        Nothing
              }
            ]
      , unions =
            [ { name = "Chunk"
              , args = []
              , comment = """ A chunk of text to show. Chunks will contain newlines here and there, so
I recommend using `white-space: pre` to make sure everything looks alright.

The error messages are designed to look nice in 80 columns, and the only way
any line will be longer than that is if a code snippet from the user is longer.
Anyway, please try to get a presentation that matches the terminal pretty well.
It will look alright, and the consistency will be more valuable than any small
changes.
"""
              , tags =
                    [ ( "Unstyled", [ Elm.Type.Type "String.String" [] ] )
                    , ( "Styled"
                      , [ Elm.Type.Type "Elm.Error.Style" []
                        , Elm.Type.Type "String.String" []
                        ]
                      )
                    ]
              }
            , { name = "Color"
              , args = []
              , comment = """ Error messages use colors to emphasize the most useful information. This
helps people resolve their problems quicker! Because the errors need to work
on the terminal as well, the colors are limited to ANSI colors that are
widely supported by different terminal softwark.

So there are eight colors, each with a `Dull` and `VIVID` version.

**Note:** I have tried to make the _meaning_ of each color consistent across
all error messages (red is problem, yellow is decent advice, green is great
advice, cyan is helpful information, etc.) so please use colors that actually
match the color names! I think consistency is worth a lot within the ecosystem.
"""
              , tags =
                    [ ( "Red", [] )
                    , ( "RED", [] )
                    , ( "Magenta", [] )
                    , ( "MAGENTA", [] )
                    , ( "Yellow", [] )
                    , ( "YELLOW", [] )
                    , ( "Green", [] )
                    , ( "GREEN", [] )
                    , ( "Cyan", [] )
                    , ( "CYAN", [] )
                    , ( "Blue", [] )
                    , ( "BLUE", [] )
                    , ( "White", [] )
                    , ( "WHITE", [] )
                    , ( "Black", [] )
                    , ( "BLACK", [] )
                    ]
              }
            , { name = "Error"
              , args = []
              , comment = """ When `elm make --report=json` fails, there are two major categories of
error. Usually you have `ModuleProblems` like an unknown variable name or type
mismatch, but you can also get a `GeneralProblem` like cyclic modules or an
invalid `elm.json` file. The latter are much less common, but because they
never have a `Region` they need to be handled separately.
"""
              , tags =
                    [ ( "GeneralProblem"
                      , [ Elm.Type.Record
                            [ ( "path", Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "String.String" [] ] )
                            , ( "title", Elm.Type.Type "String.String" [] )
                            , ( "message", Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Error.Chunk" [] ] )
                            ]
                            Nothing
                        ]
                      )
                    , ( "ModuleProblems", [ Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Error.BadModule" [] ] ] )
                    ]
              }
            ]
      , binops = []
      , values =
            [ { name = "decoder"
              , comment = """ Decode the JSON produced when `elm make --report=json` fails. The goal is
to get the data in a format that can be presented in HTML.

**Note:** Please follow the design advice in the rest of the docs, like for
[`Chunk`](#Chunk) and [`Color`](#Color). Consistent presentation of errors
means that once you learn how to read errors, you have that ability with any
tool you use in Elm.
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Error.Error" [] ]
              }
            ]
      }
    , { name = "Elm.License"
      , comment = """ The `elm.json` for packages always has a `"license"` field. That field
must contain an OSI approved license in the [SPDX](https://spdx.org/licenses/) format.

This module helps verify that licenses are acceptable.


# Licenses
@docs License, bsd3, toDetails, osiApprovedSpdxLicenses

# String Conversions
@docs toString, fromString

# JSON Conversions
@docs encode, decoder
"""
      , aliases = []
      , unions =
            [ { name = "License"
              , args = []
              , comment = """ An OSI approved license in the [SPDX](https://spdx.org/licenses/) format.
It is impossible to construct an invalid `License` value.
"""
              , tags = []
              }
            ]
      , binops = []
      , values =
            [ { name = "bsd3"
              , comment = """ Easy access to a license commonly used in the Elm ecosystem.

  - `name` = `BSD 3-clause "New" or "Revised" License`
  - `spdx` = `BSD-3-Clause`

"""
              , tipe = Elm.Type.Type "Elm.License.License" []
              }
            , { name = "decoder"
              , comment = """ Decode a SPDX string from `elm.json` into a `License`
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.License.License" [] ]
              }
            , { name = "encode"
              , comment = """ Encode a `License` into a SPDX string for use in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.License.License" []) (Elm.Type.Type "Json.Encode.Value" [])
              }
            , { name = "fromString"
              , comment = """ Convert an arbitrary `String` into a `License`:

    fromString "BSD-3-Clause" == Just bsd3
    fromString "BSD3"         == Nothing

Notice that this function only succeds when given an OSI approved license
in its SPDX abbreviation. Go [here](https://spdx.org/licenses/) for a full
list of such licenses.
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "String.String" []) (Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "Elm.License.License" [] ])
              }
            , { name = "osiApprovedSpdxLicenses"
              , comment = """ OSI approved licenses in [SPDX format](https://spdx.org/licenses/).
"""
              , tipe = Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.License.License" [] ]
              }
            , { name = "toDetails"
              , comment = """ Extract the common `name` of a `License`, along with its standardized
`spdx` abbreviation.

    toDetails bsd3
    -- { name = "BSD 3-clause \\"New\\" or \\"Revised\\" License"
    -- , spdx = "BSD-3-Clause"
    -- }
"""
              , tipe =
                    Elm.Type.Lambda (Elm.Type.Type "Elm.License.License" [])
                        (Elm.Type.Record
                            [ ( "name", Elm.Type.Type "String.String" [] )
                            , ( "spdx", Elm.Type.Type "String.String" [] )
                            ]
                            Nothing
                        )
              }
            , { name = "toString"
              , comment = """ Convert a `License` to its SPDX abbreviation:

    toString bsd3 == "BSD-3-Clause"
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.License.License" []) (Elm.Type.Type "String.String" [])
              }
            ]
      }
    , { name = "Elm.Module"
      , comment = """ Helpers for working with module name strings in `elm.json` files.

# Modules
@docs Name

# String Conversions
@docs toString, fromString

# JSON Conversions
@docs encode, decoder

"""
      , aliases = []
      , unions =
            [ { name = "Name"
              , args = []
              , comment = """ A guaranteed valid Elm module name.
"""
              , tags = []
              }
            ]
      , binops = []
      , values =
            [ { name = "decoder"
              , comment = """ Decode the module name strings that appear in `elm.json`
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Module.Name" [] ]
              }
            , { name = "encode"
              , comment = """ Turn a `Name` into a string for use in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Module.Name" []) (Elm.Type.Type "Json.Encode.Value" [])
              }
            , { name = "fromString"
              , comment = """ Try to convert a `String` into a `Name`:

    fromString "Maybe"       == Just ...
    fromString "Elm.Name"  == Just ...
    fromString "Json.Decode" == Just ...
    fromString "json.decode" == Nothing
    fromString "Json_Decode" == Nothing
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "String.String" []) (Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "Elm.Module.Name" [] ])
              }
            , { name = "toString"
              , comment = """ Convert a `Name` to a `String` that works in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Module.Name" []) (Elm.Type.Type "String.String" [])
              }
            ]
      }
    , { name = "Elm.Package"
      , comment = """ Helpers for working with package name strings in `elm.json` files.

# Packages
@docs Name

# String Conversions
@docs toString, fromString

# JSON Conversions
@docs encode, decoder

"""
      , aliases = []
      , unions =
            [ { name = "Name"
              , args = []
              , comment = """ A guaranteed valid Elm package name.
"""
              , tags = []
              }
            ]
      , binops = []
      , values =
            [ { name = "decoder"
              , comment = """ Decode the module name strings that appear in `elm.json`
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Package.Name" [] ]
              }
            , { name = "encode"
              , comment = """ Turn a `Name` into a string for use in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Package.Name" []) (Elm.Type.Type "Json.Encode.Value" [])
              }
            , { name = "fromString"
              , comment = """ Try to convert a `String` into a `Name`:

    fromString "elm/core"    == Just ...
    fromString "elm/html"    == Just ...
    fromString "tom/elm-css" == Just ...
    fromString "tom/elm_css" == Nothing
    fromString "tom/x.js"    == Nothing
    fromString "elm"         == Nothing
    fromString "html"        == Nothing
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "String.String" []) (Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "Elm.Package.Name" [] ])
              }
            , { name = "toString"
              , comment = """ Convert a `Name` to a `String` that works in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Package.Name" []) (Elm.Type.Type "String.String" [])
              }
            ]
      }
    , { name = "Elm.Project"
      , comment = """ Turn `elm.json` files into data that is nice to use in Elm.

# Projects
@docs Project, ApplicationInfo, Deps, PackageInfo, Exposed

# JSON Conversions
@docs encode, decoder

"""
      , aliases =
            [ { name = "ApplicationInfo"
              , args = []
              , comment = """ The contents of an `elm.json` with `"type": "application"`.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "elm", Elm.Type.Type "Elm.Version.Version" [] )
                        , ( "dirs", Elm.Type.Type "List.List" [ Elm.Type.Type "String.String" [] ] )
                        , ( "depsDirect", Elm.Type.Type "Elm.Project.Deps" [ Elm.Type.Type "Elm.Version.Version" [] ] )
                        , ( "depsIndirect", Elm.Type.Type "Elm.Project.Deps" [ Elm.Type.Type "Elm.Version.Version" [] ] )
                        , ( "testDepsDirect", Elm.Type.Type "Elm.Project.Deps" [ Elm.Type.Type "Elm.Version.Version" [] ] )
                        , ( "testDepsIndirect", Elm.Type.Type "Elm.Project.Deps" [ Elm.Type.Type "Elm.Version.Version" [] ] )
                        ]
                        Nothing
              }
            , { name = "Deps"
              , args = [ "constraint" ]
              , comment = """ The dependencies for a project. The order is preserved from JSON.
"""
              , tipe =
                    Elm.Type.Type "List.List"
                        [ Elm.Type.Tuple
                            [ Elm.Type.Type "Elm.Package.Name" []
                            , Elm.Type.Var "constraint"
                            ]
                        ]
              }
            , { name = "PackageInfo"
              , args = []
              , comment = """ The contents of an `elm.json` with `"type": "package"`.
"""
              , tipe =
                    Elm.Type.Record
                        [ ( "name", Elm.Type.Type "Elm.Package.Name" [] )
                        , ( "summary", Elm.Type.Type "String.String" [] )
                        , ( "license", Elm.Type.Type "Elm.License.License" [] )
                        , ( "version", Elm.Type.Type "Elm.Version.Version" [] )
                        , ( "exposed", Elm.Type.Type "Elm.Project.Exposed" [] )
                        , ( "deps", Elm.Type.Type "Elm.Project.Deps" [ Elm.Type.Type "Elm.Constraint.Constraint" [] ] )
                        , ( "testDeps", Elm.Type.Type "Elm.Project.Deps" [ Elm.Type.Type "Elm.Constraint.Constraint" [] ] )
                        , ( "elm", Elm.Type.Type "Elm.Constraint.Constraint" [] )
                        ]
                        Nothing
              }
            ]
      , unions =
            [ { name = "Exposed"
              , args = []
              , comment = """ There are two ways to specify `"exposed-modules"` field in an `elm.json`
for packages. In one you just list the exposed modules. In the other, you
provide headers for chunks of module names. In either case, the package website
preserves this information to make the presentation nicer.
"""
              , tags =
                    [ ( "ExposedList", [ Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Module.Name" [] ] ] )
                    , ( "ExposedDict"
                      , [ Elm.Type.Type "List.List"
                            [ Elm.Type.Tuple
                                [ Elm.Type.Type "String.String" []
                                , Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Module.Name" [] ]
                                ]
                            ]
                        ]
                      )
                    ]
              }
            , { name = "Project"
              , args = []
              , comment = """ There are two types of Elm projects, one for applications and another one
for packages. The `elm.json` is different in each case, so we they are modeled
as [`ApplicationInfo`](#ApplicationInfo) and [`PackageInfo`](#PackageInfo) types.
"""
              , tags =
                    [ ( "Application", [ Elm.Type.Type "Elm.Project.ApplicationInfo" [] ] )
                    , ( "Package", [ Elm.Type.Type "Elm.Project.PackageInfo" [] ] )
                    ]
              }
            ]
      , binops = []
      , values =
            [ { name = "decoder"
              , comment = """ Decode the contents of `elm.json` into a `Project`.
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Project.Project" [] ]
              }
            , { name = "encode"
              , comment = """ Turn a `Project` into the JSON that goes in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Project.Project" []) (Elm.Type.Type "Json.Encode.Value" [])
              }
            ]
      }
    , { name = "Elm.Type"
      , comment = """ This is specifically for handling the types that appear in
documentation generated by `elm-make`. If you are looking to parse
arbitrary type signatures with creative indentation (e.g. newlines
and comments) this library will not do what you want. Instead,
check out the source code and go from there. It's not too tough!

@docs Type, decoder

"""
      , aliases = []
      , unions =
            [ { name = "Type"
              , args = []
              , comment = """ Represent Elm types as values! Here are some examples:

    Int            ==> Type "Int" []

    a -> b         ==> Lambda (Var "a") (Var "b")

    ( a, b )       ==> Tuple [ Var "a", Var "b" ]

    Maybe a        ==> Type "Maybe" [ Var "a" ]

    { x : Float }  ==> Record [("x", Type "Float" [])] Nothing
"""
              , tags =
                    [ ( "Var", [ Elm.Type.Type "String.String" [] ] )
                    , ( "Lambda"
                      , [ Elm.Type.Type "Elm.Type.Type" []
                        , Elm.Type.Type "Elm.Type.Type" []
                        ]
                      )
                    , ( "Tuple", [ Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Type.Type" [] ] ] )
                    , ( "Type"
                      , [ Elm.Type.Type "String.String" []
                        , Elm.Type.Type "List.List" [ Elm.Type.Type "Elm.Type.Type" [] ]
                        ]
                      )
                    , ( "Record"
                      , [ Elm.Type.Type "List.List"
                            [ Elm.Type.Tuple
                                [ Elm.Type.Type "String.String" []
                                , Elm.Type.Type "Elm.Type.Type" []
                                ]
                            ]
                        , Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "String.String" [] ]
                        ]
                      )
                    ]
              }
            ]
      , binops = []
      , values =
            [ { name = "decoder"
              , comment = """ Decode the JSON representation of `Type` values.
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Type.Type" [] ]
              }
            ]
      }
    , { name = "Elm.Version"
      , comment = """ Helpers for working with version strings in `elm.json` files.

# Versions
@docs Version, one, compare

# String Conversions
@docs toString, fromString

# JSON Conversions
@docs encode, decoder

# Tuple Conversions
@docs toTuple, fromTuple

"""
      , aliases = []
      , unions =
            [ { name = "Version"
              , args = []
              , comment = """ A guaranteed valid Elm version. All versions are `1.0.0` or greater.
"""
              , tags = []
              }
            ]
      , binops = []
      , values =
            [ { name = "compare"
              , comment = """ Compare two versions:

    v1 = fromString "1.0.0"
    v2 = fromString "2.0.0"
    v3 = fromString "3.0.0"

    -- Maybe.map2 compare v1 v2 == Just LT
    -- Maybe.map2 compare v2 v2 == Just EQ
    -- Maybe.map2 compare v2 v1 == Just GT

"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Version.Version" []) (Elm.Type.Lambda (Elm.Type.Type "Elm.Version.Version" []) (Elm.Type.Type "Basics.Order" []))
              }
            , { name = "decoder"
              , comment = """ Decode the version strings that appear in `elm.json`
"""
              , tipe = Elm.Type.Type "Json.Decode.Decoder" [ Elm.Type.Type "Elm.Version.Version" [] ]
              }
            , { name = "encode"
              , comment = """ Turn a `Version` into a string for use in `elm.json`
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Version.Version" []) (Elm.Type.Type "Json.Encode.Value" [])
              }
            , { name = "fromString"
              , comment = """ Try to convert a `String` into a `Version`. The major, minor, and patch
numbers must all appear separated by dots:

    fromString "1.0.0" == Just one
    fromString "2.0.0" == Just ...
    fromString "3-0-0" == Nothing
    fromString "3.0"   == Nothing
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "String.String" []) (Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "Elm.Version.Version" [] ])
              }
            , { name = "fromTuple"
              , comment = """ Try to make a `Version` from given numbers. This way you do not need
to turn things into strings for no reason. It can still fail if you give
negative numbers or versions below `1.0.0`:

    fromTuple (1, 0, 0) == Just one
    fromTuple (2, 0, 1) == Just ...
    fromTuple (0, 0, 1) == Nothing
"""
              , tipe =
                    Elm.Type.Lambda
                        (Elm.Type.Tuple
                            [ Elm.Type.Type "Basics.Int" []
                            , Elm.Type.Type "Basics.Int" []
                            , Elm.Type.Type "Basics.Int" []
                            ]
                        )
                        (Elm.Type.Type "Maybe.Maybe" [ Elm.Type.Type "Elm.Version.Version" [] ])
              }
            , { name = "one"
              , comment = """ Version `1.0.0` for easy access.
"""
              , tipe = Elm.Type.Type "Elm.Version.Version" []
              }
            , { name = "toString"
              , comment = """ Convert a `Version` to a `String` that works in `elm.json`

    toString one == "1.0.0"
"""
              , tipe = Elm.Type.Lambda (Elm.Type.Type "Elm.Version.Version" []) (Elm.Type.Type "String.String" [])
              }
            , { name = "toTuple"
              , comment = """ Turn a `Version` into a tuple to extract the numbers as integers.

    toTuple one == (1, 0, 0)

    Maybe.map toTuple (fromString "2.0.4" ) == Just (2, 0, 4)
    Maybe.map toTuple (fromString "7.3.10") == Just (7, 3, 10)
"""
              , tipe =
                    Elm.Type.Lambda (Elm.Type.Type "Elm.Version.Version" [])
                        (Elm.Type.Tuple
                            [ Elm.Type.Type "Basics.Int" []
                            , Elm.Type.Type "Basics.Int" []
                            , Elm.Type.Type "Basics.Int" []
                            ]
                        )
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
