module Lint.Rule.NoImportingEverything exposing (rule, Configuration)

{-| Forbid importing everything from a module.


# Rule and configuration

@docs rule, Configuration

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Lint.Rule as Rule exposing (Error, Rule)
import Lint.Util as Util


{-| Configuration for the rule.
-}
type alias Configuration =
    { exceptions : List String }


{-| Forbid importing everything from a module. Doing so can be confusing,
especially to newcomers when the exposed functions and types are unknown to them.

A preferred pattern is to import functions by name (`import Html exposing (div, span)`)
or using qualified imports (`import Html`, then `Html.div`). If the module name
is too long, don't forget that you can do qualified imports using an alias
(`import Html.Attributes as Attr`).

You can make exceptions for some modules by adding them to the `exceptions`
field, like `{ exceptions = [ "Html", "Html.Attributes" ] }`. The name should be
the exact name of the import. Allowing importing everything from `Html` will not
allow the same thing for `Html.Events`, unless explicitly specified.

    config =
        [ ( Critical, NoImportingEverything.rule { exceptions = [] }
        ]


## Fail

    import Html exposing (..)


## Success

    -- ( Critical, NoImportingEverything.rule { exceptions = [] }
    import Html exposing (div, p, textarea)

    -- ( Critical, NoImportingEverything.rule { exceptions = [ "Html" ] }
    import Html exposing (..)


# When not to use this rule

If you prefer importing most of your modules using `exposing (..)`, then you
should not use this rule.

-}
rule : Configuration -> Rule
rule config =
    Rule.newSchema "NoImportingEverything"
        |> Rule.withSimpleImportVisitor (importVisitor config)
        |> Rule.fromSchema


error : Range -> String -> Error
error range name =
    Rule.error ("Do not expose everything from " ++ name) range


importVisitor : Configuration -> Node Import -> List Error
importVisitor config node =
    let
        { moduleName, exposingList } =
            Node.value node

        name : String
        name =
            Util.moduleName moduleName
    in
    if List.member name config.exceptions then
        []

    else
        case exposingList |> Maybe.map Node.value of
            Just (Exposing.All range) ->
                [ error range name ]

            _ ->
                []
