module Lint.Rule.NoImportingEverything exposing (rule, Configuration)

{-| Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed
functions and types are unknown to them.


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


{-| Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed
functions and types are unknown to them.

    config =
        [ ( Critical, NoImportingEverything.rule { exceptions = [] }
        ]

You can make exceptions for some modules by adding them to the `exceptions`
field, like `{ exceptions = [ "Html", "Html.Attributes" ] }`. The name should be
the exact name of the import. Allowing importing everything from `Html` will not
allow the same thing for `Html.Events`, unless explicitly specified.


## Fail

    import Html exposing (..)


## Success

    -- ( Critical, NoImportingEverything.rule { exceptions = [] }
    import Html exposing (div, p, textarea)

    -- ( Critical, NoImportingEverything.rule { exceptions = [ "Html" ] }
    import Html exposing (..)

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
