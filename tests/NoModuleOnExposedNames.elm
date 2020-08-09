module NoModuleOnExposedNames exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import NoModuleOnExposedNames.Context as Context
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import Vendor.NameVisitor as NameVisitor


{-| Forbid modules on names that have been exposed.

    config : List Rule
    config =
        [ NoModuleOnExposedNames.rule
        ]

If a function or type has been exposed on import do not allow it to be called via the module name/alias. You should either remove the call from the import exposes or remove the module name from the call.


## Failure

Here `class` is exposed but is still called by `Attr.class`.

    import Html.Attributes as Attr exposing (class)

    view children =
        div [ Attr.class "container" ] children


## Success

Remove the module name from the call:

    import Html.Attributes as Attr exposing (class)

    view children =
        div [ class "container" ] children


## Success

Or remove `class` from `exposing`:

    import Html.Attributes as Attr

    view children =
        div [ Attr.class "container" ] children


## Failure

Here `Attribute` has been exposed but is still called by `Html.Attribute`.

    import Html exposing (Attribute, Html)

    container : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    container attributes children =
        div attributes children


## Success

Remove the module name from the call:

    import Html exposing (Attribute, Html)

    container : List (Attribute msg) -> List (Html msg) -> Html msg
    container attributes children =
        div attributes children


## Success

Or remove `Attribute` from `exposing`:

    import Html exposing (Html)

    container : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    container attributes children =
        div attributes children

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoModuleOnExposedNames" Context.initial
        |> Rule.withImportVisitor importVisitor
        |> NameVisitor.withValueAndTypeVisitors
            { valueVisitor = valueVisitor
            , typeVisitor = typeVisitor
            }
        |> Rule.fromModuleRuleSchema


importVisitor : Node Import -> Context.Module -> ( List (Error {}), Context.Module )
importVisitor node context =
    ( [], context |> rememberExposedNames (Node.value node) )


rememberExposedNames : Import -> Context.Module -> Context.Module
rememberExposedNames { moduleName, moduleAlias, exposingList } context =
    case exposingList of
        Nothing ->
            context

        Just exposes ->
            let
                moduleNameOrAlias =
                    moduleAlias
                        |> Maybe.map Node.value
                        |> Maybe.withDefault (Node.value moduleName)
            in
            context |> Context.expose moduleNameOrAlias (Node.value exposes)


valueVisitor : Node ( ModuleName, String ) -> Context.Module -> ( List (Error {}), Context.Module )
valueVisitor node context =
    case Node.value node of
        ( moduleName, name ) ->
            if Context.isFunctionExposed context moduleName name then
                ( [ moduleOnExposedValueError name (Node.range node) ]
                , context
                )

            else
                ( [], context )


typeVisitor : Node ( ModuleName, String ) -> Context.Module -> ( List (Error {}), Context.Module )
typeVisitor node context =
    case Node.value node of
        ( moduleName, name ) ->
            if Context.isTypeExposed context moduleName name then
                ( [ moduleOnExposedTypeError name (Node.range node) ]
                , context
                )

            else
                ( [], context )


moduleOnExposedValueError : String -> Range -> Error {}
moduleOnExposedValueError name range =
    Rule.errorWithFix
        { message = "Module used on exposed value `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        }
        range
        [ Fix.replaceRangeBy range name ]


moduleOnExposedTypeError : String -> Range -> Error {}
moduleOnExposedTypeError name range =
    Rule.errorWithFix
        { message = "Module used on exposed type `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        }
        range
        [ Fix.replaceRangeBy range name ]
