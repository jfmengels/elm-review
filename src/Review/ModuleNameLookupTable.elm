module Review.ModuleNameLookupTable exposing
    ( ModuleNameLookupTable, moduleNameFor, moduleNameAt
    , fullModuleNameFor, fullModuleNameAt
    , createForTests
    )

{-| Looks up the name of the module a function or type comes from based on the position of the element in the module's AST.

When encountering a `Expression.FunctionOrValue ModuleName String` (among other nodes where we refer to a function or value),
the module name available represents the module name that is in the source code. But that module name can be an alias to
a different import, or it can be empty, meaning that it refers to a local value or one that has been imported explicitly
or implicitly. Resolving which module the type or function comes from can be a bit tricky sometimes, and I recommend against
doing it yourself.

`elm-review` computes this for you already. Request the `ModuleNameLookupTable` using
[`Review.Rule.withModuleNameLookupTable`](./Review-Rule#withModuleNameLookupTable)
and store it inside the module context, then use
[`ModuleNameLookupTable.moduleNameFor`](./Review-ModuleNameLookupTable#moduleNameFor) or
[`ModuleNameLookupTable.moduleNameAt`](./Review-ModuleNameLookupTable#moduleNameAt) to get the name of the module the
type or value comes from.

@docs ModuleNameLookupTable, moduleNameFor, moduleNameAt
@docs fullModuleNameFor, fullModuleNameAt


## Testing

@docs createForTests

-}

import Dict
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable.Internal as Internal


{-| Associates positions in the AST of a module to the name of the module that the contained variable or type originates
from.
-}
type alias ModuleNameLookupTable =
    Internal.ModuleNameLookupTable


{-| Returns the name of the module that defined the type, value, or operator referred to by this [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#Node).

The function returns `Just []` if the type or value was defined in this module. It returns `Just moduleName` if the Node is among these kinds of AST nodes (and `Nothing` for all the others):

  - `Expression.FunctionOrValue`
  - `nodeForTheName` in `Expression.RecordUpdateExpression nodeForTheName modifiers`
  - `nodeForTheName` in `TypeAnnotation.Typed nodeForTheName args`
  - `Pattern.NamedPattern`
  - `Expression.PrefixOperator`
  - `Expression.OperatorApplication`

```elm
expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue _ "color" ->
            if ModuleNameLookupTable.moduleNameFor context.lookupTable node == Just [ "Css" ] then
                ( [ Rule.error
                        { message = "Do not use `Css.color` directly, use the Colors module instead"
                        , details = [ "We made a module which contains all the available colors of our design system. Use the functions in there instead." ]
                        }
                        (Node.range node)
                  ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )
```

Note: If using a `Range` is easier in your situation than using a `Node`, use [`moduleNameAt`](#moduleNameAt) instead.

-}
moduleNameFor : ModuleNameLookupTable -> Node a -> Maybe ModuleName
moduleNameFor (Internal.ModuleNameLookupTable _ dict) (Node range _) =
    Dict.get (Internal.toRangeLike range) dict


{-| This is the same as [`moduleNameFor`](#moduleNameFor), except that the function will return the current module name
if the type or value was defined in this module, instead of `Just []`.
-}
fullModuleNameFor : ModuleNameLookupTable -> Node a -> Maybe ModuleName
fullModuleNameFor (Internal.ModuleNameLookupTable currentModuleName dict) (Node range _) =
    case Dict.get (Internal.toRangeLike range) dict of
        Just [] ->
            Just currentModuleName

        res ->
            res


{-| Returns the name of the module that defined the type, value, or operator referred to by this [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range#Range).

The function returns `Just []` if the type or value was defined in this module. It returns `Just moduleName` if the Node is among these kinds of AST nodes (and `Nothing` for all the others):

  - `Expression.FunctionOrValue`
  - `nodeForTheName` in `Expression.RecordUpdateExpression nodeForTheName modifiers`
  - `nodeForTheName` in `TypeAnnotation.Typed nodeForTheName args`
  - `Pattern.NamedPattern`
  - `Expression.PrefixOperator`
  - `Expression.OperatorApplication`

```elm
expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.RecordUpdateExpr (Node range name) _ ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just moduleName ->
                    ( [], markVariableAsUsed ( moduleName, name ) context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )
```

Note: If using a `Node` is easier in your situation than using a `Range`, use [`moduleNameFor`](#moduleNameFor) instead.

-}
moduleNameAt : ModuleNameLookupTable -> Range -> Maybe ModuleName
moduleNameAt (Internal.ModuleNameLookupTable _ dict) range =
    Dict.get (Internal.toRangeLike range) dict


{-| This is the same as [`moduleNameAt`](#moduleNameAt), except that the function will return the current module name
if the type or value was defined in this module, instead of `Just []`.
-}
fullModuleNameAt : ModuleNameLookupTable -> Range -> Maybe ModuleName
fullModuleNameAt (Internal.ModuleNameLookupTable currentModuleName dict) range =
    case Dict.get (Internal.toRangeLike range) dict of
        Just [] ->
            Just currentModuleName

        res ->
            res


{-| Creates a module name lookup table from a list of ranges and module names. The first argument is the name of the
module in which this lookup table would be used.

**NOTE**: This is only meant to be used for testing purposes, not for direct use in rules.

This can be useful if you want to test individual functions that take a `ModuleNameLookupTable` as an argument.

    ModuleNameLookupTable.createForTests
        [ "My", "Module" ]
        []

-}
createForTests : ModuleName -> List ( Range, ModuleName ) -> ModuleNameLookupTable
createForTests =
    Internal.fromList
