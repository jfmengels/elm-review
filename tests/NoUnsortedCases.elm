module NoUnsortedCases exposing
    ( rule
    , RuleConfig, defaults, sortOnlyMatchingTypes, doNotSortLiterals, doNotSortTypesFromDependencies, sortTypesFromDependenciesAlphabetically, sortListPatternsByLength, doNotLookPastUnsortable
    )

{-|


## Review Rule

@docs rule


## Configuration

@docs RuleConfig, defaults, sortOnlyMatchingTypes, doNotSortLiterals, doNotSortTypesFromDependencies, sortTypesFromDependenciesAlphabetically, sortListPatternsByLength, doNotLookPastUnsortable

-}

import Dict exposing (Dict)
import Dict.Extra as DictX
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable, moduleNameFor)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Util exposing (checkSorting, fallbackCompareFor, fallbackCompareWithUnsortableFor)


{-| Reports case patterns that are not in the "proper" order.

🔧 Running with `--fix` will automatically sort the patterns.

The proper order of custom types is the order in which they are defined in your
source files, and the order of other patterns may be specified in the rule
configuration. See the [Configuration](#configuration) section below for more
information.

    config =
        [ NoUnsortedCases.rule NoUnsortedCases.defaults
        ]


## Fail

    type Custom
        = Foo
        | Bar
        | Baz

    func1 c =
        case c of
            Bar ->
                "bar"

            Foo ->
                "foo"

            Baz ->
                "baz"

    func2 cs =
        case cs of
            [ Bar ] ->
                "bar"

            [ Foo ] ->
                "foo"

            [ Foo, Foo ] ->
                "foofoo"

            [ Baz ] ->
                "baz"

            _ ->
                "other"

    func3 c =
        case c of
            Nothing ->
                ""

            Just Bar ->
                "bar"

            Just Foo ->
                "foo"

            Just Baz ->
                "baz"

    func4 c1 c2 =
        case ( c1, c2 ) of
            ( Foo, Baz ) ->
                "foo baz"

            ( Foo, Bar ) ->
                "foo bar"

            ( Bar, Foo ) ->
                "bar foo"

            ( Baz, Foo ) ->
                "baz foo"

            _ ->
                "other"


## Success

    type Custom
        = Foo
        | Bar
        | Baz

    func1 c =
        case c of
            Foo ->
                "foo"

            Bar ->
                "bar"

            Baz ->
                "baz"

    func2 cs =
        case cs of
            [ Foo ] ->
                "foo"

            [ Foo, Foo ] ->
                "foofoo"

            [ Bar ] ->
                "bar"

            [ Baz ] ->
                "baz"

            _ ->
                "other"

    func3 c =
        case c of
            Just Foo ->
                "foo"

            Just Bar ->
                "bar"

            Just Baz ->
                "baz"

            Nothing ->
                ""

    func4 c1 c2 =
        case ( c1, c2 ) of
            ( Foo, Bar ) ->
                "foo bar"

            ( Foo, Baz ) ->
                "foo baz"

            ( Bar, Foo ) ->
                "bar foo"

            ( Baz, Foo ) ->
                "baz foo"

            _ ->
                "other"


## When (not) to enable this rule

This rule is useful when you want to ensure that you pattern match in a
consistent, predictable order, that is consistent with the order in which a type
was defined, as well as ensuring (optionally) that literal patterns and the like
are sorted.

This rule is not useful when you want to be able to write case patterns in
different orders throughout your codebase, e.g. if you want to emphasize what
pattern is most important at any given point or glean a tiny bit of performance
out of matching the more commonly-expected patterns first.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example --rules NoUnsortedCases
```

-}
rule : RuleConfig -> Rule
rule config =
    Rule.newProjectRuleSchema "NoUnsortedCases" initialProjectContext
        |> Rule.withDependenciesProjectVisitor (\d c -> ( [], dependencyVisitor config d c ))
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule config
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


{-| Configuration for this rule. Create a new one with `defaults` and use
`doNotSortLiterals`, `sortListPatternsByLength`, etc. to alter it.
-}
type RuleConfig
    = RuleConfig
        { lookPastUnsortable : Bool
        , sortLists : SortLists
        , sortLiterals : Bool
        , sortTypesFromDependencies : SortTypesFromDependencies
        , sortablePredicate : String -> String -> Bool
        }


{-| List patterns may be sorted in one of two ways:

  - `Elementwise` -- Patterns are sorted by comparing elements sequentially at each position (from left to right). This is the same behavior as
    `List.sort`.
  - `LengthFirst` -- Shorter patterns always come before longer pattern, with patterns of the same length sorted elementwise at each position.

-}
type SortLists
    = Elementwise
    | LengthFirst


{-| Specify how to sort types that are **imported from dependencies**.

  - `DeclarationOrder` -- Sort types in the order they appear in the
    dependency's source file (or more technically in its documentation); this is
    identical to the behavior of types defined within your own modules.
  - `AlphabeticalOrder` -- Sort types alphabetically.
  - `DoNotSort` -- Do not sort types from dependencies at all. Note that this
    will render unsortable any patterns requiring types from dependencies to be
    sorted.

-}
type SortTypesFromDependencies
    = DeclarationOrder
    | AlphabeticalOrder
    | DoNotSort


{-| The default configuration, with the following behavior:

  - All custom types are sorted. (This can be restricted by using
    `sortOnlyMatchingTypes`.)

  - Literal patterns (`String`, `Int`, etc.) are sorted in the natural order for their type.

  - Types imported from dependencies are sorted in declaration order, i.e. in the order they appear in the dependency's source file (or more technically in its documentation); this is identical to the behavior of types defined within your own modules.

  - Lists are sorted elementwise, by comparing the elements sequentially at each
    position (from left to right).

  - Unsortable patterns can be looked beyond to resolve ties, for example:

```
func x =
    case x of
        T () Bar ->
            1

        T () Baz ->
            2

        T () Foo ->
            3
```

will be sorted to

    func x =
        case x of
            T () Foo ->
                3

            T () Bar ->
                1

            T () Baz ->
                2

Use `doNotSortLiterals`, `sortListPatternsByLength`, etc. to alter any of this
behavior, e.g.

    config =
        [ NoUnsortedCases.defaults
            |> NoUnsortedCases.doNotSortLiterals
            |> NoUnsortedCases.sortListPatternsByLength
            |> NoUnsortedCases.rule
        ]

-}
defaults : RuleConfig
defaults =
    RuleConfig
        { lookPastUnsortable = True
        , sortLists = Elementwise
        , sortLiterals = True
        , sortTypesFromDependencies = DeclarationOrder
        , sortablePredicate = \_ _ -> True
        }


{-| Restrict custom type sorting to only those matching a provided predicate.
This function takes two strings, the first being the full module name of a type,
e.g. `"Review.Rule"` and the second being the name of a type, e.g. `"Rule"`, and
returns a `Bool` indicating whether the type should be sorted (with `True`
meaning sortable). For example:

Module Foo:

    module Foo exposing (Foo(..))

    type Foo
        = Foo
        | Bar
        | Baz

Module Main:

    module Main exposing (..)

    type Msg
        = ButtonPressed
        | ButtonClicked

Module ReviewConfig:

    onlyMsg moduleName typeName =
        case ( moduleName, typeName ) of
            ( "Main", "Msg" ) ->
                True

            _ ->
                False

    config =
        [ NoUnsortedCases.defaults
            |> NoUnsortedCases.sortOnlyMatchingTypes onlyMsg
            |> NoUnsortedCases.rule
        ]

will sort the following pattern:

    case msg of
        ButtonClicked ->
            ( { model | clicked = True }, Cmd.none )

        ButtonPressed ->
            ( { model | pressed = True }, Cmd.none )

but will not sort:

    case foo of
        Bar ->
            "bar"

        Baz ->
            "baz"

        Foo ->
            "foo"

-}
sortOnlyMatchingTypes : (String -> String -> Bool) -> RuleConfig -> RuleConfig
sortOnlyMatchingTypes sortablePredicate (RuleConfig c) =
    RuleConfig { c | sortablePredicate = sortablePredicate }


{-| Change the behavior of the rule to **not** sort literal patterns. If
literals are not sorted, case expressions that would require sorting literals
cannot be sorted and will thus be ignored by the rule.
-}
doNotSortLiterals : RuleConfig -> RuleConfig
doNotSortLiterals (RuleConfig c) =
    RuleConfig { c | sortLiterals = False }


{-| List patterns may be sorted in one of two ways:

  - Elementwise (**default**) -- Patterns are sorted by comparing elements
    sequentially at each position (from left to right). This is the same
    behavior as `List.sort` (which is why it is the default).
  - Length First -- Shorter patterns always come before longer pattern, with patterns of the same length sorted elementwise at each position.

Note that uncons patterns are considered the length of their matching list, with
wildcard patterns considered to have infinite length for the purposes of
sorting. This is necessary to ensure that earlier patterns are not erroneously
matched by wildcards.

**Elementwise**

    case list of
        [] ->
            ""

        [ 1 ] ->
            "1"

        [ 1, 1 ] ->
            "11"

        [ 1, 1, 1 ] ->
            "111"

        [ 1, 2 ] ->
            "12"

        [ 1, 3 ] ->
            "13"

        [ 2 ] ->
            "2"

        [ 2, 1 ] ->
            "21"

        [ 2, 2 ] ->
            "22"

        [ 2, 3 ] ->
            "23"

        [ 3 ] ->
            "3"

        _ ->
            "Too many..."

**Length First**

    case list of
        [] ->
            ""

        [ 1 ] ->
            "1"

        [ 2 ] ->
            "2"

        [ 3 ] ->
            "3"

        [ 1, 1 ] ->
            "11"

        [ 1, 2 ] ->
            "12"

        [ 1, 3 ] ->
            "13"

        [ 2, 1 ] ->
            "21"

        [ 2, 2 ] ->
            "22"

        [ 2, 3 ] ->
            "23"

        [ 1, 1, 1 ] ->
            "111"

        _ ->
            "Too many..."

-}
sortListPatternsByLength : RuleConfig -> RuleConfig
sortListPatternsByLength (RuleConfig c) =
    RuleConfig { c | sortLists = LengthFirst }


{-| Sort custom types imported from dependencies (including `Basics` types like `Maybe` and `Bool`) alphabetically, rather than by their source order in the dependency's source code.
-}
sortTypesFromDependenciesAlphabetically : RuleConfig -> RuleConfig
sortTypesFromDependenciesAlphabetically (RuleConfig c) =
    RuleConfig { c | sortTypesFromDependencies = AlphabeticalOrder }


{-| Do not sort types from dependencies at all. Note that this will render
unsortable any patterns requiring types from dependencies to be sorted.
-}
doNotSortTypesFromDependencies : RuleConfig -> RuleConfig
doNotSortTypesFromDependencies (RuleConfig c) =
    RuleConfig { c | sortTypesFromDependencies = DoNotSort }


{-| Do not look beyond unsortable patterns, i.e. do not tiebreak cases with an
unsortable sub-pattern by the next sub-pattern.

For example, given this:

    type X
        = A
        | B () Int

    f x =
        case x of
            B () 2 ->
                1

            B () 1 ->
                2

            A ->
                3

By **default**, this will be sorted to:

    case x of
        A ->
            3

        -- v The rule sorted these patterns because even though it can't compare the (), 1 comes before 2
        B () 1 ->
            2

        B () 2 ->
            1

With `doNotLookPastUnsortable`, however, the two `B` patterns will be considered
unsortable, so it will instead be sorted to this:

    case x of
        A ->
            3

        -- v Comparison stopped for these patterns because the rule didn't look beyond the ()
        B () 2 ->
            1

        B () 1 ->
            2

Note that `A` is sorted above `B` in both cases because it did not require
comparing the unsortable `()` pattern.

_It's not clear why you'd ever want to use this, so it will likely be removed in
a future major version. Please let me know if you actually find it useful!_

-}
doNotLookPastUnsortable : RuleConfig -> RuleConfig
doNotLookPastUnsortable (RuleConfig c) =
    RuleConfig { c | lookPastUnsortable = False }



-- * Types


{-| The project context, consisting of a map from module names to a map of type
names to orders.
-}
type alias ProjectContext =
    { customTypes :
        Dict
            ModuleName
            (Dict
                String
                { constructors : Set String
                , declarationOrder : List String
                }
            )
    }


{-| The module context, consisting of a map from module names to a map of type
names to orders.

  - `customTypes` -- Orderings of all known custom types.
  - `exposedCustomTypes` -- All custom type orders that are exposed from local
    module.
  - `fileIsIgnored` -- Whether file should not be checked for errors
  - `lookupTable` -- Module name lookup table
  - `extractSourceCode` -- Source extractor for fixes

-}
type alias ModuleContext =
    { customTypes :
        Dict
            ModuleName
            (Dict
                String
                { constructors : Set String
                , declarationOrder : List String
                }
            )
    , exposedCustomTypes :
        Dict
            String
            { constructors : Set String
            , declarationOrder : List String
            }
    , fileIsIgnored : Bool
    , lookupTable : ModuleNameLookupTable
    , extractSource : Range -> String
    }


{-| Any pattern that might be sortable.

  - `Constructor` -- A constructor pattern, with its type, declaration order, and any subpatterns, e.g. `Just 1 ->` becomes

```
Constructor
    { order = 0
    , subpatterns = [ Just (Literal (IntLiteral 1)) ]
    , type_ = ( [ "Basics" ], "Maybe" )
    }
```

  - `ListTupleOrUncons` -- A list, tuple, or uncons pattern, e.g. `(Nothing, Nothing) ->` becomes

```
ListTupleOrUncons
    { subpatterns =
        [ Constructor
            { order = 1
            , subpatterns = []
            , type_ = ( [ "Basics" ], "Maybe" )
            }
        , Constructor
            { order = 1
            , subpatterns = []
            , type_ = ( [ "Basics" ], "Maybe" )
            }
        ]
    , terminates = True
    }
```

and `var :: _ ->` becomes

    ListTupleOrUncons
        { subpatterns =
            [ Wildcard ]
        , terminates = False
        }

  - `Literal` -- A literal pattern, e.g. `1 ->` becomes

```
Literal (IntLiteral 1)
```

  - `Wildcard` -- A wildcard or var pattern, e.g. `var ->` becomes

```
Wildcard
```

-}
type SortablePattern
    = Constructor
        { order : Int
        , subpatterns : List (Maybe SortablePattern)
        , type_ : ( ModuleName, String )
        }
    | ListTupleOrUncons
        { subpatterns : List SortablePattern
        , terminates : Bool
        }
    | Literal LiteralPattern
    | Wildcard


{-| A literal pattern. Int and Hex literals are not distinguished, as they are
sorted identically.
-}
type LiteralPattern
    = CharLiteral Char
    | StringLiteral String
    | IntLiteral Int
    | FloatLiteral Float



-- * MODULE VISITOR


{-| Visit each module, first getting types from all declarations and then
checking all expressions for `case`s.
-}
moduleVisitor : RuleConfig -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withExpressionEnterVisitor
            (\e c ->
                if c.fileIsIgnored then
                    ( [], c )

                else
                    ( expressionVisitor config e c, c )
            )


{-| The initial project context knows of no types.
-}
initialProjectContext : ProjectContext
initialProjectContext =
    { customTypes = Dict.empty
    }


{-| Create a `ProjectContext` from a `ModuleContext`.
-}
fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName { exposedCustomTypes } ->
            { customTypes =
                if Dict.isEmpty exposedCustomTypes then
                    Dict.empty

                else
                    Dict.singleton moduleName exposedCustomTypes
            }
        )
        |> Rule.withModuleName


{-| Create a `ModuleContext` from a `ProjectContext`.
-}
fromProjectToModule : RuleConfig -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule config =
    Rule.initContextCreator
        (\lookupTable extractSource moduleName fileIsIgnored { declarations, moduleDefinition } projectContext ->
            let
                { exposedCustomTypes, customTypes } =
                    declarationListVisitor config
                        declarations
                        { moduleName = String.join "." moduleName
                        , exposedTypes = getExposedTypes <| Node.value moduleDefinition
                        , customTypes = projectContext.customTypes
                        }
            in
            { customTypes = customTypes
            , fileIsIgnored = fileIsIgnored
            , exposedCustomTypes = exposedCustomTypes
            , lookupTable = lookupTable
            , extractSource = extractSource
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withSourceCodeExtractor
        |> Rule.withModuleName
        |> Rule.withIsFileIgnored
        |> Rule.withFullAst


{-| Get a set of all types with exposed constructors or `Nothing` if everything
is exposed.
-}
getExposedTypes : Module -> Maybe (Set String)
getExposedTypes =
    let
        keepTypesWithExposedConstructors : Node TopLevelExpose -> Maybe String
        keepTypesWithExposedConstructors e =
            case Node.value e of
                TypeExpose { name } ->
                    Just name

                _ ->
                    Nothing
    in
    Module.exposingList
        >> (\l ->
                case l of
                    All _ ->
                        Nothing

                    Explicit es ->
                        List.filterMap keepTypesWithExposedConstructors es
                            |> Set.fromList
                            |> Just
           )


{-| Combine `ProjectContext`s by taking the union of known type orders.
-}
foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext prevContext =
    { customTypes =
        Dict.union newContext.customTypes prevContext.customTypes
    }



-- * DEPENDENCY VISITOR


{-| Visit all dependencies and store type order from them.
-}
dependencyVisitor : RuleConfig -> Dict String Dependency -> ProjectContext -> ProjectContext
dependencyVisitor (RuleConfig config) deps context =
    let
        docToEntry : String -> Elm.Docs.Union -> Maybe ( String, { constructors : Set String, declarationOrder : List String } )
        docToEntry moduleName { name, tags } =
            let
                constructors : List String
                constructors =
                    List.map Tuple.first tags
            in
            case ( config.sortablePredicate moduleName name, config.sortTypesFromDependencies ) of
                ( True, AlphabeticalOrder ) ->
                    Just
                        ( name
                        , { constructors = Set.fromList constructors
                          , declarationOrder = ListX.stableSortWith compare constructors
                          }
                        )

                ( True, _ ) ->
                    Just
                        ( name
                        , { constructors = Set.fromList constructors
                          , declarationOrder = constructors
                          }
                        )

                ( False, _ ) ->
                    Nothing
    in
    if config.sortTypesFromDependencies /= DoNotSort then
        Dict.foldl
            (\_ dep acc ->
                Dependency.modules dep
                    |> List.filterMap
                        (\{ name, unions } ->
                            List.filterMap (docToEntry name) unions
                                |> Dict.fromList
                                |> (\ts ->
                                        if Dict.isEmpty ts then
                                            Nothing

                                        else
                                            Just ts
                                   )
                                |> Maybe.map
                                    (Tuple.pair
                                        -- Convert to a `ModuleName`
                                        (String.split "." name)
                                    )
                        )
                    |> Dict.fromList
                    |> (\types -> { acc | customTypes = Dict.union types acc.customTypes })
            )
            context
            deps

    else
        context


{-| Visit declarations, storing custom type orders.
-}
declarationListVisitor :
    RuleConfig
    -> List (Node Declaration)
    ->
        { moduleName : String
        , exposedTypes : Maybe (Set String)
        , customTypes :
            Dict
                ModuleName
                (Dict
                    String
                    { constructors : Set String
                    , declarationOrder : List String
                    }
                )
        }
    ->
        { customTypes :
            Dict
                ModuleName
                (Dict
                    String
                    { constructors : Set String
                    , declarationOrder : List String
                    }
                )
        , exposedCustomTypes :
            Dict
                String
                { constructors : Set String
                , declarationOrder : List String
                }
        }
declarationListVisitor (RuleConfig config) declarations { moduleName, exposedTypes, customTypes } =
    let
        getCustomType : Node Declaration -> Maybe ( String, { constructors : Set String, declarationOrder : List String } )
        getCustomType node =
            case Node.value node of
                Declaration.CustomTypeDeclaration ({ name } as type_) ->
                    if config.sortablePredicate moduleName (Node.value name) then
                        Just
                            ( Node.value type_.name
                            , typeConstructors type_
                            )

                    else
                        Nothing

                _ ->
                    Nothing

        typeConstructors : Type -> { constructors : Set String, declarationOrder : List String }
        typeConstructors type_ =
            type_.constructors
                |> List.map (Node.value >> .name >> Node.value)
                |> (\cs ->
                        { constructors = Set.fromList cs
                        , declarationOrder = cs
                        }
                   )
    in
    -- Find custom types that were defined in the module
    List.filterMap getCustomType declarations
        |> (\ts ->
                if List.isEmpty ts then
                    { customTypes = customTypes
                    , exposedCustomTypes = Dict.empty
                    }

                else
                    { customTypes =
                        Dict.fromList ts
                            |> (\v -> Dict.insert [] v customTypes)
                    , exposedCustomTypes =
                        List.filter (\( typeName, _ ) -> MaybeX.unwrap True (Set.member typeName) exposedTypes) ts
                            |> Dict.fromList
                    }
           )


{-| Visit all expressions in a module, checking for `case`s and ensuring those
are sorted properly.
-}
expressionVisitor : RuleConfig -> Node Expression -> ModuleContext -> List (Error {})
expressionVisitor config node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            let
                errorRange : Range
                errorRange =
                    let
                        r : Range
                        r =
                            Node.range node
                    in
                    -- Assume that the `case` of a case block is just the first 4 chars
                    { r | end = { row = r.start.row, column = r.start.column + 4 } }
            in
            -- Convert all patterns to sortable ones, if we can
            ListX.indexedFoldr
                (\i ( p, e ) acc ->
                    getSortablePattern config context p
                        |> Maybe.map
                            (\sP ->
                                { pattern = sP
                                , index = i
                                , range =
                                    Range.combine
                                        [ Node.range p
                                        , Node.range e
                                        ]
                                }
                            )
                        |> Maybe.map2 (\acc_ sP -> sP :: acc_) acc
                )
                (Just [])
                cases
                |> Maybe.map
                    (checkSorting context.extractSource
                        "Case patterns"
                        [ -- Sort by control flow preservation first
                          \c1 c2 -> compareByControlFlow config ( c1.index, c1.pattern ) ( c2.index, c2.pattern )

                        -- Then by ordering
                        , \c1 c2 -> Maybe.withDefault EQ <| comparePatterns config c1.pattern c2.pattern
                        ]
                        errorRange
                    )
                |> Maybe.withDefault []

        _ ->
            -- Nothing to sort in non-case expressions.
            []


{-| Given config, context, and a pattern, convert it into a pattern we know
how to sort, if possible.
-}
getSortablePattern : RuleConfig -> ModuleContext -> Node Pattern -> Maybe SortablePattern
getSortablePattern ((RuleConfig config) as ruleConfig) context node =
    let
        go : Node Pattern -> Maybe SortablePattern
        go =
            getSortablePattern ruleConfig context

        n : Node Pattern
        n =
            getActualPattern node

        makeLiteral : (a -> LiteralPattern) -> a -> Maybe SortablePattern
        makeLiteral l a =
            if config.sortLiterals then
                Just <| Literal <| l a

            else
                Nothing

        findConstructorOrder : String -> List (Node Pattern) -> ModuleName -> Maybe SortablePattern
        findConstructorOrder constructor ps moduleName =
            -- Get types for the module
            Dict.get moduleName context.customTypes
                -- Find the type that the constructor belongs to
                |> Maybe.andThen
                    (DictX.find
                        (\_ { constructors } ->
                            Set.member constructor constructors
                        )
                    )
                -- Find its constructor order
                |> Maybe.andThen
                    (\( matchedType, { declarationOrder } ) ->
                        ListX.elemIndex constructor declarationOrder
                            |> Maybe.map
                                (\order ->
                                    Constructor
                                        { type_ = ( moduleName, matchedType )
                                        , order = order
                                        , subpatterns = List.map go ps
                                        }
                                )
                    )
    in
    case Node.value n of
        -- Find declaration sorting for named patterns and their arguments
        Pattern.NamedPattern { name } ps ->
            moduleNameFor context.lookupTable n
                |> Maybe.andThen (findConstructorOrder name ps)

        -- Tuples and lists we recursively convert each subpattern
        Pattern.TuplePattern ps ->
            MaybeX.traverse go ps
                |> Maybe.map
                    (\subpatterns ->
                        ListTupleOrUncons
                            { subpatterns = subpatterns
                            , terminates = True
                            }
                    )

        Pattern.ListPattern ps ->
            MaybeX.traverse go ps
                |> Maybe.map
                    (\subpatterns ->
                        ListTupleOrUncons
                            { subpatterns = subpatterns
                            , terminates = True
                            }
                    )

        -- Uncons pattern we recursively convert each subpattern and convert to the equivalent list
        Pattern.UnConsPattern p1 p2 ->
            let
                cons : SortablePattern -> SortablePattern -> Maybe SortablePattern
                cons x xs =
                    case xs of
                        Wildcard ->
                            Just <|
                                ListTupleOrUncons
                                    { subpatterns = [ x ]
                                    , terminates = False
                                    }

                        ListTupleOrUncons r ->
                            Just <|
                                ListTupleOrUncons
                                    { r | subpatterns = x :: r.subpatterns }

                        _ ->
                            -- You can't cons onto a constructor or Literal, so this is a type error
                            Nothing
            in
            Maybe.map2 cons (go p1) (go p2)
                |> MaybeX.join

        -- Var and _ are wildcards
        Pattern.AllPattern ->
            Just Wildcard

        Pattern.VarPattern _ ->
            Just Wildcard

        -- Literals can be sorted if configured to
        Pattern.CharPattern c ->
            makeLiteral CharLiteral c

        Pattern.StringPattern s ->
            makeLiteral StringLiteral s

        Pattern.IntPattern i ->
            makeLiteral IntLiteral i

        Pattern.HexPattern i ->
            makeLiteral IntLiteral i

        Pattern.FloatPattern f ->
            makeLiteral FloatLiteral f

        _ ->
            -- Remaining patterns are Unit and Record, which are not sortable, and Parens/As patterns, which we have already unwrapped
            Nothing


{-| Unwrap a pattern to get at the actual pattern inside of any parentheses or
`as` patterns.
-}
getActualPattern : Node Pattern -> Node Pattern
getActualPattern node =
    case Node.value node of
        -- Parenthesized/as patterns we just descend into
        Pattern.ParenthesizedPattern p ->
            getActualPattern p

        Pattern.AsPattern p _ ->
            getActualPattern p

        -- Other pattern are just the pattern itself
        _ ->
            node


{-| Compare two literal types, determining their order (if not a type error).
-}
compareLiteral : LiteralPattern -> LiteralPattern -> Maybe Order
compareLiteral l1 l2 =
    case ( l1, l2 ) of
        ( CharLiteral c1, CharLiteral c2 ) ->
            Just <| compare c1 c2

        ( StringLiteral s1, StringLiteral s2 ) ->
            Just <| compare s1 s2

        ( IntLiteral i1, IntLiteral i2 ) ->
            Just <| compare i1 i2

        ( FloatLiteral f1, FloatLiteral f2 ) ->
            Just <| compare f1 f2

        _ ->
            -- This is a type error, so ignore it
            Nothing


{-| Sort two patterns by the preservation of control flow, e.g. assuring that
wildcards are not moved before non-wildcards. If this function returns `EQ`,
then the two patterns may have their order switched safely.
-}
compareByControlFlow : RuleConfig -> ( Int, SortablePattern ) -> ( Int, SortablePattern ) -> Order
compareByControlFlow config ( i1, pat1 ) ( i2, pat2 ) =
    let
        go : SortablePattern -> SortablePattern -> Order
        go p1 p2 =
            compareByControlFlow config ( i1, p1 ) ( i2, p2 )
    in
    case ( pat1, pat2 ) of
        -- If both are wildcards, then they do not have defined order
        ( Wildcard, Wildcard ) ->
            EQ

        -- Wildcards cannot be moved relative to non-wildcards, so return index order
        ( Wildcard, _ ) ->
            compare i1 i2

        ( _, Wildcard ) ->
            compare i1 i2

        ( Constructor c1, Constructor c2 ) ->
            let
                goSubs : List (Maybe SortablePattern) -> List (Maybe SortablePattern) -> () -> Order
                goSubs pat1s pat2s () =
                    case ( pat1s, pat2s ) of
                        -- Wildcards cannot be moved compared to things we can't sort
                        ( (Just Wildcard) :: _, Nothing :: _ ) ->
                            compare i1 i2

                        ( Nothing :: _, (Just Wildcard) :: _ ) ->
                            compare i1 i2

                        ( p1 :: p1s, p2 :: p2s ) ->
                            -- Check each subpattern sequentially for control flow ordering
                            goSubs p1s p2s
                                |> fallbackCompareFor (Maybe.withDefault EQ <| Maybe.map2 go p1 p2)

                        _ ->
                            -- Either exhausted subpatterns with no problems or a type error
                            EQ
            in
            if c1.order == c2.order then
                -- If the constructors are the same, then control flow confusion could occur in subpatterns
                goSubs c1.subpatterns c2.subpatterns ()

            else
                -- Otherwise, no possibility of control flow confusion, as constructors are different
                EQ

        -- Lists, Tuples, and Uncons
        ( ListTupleOrUncons r1, ListTupleOrUncons r2 ) ->
            if safelySortableListPatterns config r1 r2 then
                -- Can safely sort them
                EQ

            else
                -- Otherwise, enforce ordering
                compare i1 i2

        -- Anything else is a type error or has no wildcards, so we needn't consider it
        _ ->
            EQ


{-| Check if list/tuple/uncons patterns can safely be sorted by making certain
neither would override the other's control flow.
-}
safelySortableListPatterns : RuleConfig -> { subpatterns : List SortablePattern, terminates : Bool } -> { subpatterns : List SortablePattern, terminates : Bool } -> Bool
safelySortableListPatterns config r1 r2 =
    case ( r1.subpatterns, r2.subpatterns ) of
        ( x :: xs, y :: ys ) ->
            -- Check if the head of the lists is sortable
            case comparePatterns config x y of
                Just EQ ->
                    -- If the left-most subpatterns are equal, then they are sortable if the next subpattern is sortable, so recurse
                    safelySortableListPatterns config
                        { r1 | subpatterns = xs }
                        { r2 | subpatterns = ys }

                Just _ ->
                    -- If the left-most subpattern is sortable, then they can be distinguished and so are safely sortable
                    True

                Nothing ->
                    -- If the left-most subpattern is not sortable, they cannot be sorted
                    False

        ( [], [] ) ->
            -- Both have been exhausted, so no problems sorting them
            True

        ( [], _ ) ->
            -- r1 is shorter than r2, so it is safe to sort it if it terminates
            r1.terminates

        ( _, [] ) ->
            -- r2 is shorter than r1, so it is safe to sort it if it terminates
            r2.terminates


{-| Compare two sortable patterns, determining their order (if not a type error).
-}
comparePatterns : RuleConfig -> SortablePattern -> SortablePattern -> Maybe Order
comparePatterns ((RuleConfig { lookPastUnsortable }) as ruleConfig) pat1 pat2 =
    let
        go : SortablePattern -> SortablePattern -> Maybe Order
        go p1 p2 =
            comparePatterns ruleConfig p1 p2
    in
    case ( pat1, pat2 ) of
        -- Wildcards can be sorted past if both are wild
        ( Wildcard, Wildcard ) ->
            Just EQ

        -- Wildcards cannot be moved relative to non-wildcards, so return Nothing, which ensures that they are not sorted past.
        ( Wildcard, _ ) ->
            Nothing

        ( _, Wildcard ) ->
            Nothing

        -- Literals are simply compared; if sorting literals is turned off, then LiteralPatterns are not created at all
        ( Literal l1, Literal l2 ) ->
            compareLiteral l1 l2

        --Constructors are compared by index, then by comparing subpatterns sequentially, failing if a non-sortable subpattern is encountered
        ( Constructor c1, Constructor c2 ) ->
            let
                goSubs : List (Maybe SortablePattern) -> List (Maybe SortablePattern) -> () -> Maybe Order
                goSubs pat1s pat2s () =
                    case ( pat1s, pat2s, lookPastUnsortable ) of
                        ( (Just p1) :: p1s, (Just p2) :: p2s, _ ) ->
                            goSubs p1s p2s
                                |> fallbackCompareWithUnsortableFor (go p1 p2)

                        ( Nothing :: p1s, Nothing :: p2s, True ) ->
                            -- If at the point where arguments are both unsortable, then proceed past if configured to
                            goSubs p1s p2s ()

                        ( [], [], _ ) ->
                            -- Both lists of subpatterns exhausted without a "winner", so return EQ
                            Just EQ

                        _ ->
                            -- Lists should be even, so other cases aren't sortable
                            Nothing
            in
            -- Fallback to subpatterns
            goSubs c1.subpatterns c2.subpatterns
                |> fallbackCompareWithUnsortableFor (Just <| compare c1.order c2.order)

        -- Lists, Tuples, and Uncons
        ( ListTupleOrUncons r1, ListTupleOrUncons r2 ) ->
            case
                ( ( r1.subpatterns, r1.terminates )
                , ( r2.subpatterns, r2.terminates )
                )
            of
                -- If the lists are the same length, infinite ones go later
                ( ( [], False ), ( [], True ) ) ->
                    Just GT

                ( ( [], True ), ( [], False ) ) ->
                    Just LT

                ( ( [], _ ), ( [], _ ) ) ->
                    Just EQ

                -- If one list is shorter than another, it goes after if it is infinite or before if it isn't
                ( ( _ :: _, _ ), ( [], True ) ) ->
                    Just GT

                ( ( _ :: _, _ ), ( [], False ) ) ->
                    Just LT

                ( ( [], True ), ( _ :: _, _ ) ) ->
                    Just LT

                ( ( [], False ), ( _ :: _, _ ) ) ->
                    Just GT

                -- Otherwise, compare the lists sequentially
                ( ( p1 :: p1s, _ ), ( p2 :: p2s, _ ) ) ->
                    compareNonemptyListPatterns ruleConfig ( r1, p1, p1s ) ( r2, p2, p2s )

        -- Anything else should be a type error, so we needn't consider it
        _ ->
            Nothing


{-| Compare nonempty list/tuple/uncons pattern sorting by checking by length (if
configured to) and element-wise.
-}
compareNonemptyListPatterns : RuleConfig -> ( { subpatterns : List SortablePattern, terminates : Bool }, SortablePattern, List SortablePattern ) -> ( { subpatterns : List SortablePattern, terminates : Bool }, SortablePattern, List SortablePattern ) -> Maybe Order
compareNonemptyListPatterns ((RuleConfig { sortLists }) as config) ( r1, p1, p1s ) ( r2, p2, p2s ) =
    let
        checkSubs : () -> Maybe Order
        checkSubs () =
            case comparePatterns config p1 p2 of
                Just EQ ->
                    comparePatterns config
                        (ListTupleOrUncons { r1 | subpatterns = p1s })
                        (ListTupleOrUncons { r2 | subpatterns = p2s })

                ltOrGtOrNothing ->
                    ltOrGtOrNothing
    in
    if sortLists == LengthFirst then
        checkSubs
            |> fallbackCompareWithUnsortableFor (Just <| comparePatternListLengths r1 r2)

    else
        checkSubs ()


{-| Compare the list lengths of two lists of `SortablePattern`, with the caveat
that a list must be infinitely long if it ends in a wildcard, with a shorter
list ending in a wildcard being "longer" (more specified) than a longer one.
-}
comparePatternListLengths :
    { subpatterns : List SortablePattern
    , terminates : Bool
    }
    ->
        { subpatterns : List SortablePattern
        , terminates : Bool
        }
    -> Order
comparePatternListLengths p1s p2s =
    case ( p1s.terminates, p2s.terminates ) of
        ( False, False ) ->
            -- Flip comparison if both end in wildcards
            compare (List.length p2s.subpatterns) (List.length p1s.subpatterns)

        ( False, True ) ->
            GT

        ( True, False ) ->
            LT

        ( True, True ) ->
            -- Compare normally if neither does
            compare (List.length p1s.subpatterns) (List.length p2s.subpatterns)
