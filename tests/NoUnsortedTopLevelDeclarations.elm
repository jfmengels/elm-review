module NoUnsortedTopLevelDeclarations exposing
    ( rule
    , RuleConfig, sortTopLevelDeclarations
    , alphabetically, exposedOrderWithPrivateLast, exposedOrderWithPrivateFirst, typesFirst, typesLast, portsFirst, portsLast
    , glueHelpersBefore, glueHelpersAfter, glueDependenciesBeforeFirstDependent, glueDependenciesAfterFirstDependent, glueDependenciesAfterLastDependent, glueDependenciesBeforeLastDependent
    )

{-|


## Review Rule

@docs rule


## Configuration

@docs RuleConfig, sortTopLevelDeclarations


## Orderings

@docs alphabetically, exposedOrderWithPrivateLast, exposedOrderWithPrivateFirst, typesFirst, typesLast, portsFirst, portsLast


## Glues

Glues provide a way to "stick" one declaration to another, i.e. to always sort
one declaration alongside another. Note that glues will chain, i.e. if `a` is
glued before `b` and `b` is glued after `c`, then the result will be `c` -> `a`
-> `b` (sorted wherever `c` is sorted to). Glues behave in the following ways:

  - If multiple glues are specified, the first specified will be used.
  - If multiple declarations are glued at the same place, they will be ordered
    by the orderings specified.
  - If glues are not acyclic (i.e. two declarations are glued to each other,
    possibly via intermediates), then all of the involved declarations will not
    be glued and will be sorted normally.

@docs glueHelpersBefore, glueHelpersAfter, glueDependenciesBeforeFirstDependent, glueDependenciesAfterFirstDependent, glueDependenciesAfterLastDependent, glueDependenciesBeforeLastDependent

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import String.Extra as StringX
import Util exposing (GluedTo(..), checkSortingWithGlue, findAllNamesIn, findDependencies, validate)


{-| Reports top-level declarations that are not in the "proper" order.

🔧 Running with `--fix` will automatically sort the declarations.

The proper order of declarations is specified in the rule configuration. See the
[Configuration](#configuration) section below for more information.

    config =
        [ NoUnsortedTopLevelDeclarations.rule
            (NoUnsortedTopLevelDeclarations.sortTopLevelDeclarations
                |> NoUnsortedTopLevelDeclarations.portsFirst
                |> NoUnsortedTopLevelDeclarations.exposedOrderWithPrivateLast
                |> NoUnsortedTopLevelDeclarations.alphabetically
            )
        ]


## Fail

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    type A
        = A

    z =
        zed

    type alias Z =
        A

    a =
        foo

    b =
        bar


## Success

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    type A
        = A

    a =
        foo

    type alias Z =
        A

    b =
        bar

    z =
        zed


## When (not) to enable this rule

This rule is useful when you want to ensure that your top-level declarations are
in a consistent, predictable order.

This rule is not useful when you want to be able to write top-level declarations
in varying orders throughout your codebase, e.g. if you want to emphasize what
is most important on a case-by-case basis.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example --rules NoUnsortedTopLevelDeclarations
```

-}
rule : RuleConfig r -> Rule
rule (RuleConfig r) =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnsortedTopLevelDeclarations" initialContext
        |> Rule.withModuleDefinitionVisitor (\m c -> ( [], getModuleExports m c ))
        |> Rule.withCommentsVisitor (\cs context -> ( [], getUnparsedDocComments cs context ))
        |> Rule.withImportVisitor (\i context -> ( [], accumulateImportRange i context ))
        |> Rule.withDeclarationListVisitor
            (\ds c ->
                ( declarationVisitor
                    (RuleConfig
                        { r
                            | glues = List.reverse r.glues
                            , sortBy = List.reverse r.sortBy
                        }
                    )
                    ds
                    c
                , c
                )
            )
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


{-| Configuration for this rule. Create a new one with
`sortTopLevelDeclarations` and use orderings to create a hierarchy of sorting.
-}
type RuleConfig r
    = RuleConfig
        { sortBy : List (TLD -> TLD -> Order)
        , glues : List Glue
        }


{-| Info about the module, as well as the source extractor.
-}
type alias Context =
    { extractSource : Range -> String
    , exports : Maybe (List String)
    , errorRange : Range
    , unparsedDocComments : List (Node String)
    , moduleImportRange : Maybe Range
    }


{-| Given a `TLD` and a list of other `TLD`s,
-}
type alias Glue =
    ( Int, TLD ) -> List TLD -> Maybe GluedTo


{-| Information about a TLD.
-}
type alias TLD =
    { type_ : DeclarationType
    , namesBound : Set String
    , exposedOrder : Maybe Int
    , range : Range
    , dependentOnBindings : Set String
    , glued : Maybe GluedTo
    }


{-| The type of TLD it is.
-}
type DeclarationType
    = Function
    | Port
    | Type


{-| Create a new `RuleConfig`. Use the various orderings to then specify
primary and fallback orderings.
-}
sortTopLevelDeclarations : RuleConfig { noAlphabetical : (), noDependency : (), noExposed : (), noHelper : (), noType : (), noPort : () }
sortTopLevelDeclarations =
    RuleConfig { sortBy = [], glues = [] }


{-| Sort declarations alphabetically. Note that this decapitalizes the first
letter before performing the comparison so as to treat types and functions the
same. For example, the following is sorted alphabetically:

    type A
        = A

    a =
        foo

    b =
        bar

    z =
        zed

    type alias Z =
        A

-}
alphabetically : RuleConfig { r | noAlphabetical : () } -> RuleConfig r
alphabetically (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 -> compare (Set.toList <| Set.map StringX.decapitalize d1.namesBound) (Set.toList <| Set.map StringX.decapitalize d2.namesBound))
                    :: r.sortBy
        }


{-| Sort TLDs in the order they are exposed by the module, with private TLDs
coming after all those that are exposed. For example, the following is sorted
by this and then alphabetically:

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    type A
        = A

    a =
        foo

    type alias Z =
        A

    b =
        bar

    z =
        zed

-}
exposedOrderWithPrivateLast : RuleConfig { r | noExposed : () } -> RuleConfig r
exposedOrderWithPrivateLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.exposedOrder, d2.exposedOrder ) of
                        ( Just i1, Just i2 ) ->
                            compare i1 i2

                        ( Just _, Nothing ) ->
                            LT

                        ( Nothing, Just _ ) ->
                            GT

                        ( Nothing, Nothing ) ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs in the order they are exposed by the module, with private TLDs
coming before all those that are exposed. For example, the following is sorted
by this and then alphabetically:

    module A exposing
        ( A, a
        , Z
        )

    {-|

    @docs A, a
    @docs Z

    -}

    b =
        bar

    z =
        zed

    type A
        = A

    a =
        foo

    type alias Z =
        A

-}
exposedOrderWithPrivateFirst : RuleConfig { r | noExposed : () } -> RuleConfig r
exposedOrderWithPrivateFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.exposedOrder, d2.exposedOrder ) of
                        ( Just i1, Just i2 ) ->
                            compare i1 i2

                        ( Just _, Nothing ) ->
                            GT

                        ( Nothing, Just _ ) ->
                            LT

                        ( Nothing, Nothing ) ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that types and type aliases always come before functions (and
ports, if they haven't been sorted already). For example, the following is
sorted by this order and then alphabetically:

    type A
        = A

    type alias Z =
        A

    a =
        foo

    b =
        bar

    z =
        zed

-}
typesFirst : RuleConfig { r | noType : () } -> RuleConfig r
typesFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Type, Type ) ->
                            EQ

                        ( Type, _ ) ->
                            LT

                        ( _, Type ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that types and type aliases always come after functions (and
ports, if they haven't been sorted already). For example, the following is
sorted by this order and then alphabetically:

    a =
        foo

    b =
        bar

    z =
        zed

    type A
        = A

    type alias Z =
        A

-}
typesLast : RuleConfig { r | noType : () } -> RuleConfig r
typesLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Type, Type ) ->
                            EQ

                        ( Type, _ ) ->
                            GT

                        ( _, Type ) ->
                            LT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that ports always come before functions (and types, if they
haven't been sorted already). For example, the following is sorted by this order
and then alphabetically:

    port sendMessage : String -> Cmd msg

    type A
        = A

    a =
        foo

    b =
        bar

    type alias Z =
        A

    z =
        zed

-}
portsFirst : RuleConfig { r | noPort : () } -> RuleConfig r
portsFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Port, Port ) ->
                            EQ

                        ( Port, _ ) ->
                            LT

                        ( _, Port ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort TLDs so that ports always come after functions (and types, if they
haven't been sorted already). For example, the following is sorted by this order
and then alphabetically:

    type A
        = A

    a =
        foo

    b =
        bar

    type alias Z =
        A

    z =
        zed

    port sendMessage : String -> Cmd msg

-}
portsLast : RuleConfig { r | noPort : () } -> RuleConfig r
portsLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.type_, d2.type_ ) of
                        ( Port, Port ) ->
                            EQ

                        ( Port, _ ) ->
                            GT

                        ( _, Port ) ->
                            LT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Dependencies are _unexposed_ functions that are used in multiple other
functions. This glue attaches them immediately before the first function they
are used in.

For example:

    unwrap =
        some func

    a x =
        unwrap x

    b x =
        unwrap x

    c x =
        unwrap x

-}
glueDependenciesBeforeFirstDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesBeforeFirstDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only unexposed functions can be dependencies
                    if d.exposedOrder == Nothing && d.type_ == Function then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedBeforeFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Dependencies are _unexposed_ functions that are used in multiple other
functions. This glue attaches them immediately after the first function they
are used in.

For example:

    a x =
        unwrap x

    unwrap =
        some func

    b x =
        unwrap x

    c x =
        unwrap x

-}
glueDependenciesAfterFirstDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesAfterFirstDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only unexposed functions can be dependencies
                    if d.exposedOrder == Nothing && d.type_ == Function then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedAfterFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Dependencies are _unexposed_ functions that are used in multiple other
functions. This glue attaches them immediately before the last function they
are used in.

For example:

    a x =
        unwrap x

    b x =
        unwrap x

    unwrap =
        some func

    c x =
        unwrap x

-}
glueDependenciesBeforeLastDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesBeforeLastDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only unexposed functions can be dependencies
                    if d.exposedOrder == Nothing && d.type_ == Function then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedBeforeLast << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Dependencies are _unexposed_ functions that are used in multiple other
functions. This glue attaches them immediately after the last function they
are used in.

For example:

    a x =
        unwrap x

    b x =
        unwrap x

    c x =
        unwrap x

    unwrap =
        some func

-}
glueDependenciesAfterLastDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesAfterLastDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only unexposed functions can be dependencies
                    if d.exposedOrder == Nothing && d.type_ == Function then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedAfterLast << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Helpers are _unexposed_ functions that are used in exactly one other
function. This glue attaches them immediately before the function they are used
in.

For example:

    foldrHelper : (a -> b -> b) -> b -> Int -> List a -> b
    foldrHelper fn acc ctr ls =
        case ls of
            [] ->
                acc

            a :: r1 ->
                ...

    {-| Reduce a list from the right.
    -}
    foldr : (a -> b -> b) -> b -> List a -> b
    foldr fn acc ls =
        foldrHelper fn acc 0 ls

-}
glueHelpersBefore : RuleConfig { r | noHelper : () } -> RuleConfig r
glueHelpersBefore (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only unexposed functions can be helpers
                    if d.exposedOrder == Nothing && d.type_ == Function then
                        findDependencies ( i, d ) ds
                            |> validate ((==) 1 << Tuple.second)
                            |> Maybe.map (GluedBeforeFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Helpers are _unexposed_ functions that are used in exactly one other
function. This glue attaches them immediately after the function they are used
in.

For example:

    {-| Reduce a list from the right.
    -}
    foldr : (a -> b -> b) -> b -> List a -> b
    foldr fn acc ls =
        foldrHelper fn acc 0 ls

    foldrHelper : (a -> b -> b) -> b -> Int -> List a -> b
    foldrHelper fn acc ctr ls =
        case ls of
            [] ->
                acc

            a :: r1 ->
                ...

-}
glueHelpersAfter : RuleConfig { r | noHelper : () } -> RuleConfig r
glueHelpersAfter (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only unexposed functions can be helpers
                    if d.exposedOrder == Nothing && d.type_ == Function then
                        findDependencies ( i, d ) ds
                            |> validate ((==) 1 << Tuple.second)
                            |> Maybe.map (GluedAfterFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Create a context with a source extractor.
-}
initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSource () ->
            { extractSource = extractSource
            , exports = Nothing
            , errorRange = Range.empty
            , unparsedDocComments = []
            , moduleImportRange = Nothing
            }
        )
        |> Rule.withSourceCodeExtractor


{-| Get an ordered list of all names exported by a module.
-}
getModuleExports : Node Module -> Context -> Context
getModuleExports m context =
    let
        r : Range
        r =
            Node.range m

        errorRange : Range
        errorRange =
            case Node.value m of
                Module.PortModule _ ->
                    -- Assume that the `port module` of a module is just the first 11 chars
                    { r | end = { row = r.start.row, column = r.start.column + 11 } }

                _ ->
                    -- Assume that the `module` of a module is just the first 6 chars
                    -- Effect modules aren't a thing
                    { r | end = { row = r.start.row, column = r.start.column + 6 } }
    in
    case Module.exposingList <| Node.value m of
        Exposing.All _ ->
            { context | errorRange = errorRange }

        Exposing.Explicit exports ->
            { context
                | exports =
                    Just <|
                        List.map
                            (\e ->
                                case Node.value e of
                                    InfixExpose s ->
                                        s

                                    FunctionExpose s ->
                                        s

                                    TypeOrAliasExpose s ->
                                        s

                                    TypeExpose { name } ->
                                        name
                            )
                            exports
                , errorRange = errorRange
            }


{-| Get an ordered list of all doc comments in the module that were not parsed
by `elm-syntax`.
-}
getUnparsedDocComments : List (Node String) -> Context -> Context
getUnparsedDocComments comments context =
    { context
        | unparsedDocComments =
            List.filter
                (Node.value
                    >> (\c ->
                            String.startsWith "{-|" c
                                && String.endsWith "-}" c
                       )
                )
                comments
    }


{-| Collect the range of imports in the module.
-}
accumulateImportRange : Node Import -> Context -> Context
accumulateImportRange i context =
    { context
        | moduleImportRange =
            MaybeX.unpack (\() -> Just <| Node.range i)
                (\r -> Just <| Range.combine [ r, Node.range i ])
                context.moduleImportRange
    }


{-| Generate declaration info for all TLDs and then check that they are sorted.
-}
declarationVisitor : RuleConfig r -> List (Node Declaration) -> Context -> List (Error {})
declarationVisitor (RuleConfig { glues, sortBy }) decs context =
    let
        applyGlues : List TLD -> Int -> TLD -> TLD
        applyGlues ds i d =
            { d | glued = ListX.findMap (\g -> g ( i, d ) ds) glues }
    in
    ListX.uncons context.unparsedDocComments
        |> MaybeX.unwrap []
            (\( c, cs ) ->
                case Maybe.map (Range.compare (Node.range c)) context.moduleImportRange of
                    Just GT ->
                        -- First doc comment is after imports, so cannot be module-associated.
                        context.unparsedDocComments

                    Nothing ->
                        -- No module imports, so have to figure it out manually
                        -- If there are two doc comments before first TLD, then
                        -- the first must be module But do not need to test for
                        -- this, because the first would get dropped anyways,
                        -- since only the latter would possibly get attached.
                        -- Similarly, do not need to worry if the first TLD has
                        -- a parsed doc comment, because it would get dropped by
                        -- going past that TLD. If first is a doc comment with a
                        -- line that begins with "@docs", then it is essentially
                        -- guaranteed to be for the module, however.
                        if List.any (String.startsWith "@docs") <| String.lines <| Node.value c then
                            cs

                        else
                            -- There is no @docs in the comment, so this might be a doc comment belonging to the first TLD, if it is a port.
                            context.unparsedDocComments

                    _ ->
                        -- First doc comment is before imports, so is the module doc comment, so drop it
                        cs
            )
        |> (\docCommentsWithoutModule ->
                List.foldl (getDecInfo context.exports) ( [], docCommentsWithoutModule ) decs
           )
        |> Tuple.first
        |> List.reverse
        |> (\ds -> List.indexedMap (applyGlues ds) ds)
        |> checkSortingWithGlue context.extractSource "Top-level declarations" sortBy context.errorRange


{-| Given a list of module exports, generate TLD info from a `declaration`.
-}
getDecInfo : Maybe (List String) -> Node Declaration -> ( List TLD, List (Node String) ) -> ( List TLD, List (Node String) )
getDecInfo exports d ( acc, unparsedDocComments ) =
    let
        ( immediatelyPreviousDocComment, remainingDocComments ) =
            ListX.splitWhen (\c -> Range.compare (Node.range c) (Node.range d) == GT) unparsedDocComments
                |> Maybe.withDefault
                    -- No doc comments were after the TLD, so it must be the last
                    ( unparsedDocComments, [] )
                |> Tuple.mapFirst ListX.last
    in
    ( case Node.value d of
        FunctionDeclaration { declaration } ->
            Node.value declaration
                |> (\{ name, expression } ->
                        { type_ = Function
                        , exposedOrder = Maybe.andThen (ListX.elemIndex <| Node.value name) exports
                        , namesBound = Set.singleton <| Node.value name
                        , range = Node.range d
                        , glued = Nothing
                        , dependentOnBindings = findAllNamesIn expression
                        }
                   )
                |> Just

        AliasDeclaration { name } ->
            Just
                { type_ = Type
                , exposedOrder = Maybe.andThen (ListX.elemIndex (Node.value name)) exports
                , namesBound = Set.singleton <| Node.value name
                , range = Node.range d
                , glued = Nothing
                , dependentOnBindings = Set.empty
                }

        CustomTypeDeclaration { name } ->
            Just
                { type_ = Type
                , exposedOrder = Maybe.andThen (ListX.elemIndex (Node.value name)) exports
                , namesBound = Set.singleton <| Node.value name
                , range = Node.range d
                , glued = Nothing
                , dependentOnBindings = Set.empty
                }

        PortDeclaration { name } ->
            Just
                { type_ = Port
                , namesBound = Set.singleton <| Node.value name

                -- Glue doc comment on, since port doc comments are not yet parsed by `elm-syntax`
                , range = MaybeX.unwrap (Node.range d) (\c -> Range.combine [ Node.range c, Node.range d ]) immediatelyPreviousDocComment
                , glued = Nothing

                -- Ports can't be exposed or dependent on other TLDs
                , dependentOnBindings = Set.empty
                , exposedOrder = Nothing
                }

        _ ->
            -- These are impossible
            -- Destructuring (Node Pattern) (Node Expression)
            -- InfixDeclaration Infix
            Nothing
    , remainingDocComments
    )
        |> Tuple.mapFirst (MaybeX.unwrap acc (\tld -> tld :: acc))
