module NoUnsortedRecords exposing
    ( rule
    , RuleConfig, defaults
    , sortGenericFieldsLast
    , doNotSortAmbiguousRecords, reportAmbiguousRecordsWithoutFix
    , doNotSortUnknownRecords, reportUnknownRecordsWithoutFix
    , treatSubrecordsAsUnknown, treatAllSubrecordsAsCanonical, treatCustomTypeRecordsAsCanonical
    , typecheckAllRecords
    )

{-|


## Review Rule

@docs rule


## Configuration

@docs RuleConfig, defaults


### Sorting

@docs sortGenericFieldsLast


### Ambiguous Records

An ambiguous record is a record that matches more than one known "canonical"
record.

@docs doNotSortAmbiguousRecords, reportAmbiguousRecordsWithoutFix


### Unknown Records

An unknown record is a record that does not match any known "canonical" records.

@docs doNotSortUnknownRecords, reportUnknownRecordsWithoutFix


### Subrecords

Subrecords are records that are either within the fields of a type alias or are
arguments of a custom type.

@docs treatSubrecordsAsUnknown, treatAllSubrecordsAsCanonical, treatCustomTypeRecordsAsCanonical


### Other Settings

@docs typecheckAllRecords

-}

import Dict exposing (Dict)
import Dict.Extra as DictX
import Elm.Docs
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, TypeAnnotation(..))
import Elm.Type
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable, moduleNameFor)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Util exposing (checkSorting, makeAccessFunc, validate)


{-| Reports record fields that are not in the "proper" order.

🔧 Running with `--fix` will automatically sort the fields.

The proper order of record fields is the order in which they are defined in the
type alias in your source files. See the "Configuration" section below for more
information.

    config =
        [ NoUnsortedRecords.rule
            (NoUnsortedRecords.defaults
                |> NoUnsortedRecords.reportAmbiguousRecordsWithoutFix
            )
        ]


## "Proper" Order

Proper order may be defined in several ways. Firstly, type aliases define order,
e.g.

    type alias MyRecord =
        { foo : Int, bar : Int, baz : Int }

creates a record with name `MyRecord` and the known field order `foo`, `bar`,
`baz`.

Secondly, records without a defined type alias that are nevertheless either a
subrecord of a type alias or attached to a custom type are considered to be in
the order they are defined in the source:

    type MyType
        = A Int { foo : Int, bar : Int, baz : Int }
        | B { b : Int, a : Int, c : Int } String

when encountered in their larger context. By default, these are _not_ considered
canonical records when encountered alone, though this behavior may be turned on
with [`treatAllSubrecordsAsCanonical`](#treatAllSubrecordsAsCanonical) or
[`treatCustomTypeRecordsAsCanonical`](#treatCustomTypeRecordsAsCanonical).


## Inference/Disambiguation

Since records are not associated with a unique name, it is necessary to infer
what type alias a record matches. In the most ambiguous case, all type aliases
are checked for matching fields. If none are found, then the rule can't match it
to a specific order (though it may still optionally be sorted alphabetically).

If only one matching type alias is found, then the rule will sort by that order.

In the case of multiple matching field sets, several things may happen. If all
of the field sets have the same order, then it isn't necessary to unambiguously
identify which is being matched, and that one order will be used. Otherwise, the
rule is capable of using the following disambiguation rules:

  - Disambiguation by the fact that all fields must be present:

```
type alias A =
    { foo : Int, bar : Int, baz : Int }

type alias B =
    { bar : Int, foo : Int, baz : Int, extra : Int }

-- Must be type `A` because missing `extra`
a =
    { foo = 1, bar = 2, baz = 3 }
```

  - Disambiguation by type signature:

```
type alias A =
    { foo : Int, bar : Int, baz : Int }

type alias B =
    { bar : Int, foo : Int, baz : Int }

a : A
a =
    { foo = 1, bar = 2, baz = 3 }
```

It should be noted that this works with relatively complex type signatures, e.g.

    type alias A =
        { foo : Int, bar : Int, baz : Int }

    type alias B =
        { bar : Int, foo : Int, baz : Int }

    a : Int -> String -> ( Int, String, List A )
    a i s =
        ( i, s, [ { foo = 1, bar = 2, baz = 3 } ] )

This also works with patterns, e.g.

    type alias A =
        { foo : Int, bar : Int, baz : Int }

    type alias B =
        { bar : Int, foo : Int, baz : Int }

    a : Int -> A -> Int -> Bool
    a i1 { foo, bar, baz } i2 =
        True

  - Disambiguation by field type. Very rudimentary type inference is performed,
    but it may frequently be useful to add annotations, as the inference is by
    no means complete.

```
type alias A =
    { foo : Int, bar : Int, baz : Int }

type alias B =
    { bar : Int, foo : String, baz : Int }

-- Must be type `A` because `foo` is `Int`
a : { foo : Int, bar : Int, baz : Int }
a =
    { foo = 1, bar = 2, baz = 3 }
```

  - Disambiguation by the fact that the it is associated with a custom type with
    a known record argument:

```
type Custom
    = A { foo : Int, bar : Int, baz : Int }
    | B { bar : Int, foo : Int, baz : Int }

a =
    -- Must be `A`'s record
    A { foo = 1, bar = 2, baz = 3 }

b custom =
    case custom of
        -- Must be `A`'s record
        A { foo, bar } ->
            False

        -- Must be `B`'s record
        B { bar, foo } ->
            True
```

  - Disambiguation by the fact that the it is associated with a specific index
    of a custom type with a known record argument:

```
type Custom
    = A
        Int
        { foo : Int
        , bar : Int
        , baz : Int
        }
        String
        { bar : Int
        , foo : Int
        , baz : Int
        }

a custom =
    case custom of
        A _ { foo, bar } _ { bar, foo } ->
            False
```

  - Disambiguation by the fact that the it is associated with a specific field
    of a record alias:

```
type alias A =
    { a : { foo : Int, bar : Int, baz : Int }
    , b : { bar : Int, foo : Int, baz : Int }
    }

func : A
func =
    { a = { foo = 2, bar = 1, baz = 3 }
    , b = { bar = 2, foo = 1, baz = 3 }
    }
```

  - Disambiguation by known function argument types (this includes local
    bindings):

```
module A exposing (..)

type alias A =
    { foo : Int, bar : Int, baz : Int }

type alias B =
    { bar : Int, foo : Int, baz : Int }

foo : A -> Bool
foo a =
    True

func : Bool
func =
    -- Must be `A`, because `foo` has type `A -> Bool`
    foo { foo = 1, bar = 2, baz = 3 }
```


## Best Practices for Disambiguation

Type annotations are always useful! If all functions have type annotations (with
the appropriate aliases), then it's unlikely ambiguous records will ever be
encountered. Beyond that, ambiguity can always be avoided by just making the
canonical order for possibly-ambiguous records identical.

If you want to ensure that this rule is not encountering ambiguous/unknown
records, then you can use `reportAmbiguousRecordsWithoutFix` and/or
`reportUnknownRecordsWithoutFix` to report them without automatically sorting
them alphabetically. Alternately, you can use `doNotSortAmbiguousRecords` and/or
`doNotSortUnknownRecords` to disable all sorting/error reporting for them.


## When (not) to enable this rule

This rule is useful when you want to ensure that your record fields are in a
consistent, predictable order, that is consistent with the order in which they
were defined.

This rule is not useful when you want to be able to write records in different
orders throughout your codebase, e.g. if you want to emphasize what fields are
most important at any given point. It may also not be useful if you have many
records with the same fields.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example --rules NoUnsortedRecords
```

-}
rule : RuleConfig -> Rule
rule config =
    Rule.newProjectRuleSchema "NoUnsortedRecords" initialProjectContext
        |> Rule.withDependenciesProjectVisitor (\d c -> ( [], dependencyVisitor config c d ))
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
`reportAmbiguousRecordsWithoutFix`, `doNotSortUnknownRecords`, etc. to alter it.
-}
type RuleConfig
    = RuleConfig
        { sortUnknown : SortWithoutCanonicalOrder
        , sortAmbiguous : SortWithoutCanonicalOrder
        , sortGenerics : SortGenerics
        , subrecordTreatment : SubrecordCanonicity
        , typecheckUnambiguousRecords : Bool
        }


{-| Specify how to deal with subrecords.

  - `CanonicalWhenSubrecord` -- Subrecords have canonical order only when they
    are part of their larger record/constructor.
  - `CustomTypeArgsAlwaysCanonical` -- Arguments of custom types are always
    canonical, but other subrecords are only canonical in place. This was the
    behavior prior to 1.1.0.
  - `AlwaysUnknown` -- Always consider subrecords unknown records (unless of
    course they match something else).
  - `AlwaysCanonical` -- Check for standalone subrecords as a lower-priority
    "known" record.

-}
type SubrecordCanonicity
    = CanonicalWhenSubrecord
    | CustomTypeArgsAlwaysCanonical
    | AlwaysUnknown
    | AlwaysCanonical


{-| Specify how to handle generic records.
-}
type SortGenerics
    = GenericFieldsFirst
    | GenericFieldsLast


{-| Specify how to handle records without a canonical order.
-}
type SortWithoutCanonicalOrder
    = Alphabetically
    | ReportOnly
    | DoNotSort


{-| The default configuration, with the following behavior:

  - Unknown records (those that match no known canonical order) are sorted
    alphabetically
  - Ambiguous records (those that match more than one canonical order) are
    sorted alphabetically
  - Generic fields of generic records are sorted before the canonical ones.
  - Subrecords are treated as having canonical order only when associated with
    their outer record/constructor.
  - Typechecking is only used to disambiguate records, i.e. a record will not
    _not_ match a canonical record just because the rule thinks it has the wrong
    type. For instance, `{ foo = 1, bar = 2 }` will match
    `{ foo : String, bar : String }` if no other records exist with the fields
    `foo` and `bar`. This is to protect against incorrect type inference by this
    rule.

Use `reportUnknownRecordsWithoutFix`, etc. to alter this behavior, e.g.

    config =
        [ NoUnsortedRecords.rule
            (NoUnsortedRecords.defaults
                |> NoUnsortedRecords.reportAmbiguousRecordsWithoutFix
            )
        ]

-}
defaults : RuleConfig
defaults =
    RuleConfig
        { sortUnknown = Alphabetically
        , sortAmbiguous = Alphabetically
        , sortGenerics = GenericFieldsFirst
        , subrecordTreatment = CanonicalWhenSubrecord
        , typecheckUnambiguousRecords = False
        }


{-| By default, typechecking is only used to disambiguate records. This alters
that behavior to typecheck _all_ records. For instance, this will force
`{ foo = 1, bar = 2 }` to be an "unknown" record if
`{ foo : String, bar : String }` is known. This should probably be left turned
off, unless you wish to help find examples of incorrect type inference by this
rule.
-}
typecheckAllRecords : RuleConfig -> RuleConfig
typecheckAllRecords (RuleConfig r) =
    RuleConfig { r | typecheckUnambiguousRecords = True }


{-| By default, anonymous records within known records and within custom type
constructors are sorted by their declaration order when encountered in the
context of their outer record/constructor. This disables that behavior,
treating them the same as any other unknown record.

For example:

    type A
        = A { foo : Int, bar : Int, baz : Int }

    type alias Rec =
        { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

    thisWillBeUnknown =
        A { bar = 1, baz = 2, foo = 3 }

    and =
        { yi =
            -- This will also be unknown
            { bar = 1, baz = 2, foo = 3 }
        , er = 1
        }

-}
treatSubrecordsAsUnknown : RuleConfig -> RuleConfig
treatSubrecordsAsUnknown (RuleConfig r) =
    RuleConfig { r | subrecordTreatment = AlwaysUnknown }


{-| By default, anonymous records within known records and within custom type
constructors are sorted by their declaration order when encountered in the
context of their outer record/constructor. This extends that behavior to sort
custom type args even when encountered alone (i.e. not in the context of their
constructor. This was the behavior prior to version `1.1.0` and thus this
setting is provided for compatibility. Note that canonical records will always
take priority, however.

For example:

    type A
        = A { foo : Int, bar : Int, baz : Int }

    thisWillHaveCanonicalOrder =
        -- Even though it does not appear in the context of `A`
        { foo = 3, bar = 1, baz = 2 }

-}
treatCustomTypeRecordsAsCanonical : RuleConfig -> RuleConfig
treatCustomTypeRecordsAsCanonical (RuleConfig r) =
    RuleConfig { r | subrecordTreatment = CustomTypeArgsAlwaysCanonical }


{-| By default, anonymous records within known records and within custom type
constructors are sorted by their declaration order when encountered in the
context of their outer record/constructor. This extends that behavior to sort
them even when encountered alone (i.e. not in the context of their parent
record/constructor. Note that canonical records will always take priority,
however.

For example:

    type alias Rec =
        { yi : { foo : Int, bar : Int, baz : Int }, er : Int }

    thisWillHaveCanonicalOrder =
        -- Even though it does not appear in the context of `Rec`
        { foo = 3, bar = 1, baz = 2 }

-}
treatAllSubrecordsAsCanonical : RuleConfig -> RuleConfig
treatAllSubrecordsAsCanonical (RuleConfig r) =
    RuleConfig { r | subrecordTreatment = AlwaysCanonical }


{-| By default, records that do not match any known aliases or custom types are
sorted alphabetically. This disables that behavior, leaving them in their base
sorting.
-}
doNotSortUnknownRecords : RuleConfig -> RuleConfig
doNotSortUnknownRecords (RuleConfig r) =
    RuleConfig { r | sortUnknown = DoNotSort }


{-| By default, records that match multiple known aliases with different field
orders are sorted alphabetically. (If the field orders of the various matches
are identical, then it is not ambiguous.) This disables that behavior, leaving
them in their base sorting instead.
-}
doNotSortAmbiguousRecords : RuleConfig -> RuleConfig
doNotSortAmbiguousRecords (RuleConfig r) =
    RuleConfig { r | sortAmbiguous = DoNotSort }


{-| By default, records that do not match any known aliases or custom types are
sorted alphabetically. This disables that behavior, reporting them as unknown
without automatically fixing them.

Note that this will effectively forbid the use of _ad hoc_/anonymous records!

-}
reportUnknownRecordsWithoutFix : RuleConfig -> RuleConfig
reportUnknownRecordsWithoutFix (RuleConfig r) =
    RuleConfig { r | sortUnknown = ReportOnly }


{-| By default, records that match multiple known aliases with different field
orders are sorted alphabetically. (If the field orders of the various matches
are identical, then it is not ambiguous.) This disables that behavior, reporting
them as ambiguous without automatically fixing them. This is useful if you want
to catch ambiguous records and e.g. provide type annotations to make them
unambiguous.
-}
reportAmbiguousRecordsWithoutFix : RuleConfig -> RuleConfig
reportAmbiguousRecordsWithoutFix (RuleConfig r) =
    RuleConfig { r | sortAmbiguous = ReportOnly }


{-| By default, generic fields are placed before others; this alters that
behavior to place them at the end instead, e.g.

    type alias A =
        { z : Int, y : Int, x : Int }

    type alias Generic record =
        { record | foo : Int, bar : Int, baz : Int }

    rec : Generic A
    rec =
        { foo = 1, bar = 2, baz = 3, z = 4, y = 5, x = 6 }

-}
sortGenericFieldsLast : RuleConfig -> RuleConfig
sortGenericFieldsLast (RuleConfig r) =
    RuleConfig { r | sortGenerics = GenericFieldsLast }


{-| A canonical ordering of fields in a record definition.
-}
type alias KnownRecord =
    { order : Dict String ( Int, Type )
    , isGeneric : Bool
    , isSubrecord : Bool
    }


{-| A single field of a record, to check for sorting.
-}
type alias Field =
    { field : String, type_ : Maybe DereferencedType, range : Range }


{-| A record type, expression, or pattern, packaged in a standardized format for
checking.
-}
type alias RecordToCheck =
    { fullRange : Range
    , orderInfo : Maybe OrderInfo
    , fields : List Field
    }


{-| Define a canonical ordering (and type) for fields.
-}
type FieldOrder
    = FieldOrder
        { canonical : Dict String ( Int, Type )
        , generic : Maybe Generic
        }


{-| The type of generic fields in a field order, which may be unknown or have
canonical ordering.
-}
type Generic
    = UnknownFields (List String)
    | OrderedFields FieldOrder


{-| Any info we can glean about the record that might help us figure out what
its field order should be.
-}
type OrderInfo
    = HasFieldOrder FieldOrder
    | HasTheseFields { fields : Dict String DereferencedType, hasAllFields : Bool }
    | HasAllFields


{-| A type that, after being dereferenced, will not contain aliases and stores
whether or not records have canonical field order.

Note that `TypeVar` is never concrete, as a `Type` is always made with full info
about all type variables, so the only way `TypeVar` can exist is if it's not
used.

-}
type Type
    = FunctionType { from : Type, to : Type }
    | TupleType (List Type)
    | ListType Type
    | UnitType
    | NamedType ( ModuleName, String ) (List Type)
    | RecordType { generic : Maybe Type, canonical : Bool, fields : List ( String, Type ) }
    | TypeVar (Maybe Typeclass) String


{-| Represent an Elm "typeclass" (constrained type variable, like `number`).
-}
type Typeclass
    = Appendable
    | Number
    | Comparable
    | CompAppend


{-| A type with all aliases dereferenced.
-}
type DereferencedType
    = DereferencedType Type


{-| A type with positional type variables, e.g. `Ok a` having been created from
`Result a b`.
-}
type TypeWithPositionalVars
    = TypeWithPositionalVars Type


{-| The project context.

`aliases` stores all type aliases.

`canonicalRecords` stores known aliased records.

`constructors` are different than functions, as they (might) require type
vars.

`functionTypes` stores the types of all functions (for type inference).

-}
type alias ProjectContext =
    { aliases : Dict ModuleName (Dict String TypeWithPositionalVars)
    , canonicalRecords : Dict ModuleName (Dict String KnownRecord)
    , constructors : Dict ModuleName (Dict String { customTypeName : Maybe String, type_ : TypeWithPositionalVars })
    , functionTypes : Dict ModuleName (Dict String Type)
    }


{-| The module context.

`aliases` stores all type aliases.

`canonicalRecords` stores known aliased records.

`constructors` are different than functions, as they (might) require type
vars.

`functionTypes` stores the types of all functions (for type inference), along
with any (anonymous) records associated with custom types (by index).

-}
type alias ModuleContext =
    { aliases : Dict ModuleName (Dict String TypeWithPositionalVars)
    , canonicalRecords : Dict ModuleName (Dict String KnownRecord)
    , constructors : Dict ModuleName (Dict String { customTypeName : Maybe String, type_ : TypeWithPositionalVars })
    , functionTypes : Dict ModuleName (Dict String Type)
    , exposed :
        { aliases : Dict String TypeWithPositionalVars
        , canonicalRecords : Dict String KnownRecord
        , constructors : Dict String { customTypeName : Maybe String, type_ : TypeWithPositionalVars }
        , functionTypes : Dict String Type
        }
    , moduleName : ModuleName
    , fileIsIgnored : Bool
    , lookupTable : ModuleNameLookupTable
    , extractSource : Range -> String
    }


{-| Context within an expression, which may have additional bindings from `let`
declarations and patterns.
-}
type alias LocalContext =
    { context : ModuleContext
    , localFunctions : Dict String Type
    }


{-| Visit each module, first getting type aliases from all declarations and then
checking all expressions for records.
-}
moduleVisitor : RuleConfig -> Rule.ModuleRuleSchema r ModuleContext -> Rule.ModuleRuleSchema { r | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withDeclarationEnterVisitor
            (\d c ->
                if c.fileIsIgnored then
                    ( [], c )

                else
                    ( declarationEnterVisitor config c d, c )
            )


{-| The initial project context knows of no types.
-}
initialProjectContext : ProjectContext
initialProjectContext =
    { aliases = Dict.empty
    , canonicalRecords = Dict.empty
    , constructors = Dict.empty
    , functionTypes = Dict.empty
    }


{-| Create a `ProjectContext` from a `ModuleContext`, keeping only exposed
functions (since unexposed won't be relevant out of the module), and anything
exposed by dependencies.
-}
fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName ({ exposed } as context) ->
            { aliases =
                if Dict.isEmpty exposed.aliases then
                    Dict.remove moduleName context.aliases

                else
                    Dict.insert moduleName exposed.aliases context.aliases
            , canonicalRecords =
                if Dict.isEmpty exposed.canonicalRecords then
                    Dict.remove moduleName context.canonicalRecords

                else
                    Dict.insert moduleName exposed.canonicalRecords context.canonicalRecords
            , constructors =
                if Dict.isEmpty exposed.constructors then
                    Dict.remove moduleName context.constructors

                else
                    Dict.insert moduleName exposed.constructors context.constructors
            , functionTypes =
                if Dict.isEmpty exposed.functionTypes then
                    Dict.remove moduleName context.functionTypes

                else
                    Dict.insert moduleName exposed.functionTypes context.functionTypes
            }
        )
        |> Rule.withModuleName


{-| Create a `ModuleContext` from a `ProjectContext`.
-}
fromProjectToModule : RuleConfig -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule config =
    Rule.initContextCreator
        (\lookupTable sourceCodeExtractor moduleName fileIsIgnored { moduleDefinition, declarations } projectContext ->
            let
                { aliases, canonicalRecords, constructors, functionTypes, exposed } =
                    declarationListVisitor config
                        { moduleName = moduleName
                        , lookupTable = lookupTable
                        , aliases = projectContext.aliases
                        , canonicalRecords = projectContext.canonicalRecords
                        , constructors = projectContext.constructors
                        , functionTypes = projectContext.functionTypes
                        , exposingList = getExposedNames <| Node.value moduleDefinition
                        , fileIsIgnored = fileIsIgnored
                        }
                        declarations
            in
            { aliases = aliases
            , canonicalRecords = canonicalRecords
            , constructors = constructors
            , functionTypes = functionTypes
            , exposed = exposed
            , fileIsIgnored = fileIsIgnored
            , moduleName = moduleName
            , lookupTable = lookupTable
            , extractSource = sourceCodeExtractor
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withSourceCodeExtractor
        |> Rule.withModuleName
        |> Rule.withIsFileIgnored
        |> Rule.withFullAst


{-| Combine `ProjectContext`s by taking the union of known type info.
-}
foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext prevContext =
    { aliases =
        Dict.union newContext.aliases prevContext.aliases
    , canonicalRecords =
        Dict.union newContext.canonicalRecords prevContext.canonicalRecords
    , constructors =
        Dict.union newContext.constructors prevContext.constructors
    , functionTypes =
        Dict.union newContext.functionTypes prevContext.functionTypes
    }


{-| Return whether or not subrecords should be considered canonical, for
creating the type of a full record.
-}
subrecordCanonicityForRecord : SubrecordCanonicity -> Maybe Bool
subrecordCanonicityForRecord subrecordTreatment =
    case subrecordTreatment of
        CanonicalWhenSubrecord ->
            Just True

        CustomTypeArgsAlwaysCanonical ->
            Just True

        AlwaysUnknown ->
            Just False

        AlwaysCanonical ->
            Just True


{-| Return whether or not subrecords should be considered canonical, for
creating the type of a field or argument.
-}
subrecordCanonicityForField : SubrecordCanonicity -> Maybe Bool
subrecordCanonicityForField subrecordTreatment =
    case subrecordTreatment of
        CanonicalWhenSubrecord ->
            Just True

        CustomTypeArgsAlwaysCanonical ->
            Just True

        AlwaysUnknown ->
            Nothing

        AlwaysCanonical ->
            Just True


{-| Visit all dependencies and store type order from them.
-}
dependencyVisitor : RuleConfig -> ProjectContext -> Dict String Dependency -> ProjectContext
dependencyVisitor (RuleConfig { subrecordTreatment }) =
    let
        step : Elm.Docs.Module -> ProjectContext -> ProjectContext
        step ({ aliases, binops, unions, values } as mod) acc =
            let
                recordFromTypeAlias : Elm.Docs.Alias -> ( List ( String, KnownRecord ), List ( String, { customTypeName : Maybe String, type_ : TypeWithPositionalVars } ) )
                recordFromTypeAlias { name, args, tipe } =
                    case tipe of
                        Elm.Type.Record fields Nothing ->
                            ( knownRecordFromDocType subrecordTreatment moduleName ( fields, False )
                                |> List.map (Tuple.mapFirst (\s -> name ++ s))
                            , [ ( name
                                , { customTypeName = Nothing
                                  , type_ =
                                        List.map
                                            (Tuple.second
                                                >> docTypeToTypeWithPositionalVars moduleName
                                                    -- Constrained type vars do apply to record constructors
                                                    { constrainedTypeVarsAreRespected = True

                                                    -- Arguments only have order if config says so
                                                    , subrecordIsAlsoCanonical = subrecordCanonicityForField subrecordTreatment
                                                    }
                                                    args
                                            )
                                            fields
                                            |> makeFunctionTypeWithPositionalVars
                                                (docTypeToTypeWithPositionalVars moduleName
                                                    { constrainedTypeVarsAreRespected = True

                                                    -- Return type *does* have order
                                                    , subrecordIsAlsoCanonical = subrecordCanonicityForRecord subrecordTreatment
                                                    }
                                                    args
                                                    tipe
                                                )
                                  }
                                )
                              ]
                            )

                        Elm.Type.Record fields (Just _) ->
                            ( knownRecordFromDocType subrecordTreatment moduleName ( fields, True )
                                |> List.map (Tuple.mapFirst (\s -> name ++ s))
                            , -- No constructors for generic records
                              []
                            )

                        _ ->
                            ( [], [] )

                makeAlias : Elm.Docs.Alias -> ( String, TypeWithPositionalVars )
                makeAlias { name, args, tipe } =
                    ( name
                    , docTypeToTypeWithPositionalVars moduleName
                        -- Constrained type vars aren't respected for type aliases
                        { constrainedTypeVarsAreRespected = False
                        , subrecordIsAlsoCanonical = subrecordCanonicityForRecord subrecordTreatment
                        }
                        args
                        tipe
                    )

                makeConstructor : TypeWithPositionalVars -> List String -> ( String, List Elm.Type.Type ) -> ( String, TypeWithPositionalVars, List ( String, KnownRecord ) )
                makeConstructor return typeVars ( name, arguments ) =
                    List.map
                        (docTypeToTypeWithPositionalVars moduleName
                            { constrainedTypeVarsAreRespected = True

                            -- Arguments only have order if config says so
                            , subrecordIsAlsoCanonical =
                                subrecordCanonicityForField subrecordTreatment
                            }
                            typeVars
                        )
                        arguments
                        |> (\ts ->
                                ( name
                                , makeFunctionTypeWithPositionalVars return ts
                                , makeCustomTypeSubrecords subrecordTreatment name ts
                                )
                           )

                functionsFromCustomType : Elm.Docs.Union -> ( List ( String, { customTypeName : Maybe String, type_ : TypeWithPositionalVars } ), List ( String, KnownRecord ) )
                functionsFromCustomType { name, args, tags } =
                    List.foldl
                        (\t ( fAcc, rAcc ) ->
                            makeConstructor
                                (List.map (TypeVar Nothing) args
                                    |> NamedType ( moduleName, name )
                                    |> DereferencedType
                                    |> assignTypeVars (makePositionalArgTypeVars args)
                                    |> getType
                                    |> TypeWithPositionalVars
                                )
                                args
                                t
                                |> (\( n, type_, rs ) ->
                                        ( ( n, { customTypeName = Just name, type_ = type_ } ) :: fAcc
                                        , rs ++ rAcc
                                        )
                                   )
                        )
                        ( [], [] )
                        tags

                functionFromValue : Elm.Docs.Value -> ( String, Type )
                functionFromValue { name, tipe } =
                    docTypeToType moduleName
                        { constrainedTypeVarsAreRespected = True
                        , subrecordIsAlsoCanonical = Nothing
                        }
                        tipe
                        |> Tuple.pair name

                functionFromOperator : Elm.Docs.Binop -> ( String, Type )
                functionFromOperator { name, tipe } =
                    docTypeToType moduleName
                        { constrainedTypeVarsAreRespected = True
                        , subrecordIsAlsoCanonical = Nothing
                        }
                        tipe
                        |> Tuple.pair name

                moduleName : ModuleName
                moduleName =
                    String.split "." mod.name

                newAliases : Maybe ( ModuleName, Dict String TypeWithPositionalVars )
                newAliases =
                    List.map makeAlias aliases
                        |> validate (not << List.isEmpty)
                        |> Maybe.map Dict.fromList
                        |> Maybe.map (Tuple.pair moduleName)

                ( newAliasRecords, newRecordConstructors ) =
                    List.map recordFromTypeAlias aliases
                        |> List.unzip
                        |> Tuple.mapBoth (Dict.fromList << List.concat) (Dict.fromList << List.concat)

                ( newConstructors, newRecords ) =
                    List.foldl
                        (\u ( fAcc, rAcc ) ->
                            functionsFromCustomType u
                                |> (\( fs, rs ) -> ( fs ++ fAcc, rs ++ rAcc ))
                        )
                        ( [], [] )
                        unions
                        |> Tuple.mapBoth
                            (Dict.fromList
                                >> Dict.union newRecordConstructors
                                >> validate (not << Dict.isEmpty)
                                >> Maybe.map (Tuple.pair moduleName)
                            )
                            (Dict.fromList
                                >> Dict.union newAliasRecords
                                >> validate (not << Dict.isEmpty)
                                >> Maybe.map (Tuple.pair moduleName)
                            )

                newOperators : Dict String Type
                newOperators =
                    List.map functionFromOperator binops
                        |> Dict.fromList

                newFunctions : Maybe ( ModuleName, Dict String Type )
                newFunctions =
                    List.map functionFromValue values
                        |> Dict.fromList
                        |> Dict.union newOperators
                        |> validate (not << Dict.isEmpty)
                        |> Maybe.map (Tuple.pair moduleName)
            in
            { acc
                | aliases = MaybeX.unwrap acc.aliases (\( k, v ) -> Dict.insert k v acc.aliases) newAliases
                , canonicalRecords = MaybeX.unwrap acc.canonicalRecords (\( k, v ) -> Dict.insert k v acc.canonicalRecords) newRecords
                , constructors = MaybeX.unwrap acc.constructors (\( k, v ) -> Dict.insert k v acc.constructors) newConstructors
                , functionTypes = MaybeX.unwrap acc.functionTypes (\( k, v ) -> Dict.insert k v acc.functionTypes) newFunctions
            }
    in
    Dict.foldl
        (\_ dep acc ->
            Dependency.modules dep
                |> List.foldl step initialProjectContext
                |> (\{ aliases, canonicalRecords, constructors, functionTypes } ->
                        { acc
                            | aliases = Dict.union aliases acc.aliases
                            , canonicalRecords = Dict.union canonicalRecords acc.canonicalRecords
                            , constructors = Dict.union constructors acc.constructors
                            , functionTypes = Dict.union functionTypes acc.functionTypes
                        }
                   )
        )


{-| Given the current module name, whether or not a top-level record type (if
`Just`) and subrecords (if `Just True`) found are in canonical order, and a dict
of positional type vars, convert an `Elm.Type.Type` to a
`TypeWithPositionalVars`.
-}
docTypeToTypeWithPositionalVars : ModuleName -> { constrainedTypeVarsAreRespected : Bool, subrecordIsAlsoCanonical : Maybe Bool } -> List String -> Elm.Type.Type -> TypeWithPositionalVars
docTypeToTypeWithPositionalVars moduleName settings typeArgs =
    docTypeToType moduleName settings
        >> DereferencedType
        >> assignTypeVars (makePositionalArgTypeVars typeArgs)
        >> getType
        >> TypeWithPositionalVars


{-| Given how to treat subrecords, the current module names, and a list of
fields/whether the record is generic, generate all `KnownRecord`s from a
`Elm.Type.Type`.
-}
knownRecordFromDocType : SubrecordCanonicity -> ModuleName -> ( List ( String, Elm.Type.Type ), Bool ) -> List ( String, KnownRecord )
knownRecordFromDocType subrecordTreatment moduleName ( fields, isGeneric ) =
    ListX.indexedFoldl
        (\i ( f, t ) ->
            Dict.insert f
                ( i
                , docTypeToType moduleName
                    { -- Constrained type vars don't apply to type aliases
                      constrainedTypeVarsAreRespected = False

                    -- Subrecords canonical if config says so
                    , subrecordIsAlsoCanonical = subrecordCanonicityForField subrecordTreatment
                    }
                    t
                )
        )
        Dict.empty
        fields
        |> (\order ->
                { order = order
                , isGeneric = isGeneric
                , isSubrecord = False
                }
           )
        |> (\k ->
                if subrecordTreatment == AlwaysCanonical then
                    ( "", k )
                        :: List.concatMap (\( f, ( _, t ) ) -> makeSubrecordsFromType True ("." ++ f) t) (Dict.toList k.order)

                else
                    -- Only create the top level record
                    [ ( "", k ) ]
           )


{-| Given whether or not to descend beyond the initial type, a name prefix and a
type, generate `KnownRecord`s for records in that type.
-}
makeSubrecordsFromType : Bool -> String -> Type -> List ( String, KnownRecord )
makeSubrecordsFromType recurse namePrefix type_ =
    let
        go : Maybe String -> Type -> List ( String, KnownRecord )
        go s =
            if recurse then
                makeSubrecordsFromType recurse (MaybeX.unwrap namePrefix (\subRec -> namePrefix ++ subRec) s)

            else
                always []
    in
    case type_ of
        FunctionType { from, to } ->
            go Nothing from ++ go Nothing to

        TupleType ts ->
            List.concatMap (go Nothing) ts

        ListType t_ ->
            go Nothing t_

        RecordType { fields, generic } ->
            ( namePrefix
            , { order =
                    ListX.indexedFoldl
                        (\i ( f, t ) ->
                            Dict.insert f
                                ( i
                                , t
                                )
                        )
                        Dict.empty
                        fields
              , isGeneric = generic /= Nothing
              , isSubrecord = True
              }
            )
                :: List.concatMap (\( f, t ) -> go (Just <| "." ++ f) t) fields

        _ ->
            -- UnitType
            -- TypeVar
            -- NamedType
            []


{-| Given the current module name and whether or not a top-level record type (if
`Just`) and subrecords (if `Just True`) found are in canonical order, convert an
`Elm.Type.Type` to a `Type`.
-}
docTypeToType : ModuleName -> { constrainedTypeVarsAreRespected : Bool, subrecordIsAlsoCanonical : Maybe Bool } -> Elm.Type.Type -> Type
docTypeToType moduleName ({ constrainedTypeVarsAreRespected, subrecordIsAlsoCanonical } as settings) type_ =
    let
        go : Elm.Type.Type -> Type
        go =
            MaybeX.filter identity subrecordIsAlsoCanonical
                |> (\subrecordStillCanon ->
                        docTypeToType moduleName { settings | subrecordIsAlsoCanonical = subrecordStillCanon }
                   )

        makeList : ModuleName -> String -> List Elm.Type.Type -> Maybe Type
        makeList mod name args =
            case ( mod, name, args ) of
                ( [ "List" ], "List", [ listType ] ) ->
                    Just <| ListType <| go listType

                _ ->
                    Nothing
    in
    case type_ of
        Elm.Type.Lambda from to ->
            FunctionType { from = go from, to = go to }

        Elm.Type.Tuple ts ->
            TupleType <| List.map go ts

        Elm.Type.Type qualified args ->
            -- Can't use module name lookup, so just have to hope this is right.
            String.split "." qualified
                |> ListX.unconsLast
                |> Maybe.map
                    (\( n, m ) ->
                        if m == [] then
                            ( moduleName, n )

                        else
                            ( m, n )
                    )
                |> Maybe.withDefault ( moduleName, qualified )
                |> (\( mod, name ) ->
                        makeList mod name args
                            |> MaybeX.withDefaultLazy (\() -> NamedType ( mod, name ) <| List.map go args)
                   )

        Elm.Type.Record fields generic ->
            RecordType
                { generic =
                    -- Generic records completely ignore typeclasses, i.e.
                    -- `type alias G comparable = { comparable | x : Int }`
                    -- is just a normal generic record.
                    Maybe.map (TypeVar Nothing) generic
                , canonical = subrecordIsAlsoCanonical /= Nothing
                , fields = List.map (Tuple.mapSecond go) fields
                }

        Elm.Type.Var s ->
            makeTypeVar constrainedTypeVarsAreRespected s


{-| Make a `TypeVar` from a string.
-}
makeTypeVar : Bool -> String -> Type
makeTypeVar constrainedTypeVarsAreRespected s =
    (if String.startsWith "number" s then
        Just Number

     else if String.startsWith "appendable" s then
        Just Appendable

     else if String.startsWith "comparable" s then
        Just Comparable

     else if String.startsWith "compappend" s then
        Just CompAppend

     else
        Nothing
    )
        -- Sometimes constrained type variables are not respected, like in type aliases
        |> MaybeX.filter (always <| constrainedTypeVarsAreRespected)
        |> (\t -> TypeVar t s)


{-| Convert a type annotation into a record definition (and whether or not the
record is generic) if it is one.
-}
annotToFields : Node TypeAnnotation -> Maybe ( RecordDefinition, Bool )
annotToFields annot =
    case Node.value annot of
        TypeAnnotation.Record fields ->
            Just ( fields, False )

        GenericRecord _ fields ->
            Just ( Node.value fields, True )

        _ ->
            Nothing


{-| Make subrecords for custom types, if appropriate.
-}
makeCustomTypeSubrecords : SubrecordCanonicity -> String -> List TypeWithPositionalVars -> List ( String, KnownRecord )
makeCustomTypeSubrecords subrecordTreatment n ts =
    (case subrecordTreatment of
        AlwaysUnknown ->
            Nothing

        CanonicalWhenSubrecord ->
            Nothing

        CustomTypeArgsAlwaysCanonical ->
            Just False

        AlwaysCanonical ->
            Just True
    )
        |> MaybeX.unwrap []
            (\recurse ->
                List.indexedMap
                    (\i t ->
                        getTypeWithPositionalVars t
                            |> makeSubrecordsFromType recurse (n ++ " arg" ++ String.fromInt i)
                    )
                    ts
                    |> List.concat
            )


{-| Information on what functions, types/aliases, and fully-exposed types with constructors are exposed by the module.
-}
type alias ExposedNames =
    { functions : Set String
    , types : Set String
    , openTypes : Set String
    }


{-| Get a set of all names exposed by the modules or `Nothing` if everything is
exposed.
-}
getExposedNames : Module -> Maybe ExposedNames
getExposedNames =
    let
        step : Node TopLevelExpose -> ExposedNames -> ExposedNames
        step e acc =
            case Node.value e of
                FunctionExpose name ->
                    { acc | functions = Set.insert name acc.functions }

                TypeExpose { name } ->
                    { acc | openTypes = Set.insert name acc.openTypes }

                InfixExpose name ->
                    { acc | functions = Set.insert name acc.functions }

                TypeOrAliasExpose name ->
                    { acc | types = Set.insert name acc.types }
    in
    Module.exposingList
        >> (\l ->
                case l of
                    All _ ->
                        Nothing

                    Explicit es ->
                        List.foldl step
                            { functions = Set.empty, types = Set.empty, openTypes = Set.empty }
                            es
                            |> Just
           )


{-| Visit declarations, storing record field orders.
-}
declarationListVisitor :
    RuleConfig
    ->
        { moduleName : ModuleName
        , lookupTable : ModuleNameLookupTable
        , aliases : Dict ModuleName (Dict String TypeWithPositionalVars)
        , canonicalRecords : Dict ModuleName (Dict String KnownRecord)
        , constructors : Dict ModuleName (Dict String { customTypeName : Maybe String, type_ : TypeWithPositionalVars })
        , functionTypes : Dict ModuleName (Dict String Type)
        , exposingList : Maybe ExposedNames
        , fileIsIgnored : Bool
        }
    -> List (Node Declaration)
    ->
        { aliases : Dict ModuleName (Dict String TypeWithPositionalVars)
        , canonicalRecords : Dict ModuleName (Dict String KnownRecord)
        , constructors : Dict ModuleName (Dict String { customTypeName : Maybe String, type_ : TypeWithPositionalVars })
        , functionTypes : Dict ModuleName (Dict String Type)
        , exposed :
            { aliases : Dict String TypeWithPositionalVars
            , canonicalRecords : Dict String KnownRecord
            , constructors : Dict String { customTypeName : Maybe String, type_ : TypeWithPositionalVars }
            , functionTypes : Dict String Type
            }
        }
declarationListVisitor (RuleConfig { subrecordTreatment }) context declarations =
    -- Find aliases, canonical records, and function types and store them
    List.foldl (accumulateDeclarationInfo subrecordTreatment context)
        { aliases = []
        , canonicalRecords = []
        , constructors = []
        , functionTypes = []
        , exposedAliases = []
        , exposedCanonicalRecords = []
        , exposedConstructors = []
        , exposedFunctionTypes = []
        }
        declarations
        |> (\r ->
                if context.fileIsIgnored then
                    { aliases = context.aliases
                    , canonicalRecords = context.canonicalRecords
                    , constructors = context.constructors
                    , functionTypes = context.functionTypes
                    , exposed =
                        { aliases =
                            Dict.fromList r.exposedAliases
                        , canonicalRecords =
                            Dict.fromList r.exposedCanonicalRecords
                        , constructors =
                            Dict.fromList r.exposedConstructors
                        , functionTypes =
                            Dict.fromList r.exposedFunctionTypes
                        }
                    }

                else
                    { aliases =
                        validate (not << List.isEmpty) r.aliases
                            |> Maybe.map Dict.fromList
                            |> MaybeX.unwrap context.aliases (\v -> Dict.insert context.moduleName v context.aliases)
                    , canonicalRecords =
                        validate (not << List.isEmpty) r.canonicalRecords
                            |> Maybe.map Dict.fromList
                            |> MaybeX.unwrap context.canonicalRecords (\v -> Dict.insert context.moduleName v context.canonicalRecords)
                    , constructors =
                        validate (not << List.isEmpty) r.constructors
                            |> Maybe.map Dict.fromList
                            |> MaybeX.unwrap context.constructors (\v -> Dict.insert context.moduleName v context.constructors)
                    , functionTypes =
                        validate (not << List.isEmpty) r.functionTypes
                            |> Maybe.map Dict.fromList
                            |> MaybeX.unwrap context.functionTypes (\v -> Dict.insert context.moduleName v context.functionTypes)
                    , exposed =
                        { aliases =
                            Dict.fromList r.exposedAliases
                        , canonicalRecords =
                            Dict.fromList r.exposedCanonicalRecords
                        , constructors =
                            Dict.fromList r.exposedConstructors
                        , functionTypes =
                            Dict.fromList r.exposedFunctionTypes
                        }
                    }
           )


{-| Given a top-level declaration, accumulate information from it for storing in
module context, determining what to expose or not.
-}
accumulateDeclarationInfo :
    SubrecordCanonicity
    ->
        { context
            | moduleName : ModuleName
            , lookupTable : ModuleNameLookupTable
            , exposingList : Maybe ExposedNames
            , fileIsIgnored : Bool
        }
    -> Node Declaration
    ->
        { aliases : List ( String, TypeWithPositionalVars )
        , canonicalRecords : List ( String, KnownRecord )
        , constructors : List ( String, { customTypeName : Maybe String, type_ : TypeWithPositionalVars } )
        , functionTypes : List ( String, Type )
        , exposedAliases : List ( String, TypeWithPositionalVars )
        , exposedCanonicalRecords : List ( String, KnownRecord )
        , exposedConstructors : List ( String, { customTypeName : Maybe String, type_ : TypeWithPositionalVars } )
        , exposedFunctionTypes : List ( String, Type )
        }
    ->
        { aliases : List ( String, TypeWithPositionalVars )
        , canonicalRecords : List ( String, KnownRecord )
        , constructors : List ( String, { customTypeName : Maybe String, type_ : TypeWithPositionalVars } )
        , functionTypes : List ( String, Type )
        , exposedAliases : List ( String, TypeWithPositionalVars )
        , exposedCanonicalRecords : List ( String, KnownRecord )
        , exposedConstructors : List ( String, { customTypeName : Maybe String, type_ : TypeWithPositionalVars } )
        , exposedFunctionTypes : List ( String, Type )
        }
accumulateDeclarationInfo subrecordTreatment context node acc =
    let
        makeConstructorAndSubrecords : TypeWithPositionalVars -> List (Node String) -> ValueConstructor -> ( String, TypeWithPositionalVars, List ( String, KnownRecord ) )
        makeConstructorAndSubrecords return typeVars { name, arguments } =
            List.map
                (typeAnnotToTypeWithPositionalVars context
                    { constrainedTypeVarsAreRespected = True
                    , subrecordIsAlsoCanonical = subrecordCanonicityForField subrecordTreatment
                    }
                    (List.map Node.value typeVars)
                )
                arguments
                |> (\ts ->
                        let
                            n : String
                            n =
                                Node.value name
                        in
                        ( n
                        , makeFunctionTypeWithPositionalVars return ts
                        , makeCustomTypeSubrecords subrecordTreatment n ts
                        )
                   )

        skipIfIgnored : (ExposedNames -> Bool) -> r -> (() -> info) -> (info -> r -> r) -> (info -> r -> r) -> r
        skipIfIgnored checkIfExposed acc_ makeInfo addLocal addExposed =
            let
                isExposed : Bool
                isExposed =
                    MaybeX.unwrap True checkIfExposed context.exposingList
            in
            case ( context.fileIsIgnored, isExposed ) of
                ( True, False ) ->
                    acc_

                ( True, True ) ->
                    addExposed (makeInfo ()) acc_

                ( False, True ) ->
                    let
                        info : info
                        info =
                            makeInfo ()
                    in
                    addExposed info <| addLocal info acc_

                ( False, False ) ->
                    addLocal (makeInfo ()) acc_
    in
    case Node.value node of
        FunctionDeclaration { signature } ->
            Maybe.map Node.value signature
                |> MaybeX.unwrap acc
                    (\{ name, typeAnnotation } ->
                        let
                            n : String
                            n =
                                Node.value name
                        in
                        skipIfIgnored (Set.member n << .functions)
                            acc
                            -- Function declarations do not have canonical record orders nor do they have type variables (that might be made concrete)
                            (\() -> ( n, typeAnnotToNoncanonicalType context typeAnnotation ))
                            (\info acc_ -> { acc_ | functionTypes = info :: acc.functionTypes })
                            (\info acc_ -> { acc_ | exposedFunctionTypes = info :: acc.exposedFunctionTypes })
                    )

        CustomTypeDeclaration { name, generics, constructors } ->
            let
                n : String
                n =
                    Node.value name
            in
            skipIfIgnored (Set.member n << .openTypes)
                acc
                (\() ->
                    List.foldl
                        (\c ( fAcc, rAcc ) ->
                            Node.value c
                                |> makeConstructorAndSubrecords
                                    (List.map Node.value generics
                                        |> (\gs ->
                                                List.map (TypeVar Nothing) gs
                                                    |> NamedType ( [], n )
                                                    |> DereferencedType
                                                    |> assignTypeVars (makePositionalArgTypeVars gs)
                                                    |> getType
                                                    |> TypeWithPositionalVars
                                           )
                                    )
                                    generics
                                |> (\( n_, type_, rs ) ->
                                        ( ( n_
                                          , { customTypeName = Just n
                                            , type_ = type_
                                            }
                                          )
                                            :: fAcc
                                        , rs ++ rAcc
                                        )
                                   )
                        )
                        ( [], [] )
                        constructors
                )
                (\( newConstructors, newRecords ) acc_ ->
                    { acc_
                        | constructors = newConstructors ++ acc.constructors
                        , canonicalRecords = newRecords ++ acc.canonicalRecords
                    }
                )
                (\( newConstructors, newRecords ) acc_ ->
                    { acc_
                        | exposedConstructors = newConstructors ++ acc.exposedConstructors
                        , exposedCanonicalRecords = newRecords ++ acc.exposedCanonicalRecords
                    }
                )

        AliasDeclaration { name, generics, typeAnnotation } ->
            let
                n : String
                n =
                    Node.value name

                aliasInfo : ( String, TypeWithPositionalVars )
                aliasInfo =
                    ( n
                    , typeAnnotToTypeWithPositionalVars context
                        -- Constrained type vars are not respected for aliases
                        { constrainedTypeVarsAreRespected = False
                        , subrecordIsAlsoCanonical = subrecordCanonicityForRecord subrecordTreatment
                        }
                        (List.map Node.value generics)
                        typeAnnotation
                    )
            in
            skipIfIgnored (Set.member n << .types)
                { acc | exposedAliases = aliasInfo :: acc.exposedAliases }
                (\() ->
                    annotToFields typeAnnotation
                        |> MaybeX.unwrap ( [], [] )
                            (\( fields, isGeneric ) ->
                                ( -- Generic records do not have constructors.
                                  if isGeneric then
                                    []

                                  else
                                    [ ( n
                                      , { customTypeName = Nothing
                                        , type_ =
                                            List.map Node.value generics
                                                |> (\vars ->
                                                        List.map
                                                            (Node.value
                                                                >> Tuple.second
                                                                >> typeAnnotToTypeWithPositionalVars context
                                                                    { constrainedTypeVarsAreRespected = True
                                                                    , subrecordIsAlsoCanonical = subrecordCanonicityForField subrecordTreatment
                                                                    }
                                                                    vars
                                                            )
                                                            fields
                                                            |> makeFunctionTypeWithPositionalVars
                                                                (typeAnnotToTypeWithPositionalVars context
                                                                    { constrainedTypeVarsAreRespected = True
                                                                    , subrecordIsAlsoCanonical = subrecordCanonicityForRecord subrecordTreatment
                                                                    }
                                                                    vars
                                                                    typeAnnotation
                                                                )
                                                   )
                                        }
                                      )
                                    ]
                                , knownRecordFromTypeAnnot subrecordTreatment context ( fields, isGeneric )
                                    |> List.map (Tuple.mapFirst (\s -> n ++ s))
                                )
                            )
                )
                (\( newConstructors, newRecords ) acc_ ->
                    { acc_
                        | aliases = aliasInfo :: acc.aliases
                        , constructors = newConstructors ++ acc.constructors
                        , canonicalRecords = newRecords ++ acc.canonicalRecords
                    }
                )
                (\( newConstructors, newRecords ) acc_ ->
                    { acc_
                        | exposedConstructors = newConstructors ++ acc.exposedConstructors
                        , exposedCanonicalRecords = newRecords ++ acc.exposedCanonicalRecords
                    }
                )

        _ ->
            -- Nothing to do for:
            -- PortDeclaration
            -- InfixDeclaration
            -- Destructuring
            acc


{-| Visit each TLD and check it in turn.
-}
declarationEnterVisitor : RuleConfig -> ModuleContext -> Node Declaration -> List (Error {})
declarationEnterVisitor config context node =
    case Node.value node of
        FunctionDeclaration f ->
            checkFunctionDeclaration config { context = context, localFunctions = Dict.empty } f

        _ ->
            []


{-| Check a function declaration for unsorted record signatures, patterns, or
expressions.
-}
checkFunctionDeclaration : RuleConfig -> LocalContext -> Function -> List (Error {})
checkFunctionDeclaration config local func =
    let
        { arguments, expression } =
            Node.value func.declaration

        hasType : Maybe DereferencedType
        hasType =
            getFunctionBinding local.context func
                |> Maybe.map Tuple.second
    in
    Maybe.map (checkTypeAnnotation config local.context Nothing << .typeAnnotation << Node.value) func.signature
        |> Maybe.withDefault []
        |> (\errsInAnnot -> errsInAnnot ++ checkFunctionArgsAndExpr config local hasType arguments expression)


{-| Get the name and type of a function, if possible.
-}
getFunctionBinding : ModuleContext -> Function -> Maybe ( String, DereferencedType )
getFunctionBinding context { signature } =
    Maybe.map Node.value signature
        |> Maybe.map
            (\{ name, typeAnnotation } ->
                ( Node.value name
                , typeAnnotToNoncanonicalType context typeAnnotation
                    |> dereferenceType context
                )
            )


{-| Given a return type and a successive list of argument types, create a
function with that type.
-}
makeFunctionType : Type -> List Type -> Type
makeFunctionType return ts =
    case ts of
        [] ->
            return

        t :: ts_ ->
            FunctionType { from = t, to = makeFunctionType return ts_ }


{-| Given a return type and a successive list of argument types, create a
function with that type, all types having positional vars.
-}
makeFunctionTypeWithPositionalVars : TypeWithPositionalVars -> List TypeWithPositionalVars -> TypeWithPositionalVars
makeFunctionTypeWithPositionalVars return ts =
    makeFunctionType (getTypeWithPositionalVars return) (List.map getTypeWithPositionalVars ts)
        |> TypeWithPositionalVars


{-| Given context, whether or not a top-level record type (if `Just`) and
subrecords (if `Just True`) found are in canonical order, and a `Dict` of
positional type variables, convert a `TypeAnnotation` into a
`TypeWithPositionalVars`.
-}
typeAnnotToTypeWithPositionalVars : { context | moduleName : ModuleName, lookupTable : ModuleNameLookupTable } -> { constrainedTypeVarsAreRespected : Bool, subrecordIsAlsoCanonical : Maybe Bool } -> List String -> Node TypeAnnotation -> TypeWithPositionalVars
typeAnnotToTypeWithPositionalVars context settings typeArgs =
    typeAnnotToType context settings
        >> DereferencedType
        >> assignTypeVars (makePositionalArgTypeVars typeArgs)
        >> getType
        >> TypeWithPositionalVars


{-| Given how to treat subrecords, the current module names, and a
`RecordDefinition`/whether the record is generic, generate all `KnownRecord`s
from a type annotation.
-}
knownRecordFromTypeAnnot : SubrecordCanonicity -> { context | moduleName : ModuleName, lookupTable : ModuleNameLookupTable } -> ( RecordDefinition, Bool ) -> List ( String, KnownRecord )
knownRecordFromTypeAnnot subrecordTreatment context ( fields, isGeneric ) =
    ListX.indexedFoldl
        (\i field ->
            let
                ( f, t ) =
                    Node.value field
            in
            Dict.insert
                (Node.value f)
                ( i
                , typeAnnotToType context
                    -- Constrained type vars are not respected for aliases
                    { constrainedTypeVarsAreRespected = False

                    -- Subrecords canonical if config says so
                    , subrecordIsAlsoCanonical =
                        subrecordCanonicityForField subrecordTreatment
                    }
                    t
                )
        )
        Dict.empty
        fields
        |> (\order -> { order = order, isGeneric = isGeneric, isSubrecord = False })
        |> (\k ->
                if subrecordTreatment == AlwaysCanonical then
                    ( "", k )
                        :: List.concatMap (\( f, ( _, t ) ) -> makeSubrecordsFromType True ("." ++ f) t) (Dict.toList k.order)

                else
                    -- Only create the top level record
                    [ ( "", k ) ]
           )


{-| Wrapper for `typeAnnotToType` when not dealing with aliases.
-}
typeAnnotToNoncanonicalType : { context | moduleName : ModuleName, lookupTable : ModuleNameLookupTable } -> Node TypeAnnotation -> Type
typeAnnotToNoncanonicalType context =
    typeAnnotToType context
        { constrainedTypeVarsAreRespected = True
        , subrecordIsAlsoCanonical = Nothing
        }


{-| Given context and whether or not a top-level record type (if `Just`) and
subrecords (if `Just True`) found are in canonical order, convert a
`TypeAnnotation` into a `Type`.
-}
typeAnnotToType : { context | moduleName : ModuleName, lookupTable : ModuleNameLookupTable } -> { constrainedTypeVarsAreRespected : Bool, subrecordIsAlsoCanonical : Maybe Bool } -> Node TypeAnnotation -> Type
typeAnnotToType context ({ constrainedTypeVarsAreRespected, subrecordIsAlsoCanonical } as settings) annot =
    let
        go : Node TypeAnnotation -> Type
        go =
            MaybeX.filter identity subrecordIsAlsoCanonical
                |> (\subrecordStillCanon ->
                        typeAnnotToType context { settings | subrecordIsAlsoCanonical = subrecordStillCanon }
                   )

        makeList : ModuleName -> String -> List (Node TypeAnnotation) -> Maybe Type
        makeList moduleName name args =
            case ( moduleName, name, args ) of
                ( [ "List" ], "List", [ listType ] ) ->
                    Just <| ListType <| go listType

                _ ->
                    Nothing
    in
    case Node.value annot of
        Typed name args ->
            moduleNameFor context.lookupTable name
                |> Maybe.withDefault (Tuple.first <| Node.value name)
                |> (\moduleName ->
                        if moduleName == [] then
                            -- If the module name is empty, then update to current module name
                            context.moduleName

                        else
                            moduleName
                   )
                |> Tuple.pair (Tuple.second <| Node.value name)
                |> (\( n, moduleName ) ->
                        makeList moduleName n args
                            |> MaybeX.withDefaultLazy (\() -> NamedType ( moduleName, n ) <| List.map go args)
                   )

        Unit ->
            UnitType

        Tupled ts ->
            TupleType <| List.map go ts

        Record fs ->
            RecordType
                { generic = Nothing
                , canonical = subrecordIsAlsoCanonical /= Nothing
                , fields = List.map (Tuple.mapBoth Node.value go << Node.value) fs
                }

        GenericRecord generic fs ->
            RecordType
                { generic =
                    -- Generic records completely ignore typeclasses, i.e.
                    -- `type alias G comparable = { comparable | x : Int }`
                    -- is just a normal generic record.
                    Just <| TypeVar Nothing <| Node.value generic
                , canonical = subrecordIsAlsoCanonical /= Nothing
                , fields = List.map (Tuple.mapBoth Node.value go << Node.value) <| Node.value fs
                }

        FunctionTypeAnnotation from to ->
            FunctionType { from = go from, to = go to }

        GenericType s ->
            makeTypeVar constrainedTypeVarsAreRespected s


{-| Canonicalize a type, dereferencing all aliases.
-}
dereferenceType : ModuleContext -> Type -> DereferencedType
dereferenceType context type_ =
    let
        dropFields : Set String -> Type -> Type
        dropFields toDrop t =
            case t of
                RecordType r ->
                    RecordType
                        { r
                            | fields = List.filter (\( f, _ ) -> not <| Set.member f toDrop) r.fields
                            , generic = Maybe.map (dropFields toDrop) r.generic
                        }

                notARecord ->
                    notARecord

        go : Type -> Type
        go t =
            case t of
                FunctionType { from, to } ->
                    FunctionType { from = go from, to = go to }

                TupleType ts ->
                    TupleType <| List.map go ts

                ListType t_ ->
                    ListType <| go t_

                NamedType ( moduleName, name ) ts ->
                    let
                        ts_ : List Type
                        ts_ =
                            List.map go ts
                    in
                    Dict.get moduleName context.aliases
                        |> Maybe.andThen (Dict.get name)
                        -- Apply type vars
                        |> Maybe.map (go << assignPositionalTypeVars ts_)
                        -- If no aliases match, it must be a custom type
                        |> Maybe.withDefault (NamedType ( moduleName, name ) ts_)

                RecordType r ->
                    let
                        fields : List ( String, Type )
                        fields =
                            List.map (Tuple.mapSecond go) r.fields
                    in
                    RecordType
                        { r
                            | fields = fields
                            , generic =
                                Maybe.map go r.generic
                                    -- Generic records overwrite more "inner" fields with their outer ones
                                    |> Maybe.map (dropFields (Set.fromList <| List.map Tuple.first fields))
                        }

                _ ->
                    -- UnitType
                    -- TypeVar
                    t
    in
    DereferencedType <| go type_


{-| Assign a list of positional type vars.
-}
assignPositionalTypeVars : List Type -> TypeWithPositionalVars -> Type
assignPositionalTypeVars ts t =
    List.indexedMap (\i -> Tuple.pair ("positional arg " ++ String.fromInt i)) ts
        |> Dict.fromList
        |> (\vars -> assignTypeVars vars (DereferencedType <| getTypeWithPositionalVars t))
        |> getType


{-| Given a list of type vars (as string), create positional type vars from
them.
-}
makePositionalArgTypeVars : List String -> Dict String Type
makePositionalArgTypeVars =
    List.indexedMap (\i s -> ( s, TypeVar Nothing <| "positional arg " ++ String.fromInt i ))
        >> Dict.fromList


{-| Assign type vars to a type.
-}
assignTypeVars : Dict String Type -> DereferencedType -> DereferencedType
assignTypeVars typeVars type_ =
    let
        go : Type -> Type
        go t =
            case t of
                FunctionType { from, to } ->
                    FunctionType { from = go from, to = go to }

                TupleType ts ->
                    TupleType <| List.map go ts

                ListType t_ ->
                    ListType <| go t_

                NamedType ( moduleName, name ) ts ->
                    List.map go ts
                        |> NamedType ( moduleName, name )

                RecordType r ->
                    RecordType
                        { r
                            | fields = List.map (Tuple.mapSecond go) r.fields
                            , generic = Maybe.map go r.generic
                        }

                TypeVar _ var ->
                    Dict.get var typeVars
                        |> Maybe.withDefault t

                UnitType ->
                    UnitType
    in
    DereferencedType <| go <| getType type_


{-| Prefix a string before type variables so as to disambiguate them for type
matching.
-}
prefixTypeVars : String -> DereferencedType -> DereferencedType
prefixTypeVars prefix type_ =
    let
        go : Type -> Type
        go t =
            case t of
                FunctionType { from, to } ->
                    FunctionType { from = go from, to = go to }

                TupleType ts ->
                    TupleType <| List.map go ts

                ListType t_ ->
                    ListType <| go t_

                NamedType ( moduleName, name ) ts ->
                    List.map go ts
                        |> NamedType ( moduleName, name )

                RecordType r ->
                    RecordType
                        { r
                            | fields = List.map (Tuple.mapSecond go) r.fields
                            , generic = Maybe.map go r.generic
                        }

                TypeVar class var ->
                    TypeVar class (prefix ++ var)

                UnitType ->
                    UnitType
    in
    DereferencedType <| go <| getType type_


{-| Unwrap a `DereferencedType`.
-}
getType : DereferencedType -> Type
getType (DereferencedType t) =
    t


{-| Unwrap a `TypeWithPositionalVars`.
-}
getTypeWithPositionalVars : TypeWithPositionalVars -> Type
getTypeWithPositionalVars (TypeWithPositionalVars t) =
    t


{-| Check the arguments to a function (for unsorted patterns) and its
expression, possibly having been given a type annotation.
-}
checkFunctionArgsAndExpr : RuleConfig -> LocalContext -> Maybe DereferencedType -> List (Node Pattern) -> Node Expression -> List (Error {})
checkFunctionArgsAndExpr config local hasType args expr =
    let
        ( argTypes, exprType ) =
            Maybe.map flattenFunctionType hasType
                |> Maybe.andThen (partiallyApplyArgsAndTypes args)
                |> Maybe.map (Tuple.mapBoth (List.map Just) Just)
                |> MaybeX.withDefaultLazy (\() -> ( List.map (always Nothing) args, Nothing ))

        newBindings : Dict String Type
        newBindings =
            List.map2 (bindingsInPatternWithType local.context) args argTypes
                |> List.concat
                |> Dict.fromList
    in
    checkExpression config { local | localFunctions = Dict.union local.localFunctions newBindings } exprType expr
        ++ List.concat (List.map2 (checkPattern config local.context) argTypes args)


{-| Given a list of arguments and a flattened function type, return a list of
argument types and the final function type.
-}
partiallyApplyArgsAndTypes : List a -> List DereferencedType -> Maybe ( List DereferencedType, DereferencedType )
partiallyApplyArgsAndTypes args types =
    List.length args
        |> (\i -> ListX.splitAt i types)
        |> (\( argTypes, return ) ->
                ListX.unconsLast return
                    |> Maybe.map
                        (Tuple.mapBoth getType (List.map getType)
                            >> (\( r, ts ) -> makeFunctionType r ts)
                            >> DereferencedType
                        )
                    |> Maybe.map (Tuple.pair argTypes)
           )


{-| Get all new bindings and their types from a pattern.
-}
bindingsInPatternWithType : ModuleContext -> Node Pattern -> Maybe DereferencedType -> List ( String, Type )
bindingsInPatternWithType context pattern type_ =
    let
        go : Node Pattern -> Maybe DereferencedType -> List ( String, Type )
        go =
            bindingsInPatternWithType context

        makeType : String -> Maybe DereferencedType -> List ( String, Type )
        makeType n t =
            Maybe.map getType t
                |> Maybe.map (List.singleton << Tuple.pair n)
                |> Maybe.withDefault []
    in
    case Node.value pattern of
        ListPattern ps ->
            getListType type_
                |> (\t -> List.concatMap (\p -> go p t) ps)

        TuplePattern ps ->
            getTupleTypes ps type_
                |> List.map2 go ps
                |> List.concat

        RecordPattern ps ->
            getRecordFieldTypes type_
                |> .types
                |> (\ts ->
                        List.map
                            (\p ->
                                Dict.get (Node.value p) ts
                                    |> makeType (Node.value p)
                            )
                            ps
                   )
                |> List.concat

        NamedPattern { name } ps ->
            -- Get type info we've stored and use it to check
            -- Note that while `findFunctionType` requires local context, a pattern can only be a constructor,
            -- which cannot be local, so we can just create an empty local context here.
            findFunctionType { context = context, localFunctions = Dict.empty } type_ pattern name
                |> Maybe.map (List.map2 (\p t -> go p (Just t)) ps)
                |> Maybe.map List.concat
                -- No type info
                |> MaybeX.withDefaultLazy (\() -> List.concatMap (\p -> go p Nothing) ps)

        UnConsPattern p ps ->
            -- `p` has type in list, and `ps` is overall list type
            getListType type_
                |> (\t -> go p t ++ go ps type_)

        VarPattern name ->
            -- Bind `name` to the overall type
            makeType name type_

        AsPattern p name ->
            -- Bind `name` to overall type and then descend
            makeType (Node.value name) type_ ++ go p type_

        ParenthesizedPattern p ->
            -- Parentheses don't affect type
            go p type_

        _ ->
            -- No bindings in:
            -- AllPattern
            -- UnitPattern
            -- CharPattern
            -- StringPattern
            -- IntPattern
            -- HexPattern
            -- FloatPattern
            []


{-| Descend into type annotations, checking for unsorted records.
-}
checkTypeAnnotation : RuleConfig -> ModuleContext -> Maybe DereferencedType -> Node TypeAnnotation -> List (Error {})
checkTypeAnnotation config context hasTypeFromParent type_ =
    let
        go : Maybe DereferencedType -> Node TypeAnnotation -> List (Error {})
        go =
            checkTypeAnnotation config context

        checkFields : List ( Node String, Node TypeAnnotation ) -> ( List (Error {}), Dict String Type ) -> List (Error {})
        checkFields fields ( parentError, canonicalTypeInfoFromParent ) =
            parentError
                ++ List.concatMap
                    (\( field, a ) ->
                        Dict.get (Node.value field) canonicalTypeInfoFromParent
                            |> Maybe.map (dereferenceType context)
                            |> (\t -> checkTypeAnnotation config context t a)
                    )
                    fields
    in
    case Node.value type_ of
        -- Records are simply records
        Record def ->
            (recordDefToCheckable context (Node.range type_) True hasTypeFromParent def
                |> checkRecord config context
            )
                -- Used any found record information to check subrecords
                |> checkFields (List.map Node.value def)

        GenericRecord _ def ->
            (recordDefToCheckable context (Node.range type_) False hasTypeFromParent (Node.value def)
                |> checkRecord config context
            )
                -- Used any found record information to check subrecords
                |> checkFields (List.map Node.value <| Node.value def)

        -- Descend into functions, tuples, and custom types
        FunctionTypeAnnotation fromA toA ->
            let
                ( fromType, toType ) =
                    case Maybe.map getType hasTypeFromParent of
                        Just (FunctionType { from, to }) ->
                            ( Just <| DereferencedType from, Just <| DereferencedType to )

                        _ ->
                            ( Nothing, Nothing )
            in
            go fromType fromA ++ go toType toA

        Tupled types_ ->
            -- Tuples must have a tuple type
            List.map2 go (getTupleTypes types_ hasTypeFromParent) types_
                |> List.concat

        Typed _ types_ ->
            let
                typeVars : List (Maybe DereferencedType)
                typeVars =
                    case Maybe.map getType hasTypeFromParent of
                        Just (NamedType _ ts) ->
                            List.map (Just << DereferencedType) ts

                        Just (ListType t) ->
                            [ Just <| DereferencedType t ]

                        _ ->
                            List.map (always Nothing) types_
            in
            List.map2 go typeVars types_
                |> List.concat

        -- Generic and unit types are dead ends
        GenericType _ ->
            []

        Unit ->
            []


{-| Descend into subexpressions, keeping as much type information as possible.
-}
checkExpression : RuleConfig -> LocalContext -> Maybe DereferencedType -> Node Expression -> List (Error {})
checkExpression config local hasType node =
    let
        go : Maybe DereferencedType -> Node Expression -> List (Error {})
        go =
            checkExpression config local

        checkFields : Dict String DereferencedType -> List ( Node String, Node Expression ) -> ( List (Error {}), Dict String Type ) -> List (Error {})
        checkFields typeInfo fields ( parentError, canonicalTypeInfoFromParent ) =
            parentError
                ++ List.concatMap
                    (\( field, e ) ->
                        let
                            f : String
                            f =
                                Node.value field
                        in
                        Dict.get f canonicalTypeInfoFromParent
                            |> Maybe.map (dereferenceType local.context)
                            |> MaybeX.orElseLazy (\() -> Dict.get f typeInfo)
                            |> (\t -> go t e)
                    )
                    fields
    in
    case Node.value node of
        -- Simple patterns simply descend into sub expressions of, unwrapping type if necessary
        Negation e ->
            -- Negation doesn't change a type
            go hasType e

        ParenthesizedExpression e ->
            -- Parentheses don't change a type
            go hasType e

        ListExpr es ->
            let
                -- Lists must have a list type
                type_ : Maybe DereferencedType
                type_ =
                    getListType hasType
            in
            List.concatMap (go type_) es

        Application es ->
            -- Try to pull type info from known functions
            checkApplicationChain config local hasType es

        OperatorApplication op _ e1 e2 ->
            -- Handle a few known operators
            checkOperatorApplication (checkApplicationChain config local hasType) op e1 e2

        IfBlock pred thenE elseE ->
            -- List predicate must have type Bool and branches have same type as overall
            go (Just <| DereferencedType <| NamedType ( [ "Basics" ], "Bool" ) []) pred ++ go hasType thenE ++ go hasType elseE

        TupledExpression es ->
            -- Tuples must have a tuple type
            List.map2 go (getTupleTypes es hasType) es
                |> List.concat

        CaseExpression { expression, cases } ->
            -- Try to infer type of case expression for patterns
            -- Branches have same type as overall
            let
                caseType : Maybe DereferencedType
                caseType =
                    inferExprType local expression
            in
            go Nothing expression
                ++ List.concatMap
                    (\( p, e ) ->
                        bindingsInPatternWithType local.context p caseType
                            |> Dict.fromList
                            |> (\newBindings ->
                                    checkExpression config { local | localFunctions = Dict.union local.localFunctions newBindings } hasType e
                               )
                    )
                    cases
                ++ List.concatMap (checkPattern config local.context caseType << Tuple.first) cases

        LambdaExpression { args, expression } ->
            -- Check as a function
            checkFunctionArgsAndExpr config local hasType args expression

        LetExpression { declarations, expression } ->
            -- Create new bindings from any that have type info
            let
                ( newBindings, decsToCheck ) =
                    List.map (checkLetDeclaration config local << Node.value) declarations
                        |> List.unzip
                        |> Tuple.mapFirst (Dict.fromList << List.concat)

                newContext : LocalContext
                newContext =
                    { local | localFunctions = Dict.union local.localFunctions newBindings }
            in
            checkExpression config newContext hasType expression
                ++ List.concatMap (\f -> f newContext) decsToCheck

        RecordExpr recordSetters ->
            -- A record expression has to have all fields of the known record
            let
                { types } =
                    getRecordFieldTypes hasType
            in
            recordSettersToCheckable local (Node.range node) True hasType recordSetters
                |> checkRecord config local.context
                -- Used any found record information to check subrecords
                |> checkFields types (List.map Node.value recordSetters)

        RecordUpdateExpression _ recordSetters ->
            -- A record update must have the same type as the record, so type is useful
            -- A record update expression does not have to have all fields of the known record
            let
                updateType : Maybe DereferencedType
                updateType =
                    -- Get type from updated var if we don't have a good annotation
                    hasType
                        |> MaybeX.orElseLazy (\() -> inferExprType local node)

                { types } =
                    getRecordFieldTypes updateType
            in
            recordSettersToCheckable local (Node.range node) False updateType recordSetters
                |> checkRecord config local.context
                -- Used any found record information to check subrecords
                |> checkFields types (List.map Node.value recordSetters)

        RecordAccess e accessFunc ->
            go (makeRecordAccessType hasType <| Node.value accessFunc) e

        _ ->
            -- The following neither have subexpressions nor are a record and so are dead ends
            -- UnitExpr
            -- PrefixOperator String
            -- Operator String
            -- Integer Int
            -- Hex Int
            -- Floatable Float
            -- GLSLExpression String
            -- RecordAccessFunction String
            -- Literal String
            -- CharLiteral Char
            -- FunctionOrValue ModuleName String
            []


{-| Generate a list of bindings from a `let` declaration as well as check them
for errors (when provided with the new context that includes all new `let`
bindings. Note that type inference isn't performed with new bindings.
-}
checkLetDeclaration : RuleConfig -> LocalContext -> LetDeclaration -> ( List ( String, Type ), LocalContext -> List (Error {}) )
checkLetDeclaration config local d =
    (case d of
        LetFunction f ->
            \local_ -> checkFunctionDeclaration config local_ f

        LetDestructuring p e ->
            -- Try to infer type of e for type info, since destructuring can't have type annotations
            \local_ -> checkExpression config local_ Nothing e ++ checkPattern config local.context (inferExprType local_ e) p
    )
        |> Tuple.pair (bindingsFromLetDeclaration local d)


{-| Get (a best guess) at the bindings from a let declaration.
-}
bindingsFromLetDeclaration : LocalContext -> LetDeclaration -> List ( String, Type )
bindingsFromLetDeclaration local d =
    case d of
        LetFunction f ->
            getFunctionBinding local.context f
                |> Maybe.map (Tuple.mapSecond getType)
                |> MaybeX.orElseLazy
                    (\() ->
                        Node.value f.declaration
                            |> (\{ name, expression } ->
                                    inferExprType local expression
                                        |> Maybe.map getType
                                        |> Maybe.map (Tuple.pair (Node.value name))
                               )
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        LetDestructuring p e ->
            -- Try to infer type of e for type info, since destructuring can't have type annotations
            inferExprType local e
                |> bindingsInPatternWithType local.context p


{-| Handle the few common operators we expect to see records passed through.
-}
checkOperatorApplication : (List (Node Expression) -> a) -> String -> Node Expression -> Node Expression -> a
checkOperatorApplication checkApp op e1 e2 =
    case op of
        "|>" ->
            checkApp [ e2, e1 ]

        "<|" ->
            checkApp [ e1, e2 ]

        _ ->
            -- Other operators, we don't know (or rather care) what they do, so just treat them like functions
            checkApp [ Node Range.empty <| PrefixOperator op, e1, e2 ]


{-| Check a chain of expressions being applied to the first one, as we can glean
additional info from any stored function types we have.
-}
checkApplicationChain : RuleConfig -> LocalContext -> Maybe DereferencedType -> List (Node Expression) -> List (Error {})
checkApplicationChain config local hasType es =
    let
        checkExpr : Maybe DereferencedType -> Node Expression -> List (Error {})
        checkExpr =
            checkExpression config local
    in
    case es of
        [] ->
            []

        [ e ] ->
            checkExpr hasType e

        func :: args ->
            case Node.value func of
                FunctionOrValue _ name ->
                    findFunctionType local hasType func name
                        |> Maybe.andThen (partiallyApplyArgsAndTypes args)
                        |> Maybe.map
                            (\( argTypes, return ) ->
                                -- Assign type vars
                                Maybe.map (typesMatch Dict.empty return) hasType
                                    |> Maybe.map Tuple.first
                                    |> Maybe.map (Dict.filter (\( i, _ ) _ -> i == 1))
                                    |> Maybe.map (DictX.mapKeys Tuple.second)
                                    |> Maybe.map (\typeVars -> List.map (assignTypeVars typeVars) argTypes)
                                    |> Maybe.withDefault argTypes
                            )
                        |> Maybe.map (List.map2 (\e t -> checkExpr (Just t) e) args)
                        |> Maybe.map List.concat
                        -- Couldn't find the function type, so type info is gone now
                        |> MaybeX.withDefaultLazy (\() -> List.concatMap (checkExpr Nothing) es)

                PrefixOperator op ->
                    findOperatorType local.context op
                        |> Maybe.andThen (partiallyApplyArgsAndTypes args)
                        |> Maybe.map
                            (\( argTypes, return ) ->
                                -- Assign type vars
                                Maybe.map (typesMatch Dict.empty return) hasType
                                    |> Maybe.map Tuple.first
                                    |> Maybe.map (Dict.filter (\( i, _ ) _ -> i == 1))
                                    |> Maybe.map (DictX.mapKeys Tuple.second)
                                    |> Maybe.map (\typeVars -> List.map (assignTypeVars typeVars) argTypes)
                                    |> Maybe.withDefault argTypes
                            )
                        |> Maybe.map (List.map2 (\e t -> checkExpr (Just t) e) args)
                        |> Maybe.map List.concat
                        -- Couldn't find the function type, so type info is gone now
                        |> MaybeX.withDefaultLazy (\() -> List.concatMap (checkExpr Nothing) es)

                ParenthesizedExpression func_ ->
                    -- Unwrap the parentheses
                    checkApplicationChain config local hasType (func_ :: args)

                RecordAccessFunction accessFunc ->
                    checkExpr (makeRecordAccessType hasType accessFunc)
                        -- Assume there is only one argument, because if not, it's a type error
                        |> (\f -> List.concatMap f args)

                _ ->
                    -- * This is valid application, but we can't infer the type
                    -- LambdaExpression Lambda
                    -- * These are possibly valid, but can't infer the "return" type of the block
                    -- Application (List (Node Expression))
                    -- LetExpression LetBlock
                    -- CaseExpression CaseBlock
                    -- IfBlock (Node Expression) (Node Expression) (Node Expression)
                    -- OperatorApplication String InfixDirection (Node Expression) (Node Expression)
                    -- RecordAccess (Node Expression) (Node String)
                    -- * Cannot apply args to non-functions
                    -- UnitExpr
                    -- GLSLExpression String
                    -- List.concatMap (checkExpr Nothing) es
                    -- RecordUpdateExpression (Node String) (List (Node RecordSetter))
                    -- ListExpr (List (Node Expression))
                    -- RecordExpr (List (Node RecordSetter))
                    -- Literal String
                    -- CharLiteral Char
                    -- TupledExpression (List (Node Expression))
                    -- Integer Int
                    -- Hex Int
                    -- Floatable Float
                    -- Operator String
                    -- Negation (Node Expression)
                    List.concatMap (checkExpr Nothing) es


{-| Make a type for a record being accessed
-}
makeRecordAccessType : Maybe DereferencedType -> String -> Maybe DereferencedType
makeRecordAccessType hasType accessFunc =
    -- We know the expression being accessed is a record with a field of the type of the total expression
    makeAccessFunc accessFunc
        |> (\f ->
                Maybe.map
                    (\t ->
                        DereferencedType <|
                            RecordType
                                -- This is, in essence, a generic record with one field
                                { generic = Just <| TypeVar Nothing <| "record access field " ++ f
                                , canonical = False
                                , fields = [ ( f, getType t ) ]
                                }
                    )
                    hasType
           )


{-| Given context, its return type, its module lookup node, and its name, find
any type info stored for a function, canonicalizing the result and returning a
list of arg types (and finally the return type).
-}
findFunctionType : LocalContext -> Maybe DereferencedType -> Node a -> String -> Maybe (List DereferencedType)
findFunctionType { context, localFunctions } type_ moduleNode name =
    let
        getTypeVars : Maybe DereferencedType -> List Type
        getTypeVars t =
            case Maybe.map getType t of
                Just (NamedType _ ts) ->
                    ts

                _ ->
                    []
    in
    moduleNameFor context.lookupTable moduleNode
        |> Maybe.map
            (\moduleName ->
                if moduleName == [] then
                    context.moduleName

                else
                    moduleName
            )
        |> Maybe.andThen
            (\moduleName ->
                if MaybeX.unwrap False (Char.isUpper << Tuple.first) <| String.uncons name then
                    -- Constructor, not a function, which cannot be defined in local context
                    Dict.get moduleName context.constructors
                        |> Maybe.andThen (Dict.get name)
                        |> Maybe.map .type_
                        |> Maybe.map (assignPositionalTypeVars (getTypeVars type_))

                else
                    Dict.get moduleName context.functionTypes
                        |> Maybe.andThen (Dict.get name)
                        |> MaybeX.orElseLazy (\() -> Dict.get name localFunctions)
            )
        |> Maybe.map (flattenFunctionType << dereferenceType context)


{-| Given context and an operator, find any type info stored for an operator,
canonicalizing the result.
-}
findOperatorType : ModuleContext -> String -> Maybe (List DereferencedType)
findOperatorType context op =
    DictX.filterMap (\_ funcs -> DictX.find (\funcName _ -> op == funcName) funcs) context.functionTypes
        |> Dict.toList
        -- There aren't any duplicate operators
        |> List.head
        |> Maybe.map
            (Tuple.second
                >> Tuple.second
                >> dereferenceType context
                >> flattenFunctionType
            )


{-| Turn a function into a list of argument types and a return type.
-}
flattenFunctionType : DereferencedType -> List DereferencedType
flattenFunctionType =
    let
        go : Type -> List Type
        go t =
            case t of
                FunctionType { from, to } ->
                    from :: go to

                otherType ->
                    [ otherType ]
    in
    getType
        >> go
        >> List.map DereferencedType


{-| Get the type a `List` is full of if possible.
-}
getListType : Maybe DereferencedType -> Maybe DereferencedType
getListType assocType =
    case Maybe.map getType assocType of
        Just (ListType t) ->
            Just <| DereferencedType t

        _ ->
            Nothing


{-| Get all types for a tuple's subexpressions.
-}
getTupleTypes : List a -> Maybe DereferencedType -> List (Maybe DereferencedType)
getTupleTypes emptyList assocType =
    case Maybe.map getType assocType of
        Just (TupleType ts) ->
            List.map (Just << DereferencedType) ts

        _ ->
            List.map (always Nothing) emptyList


{-| Get all types for a record's fields.
-}
getRecordFieldTypes : Maybe DereferencedType -> { types : Dict String DereferencedType, hasAllFields : Bool }
getRecordFieldTypes type_ =
    case Maybe.map getType type_ of
        Just (RecordType { fields, generic }) ->
            { types = Dict.fromList <| List.map (Tuple.mapSecond DereferencedType) fields
            , hasAllFields = generic == Nothing
            }

        _ ->
            { types = Dict.empty, hasAllFields = False }


{-| Descend into pattern, keeping as much type information as possible.
-}
checkPattern : RuleConfig -> ModuleContext -> Maybe DereferencedType -> Node Pattern -> List (Error {})
checkPattern config context hasType node =
    let
        go : Maybe DereferencedType -> Node Pattern -> List (Error {})
        go =
            checkPattern config context
    in
    case Node.value node of
        TuplePattern ps ->
            -- Tuples must have a tuple type
            List.map2 go (getTupleTypes ps hasType) ps
                |> List.concat

        UnConsPattern p ps ->
            let
                -- Uncons has to be a list
                type_ : Maybe DereferencedType
                type_ =
                    getListType hasType
            in
            go type_ p ++ go type_ ps

        ListPattern ps ->
            let
                -- List pattern has to be a list
                type_ : Maybe DereferencedType
                type_ =
                    getListType hasType
            in
            List.concatMap (go type_) ps

        AsPattern p _ ->
            -- As pattern does not change type
            go hasType p

        ParenthesizedPattern p ->
            -- Parentheses do not change type
            go hasType p

        NamedPattern { name } pats ->
            -- Get type info we've stored and use it to check
            -- Note that while `findFunctionType` requires local context, a pattern can only be a constructor,
            -- which cannot be local, so we can just create an empty local context here.
            findFunctionType { context = context, localFunctions = Dict.empty } hasType node name
                |> Maybe.map (List.map2 (\p t -> go (Just t) p) pats)
                |> Maybe.map List.concat
                |> MaybeX.withDefaultLazy (\() -> List.concatMap (go Nothing) pats)

        RecordPattern fields ->
            recordPatternToCheckable (Node.range node) hasType fields
                |> checkRecord config context
                -- No such thing as subrecords for patterns, so we can just return the errors
                |> Tuple.first

        _ ->
            -- Neither can descend into nor check:
            -- AllPattern
            -- UnitPattern
            -- CharPattern Char
            -- StringPattern String
            -- IntPattern Int
            -- HexPattern Int
            -- FloatPattern Float
            -- VarPattern String
            []


{-| Given an error range and whether or not a record has all fields of the known
type, convert a `RecordDefinition` into a checkable record.
-}
recordDefToCheckable : ModuleContext -> Range -> Bool -> Maybe DereferencedType -> RecordDefinition -> RecordToCheck
recordDefToCheckable context fullRange hasAllFields hasTypeFromParent fields =
    let
        makeType : Node TypeAnnotation -> Maybe DereferencedType
        makeType =
            typeAnnotToNoncanonicalType context
                >> dereferenceType context
                >> Just

        orderInfo : Maybe OrderInfo
        orderInfo =
            Maybe.andThen (Result.toMaybe << makeFieldOrder) hasTypeFromParent
                |> Maybe.map HasFieldOrder
                |> MaybeX.orElse
                    (if hasAllFields then
                        Just HasAllFields

                     else
                        Nothing
                    )
    in
    List.map
        (\f ->
            let
                ( field, type_ ) =
                    Node.value f
            in
            { field = Node.value field, type_ = makeType type_, range = Node.range f }
        )
        fields
        |> (\fs ->
                { fullRange = fullRange
                , orderInfo = orderInfo
                , fields = fs
                }
           )


{-| Convert a record type into a field order, assuming no fields are missing
(since that would be a type error). Returns a list of non-canonical fields
encountered
-}
makeFieldOrder : DereferencedType -> Result (List ( String, Type )) FieldOrder
makeFieldOrder =
    let
        go : Type -> Result (List ( String, Type )) FieldOrder
        go type_ =
            case type_ of
                RecordType { canonical, generic, fields } ->
                    if canonical then
                        Ok <|
                            FieldOrder
                                { canonical =
                                    List.indexedMap (\i ( n, t ) -> ( n, ( i, t ) )) fields
                                        |> Dict.fromList
                                , generic =
                                    Maybe.map
                                        (\g ->
                                            case go g of
                                                Ok o ->
                                                    OrderedFields o

                                                Err [] ->
                                                    -- Unit record is always canonical, or else it's a type error, in which case who knows
                                                    OrderedFields <| FieldOrder { canonical = Dict.empty, generic = Nothing }

                                                Err fs ->
                                                    List.map Tuple.first fs
                                                        |> UnknownFields
                                        )
                                        generic
                                }

                    else
                        Err fields

                _ ->
                    Err []
    in
    go << getType


{-| Given an error range and maybe a type, convert a record pattern to a
checkable record.
-}
recordPatternToCheckable : Range -> Maybe DereferencedType -> List (Node String) -> RecordToCheck
recordPatternToCheckable fullRange hasType =
    let
        { types, hasAllFields } =
            getRecordFieldTypes hasType

        orderInfo : Maybe OrderInfo
        orderInfo =
            Maybe.andThen (Result.toMaybe << makeFieldOrder) hasType
                |> Maybe.map HasFieldOrder
                |> MaybeX.orElse
                    (if Dict.isEmpty types then
                        Nothing

                     else
                        Just <| HasTheseFields { fields = types, hasAllFields = hasAllFields }
                    )
    in
    List.map
        (\r ->
            { field = Node.value r
            , type_ = Dict.get (Node.value r) types
            , range = Node.range r
            }
        )
        >> (\fs ->
                { fullRange = fullRange
                , orderInfo = orderInfo
                , fields = fs
                }
           )


{-| Given an error range, whether or not a record has all fields of the known
type, and maybe a type, convert a list of record setters to a checkable record.
-}
recordSettersToCheckable : LocalContext -> Range -> Bool -> Maybe DereferencedType -> List (Node RecordSetter) -> RecordToCheck
recordSettersToCheckable context fullRange hasAllFields hasType =
    let
        { types } =
            getRecordFieldTypes hasType

        orderInfo : Maybe OrderInfo
        orderInfo =
            Maybe.andThen (Result.toMaybe << makeFieldOrder) hasType
                |> Maybe.map HasFieldOrder
                |> MaybeX.orElse
                    (if hasAllFields then
                        Just HasAllFields

                     else
                        Nothing
                    )
    in
    List.map
        (\r ->
            let
                f : String
                f =
                    Node.value <| Tuple.first <| Node.value r
            in
            { field = f
            , type_ =
                Dict.get f types
                    |> MaybeX.orElseLazy (\() -> inferExprType context <| Tuple.second <| Node.value r)
            , range = Node.range r
            }
        )
        >> (\fs ->
                { fullRange = fullRange
                , orderInfo = orderInfo
                , fields = fs
                }
           )


{-| Given two dicts of field types, keep only fields that have identical types
between the two.
-}
keepOnlyMatchingFieldTypes : Dict String Type -> Dict String Type -> Dict String Type
keepOnlyMatchingFieldTypes d1 d2 =
    Dict.merge (\_ _ acc -> acc)
        (\field t1 t2 acc ->
            if t1 == t2 then
                Dict.insert field t1 acc

            else
                acc
        )
        (\_ _ acc -> acc)
        d1
        d2
        Dict.empty


{-| Once a record has been reduced to a standard format, check its sorting,
returning a list of canonical field types, if any were found.
-}
checkRecord : RuleConfig -> ModuleContext -> RecordToCheck -> ( List (Error {}), Dict String Type )
checkRecord ((RuleConfig { sortUnknown, sortAmbiguous, sortGenerics }) as config) context ({ fullRange, orderInfo, fields } as record) =
    let
        errorRange : Range
        errorRange =
            let
                s : Location
                s =
                    fullRange.start
            in
            -- Assume opening `{` is just the first character of the range.
            { start = s, end = { s | column = s.column + 1 } }

        matchingOrders :
            ( List
                { typeName : List ( ModuleName, String )
                , fieldOrder : Dict String Int
                , hasUnknownFields : Bool
                , canonicalFieldTypes : Dict String Type
                , isSubrecord : Bool
                }
            , Bool
            )
        matchingOrders =
            findMatchingTypes config context orderInfo fields
                |> List.map
                    (\{ typeName, fieldOrder, isSubrecord } ->
                        makeOrder sortGenerics fields fieldOrder
                            |> (\o ->
                                    ( Dict.toList o.fieldOrder
                                    , { typeName = typeName
                                      , fieldOrder = o.fieldOrder
                                      , hasUnknownFields = o.hasUnknownFields
                                      , canonicalFieldTypes = o.canonicalFieldTypes
                                      , isSubrecord = isSubrecord
                                      }
                                    )
                               )
                    )
                -- Dedupe by field order
                |> DictX.fromListDedupe
                    (\o1 o2 ->
                        (if o1.hasUnknownFields then
                            -- Prefer matches without unknown fields
                            o2

                         else
                            o1
                        )
                            |> (\o ->
                                    -- Keep only matching field types between duplicate orders
                                    { o | canonicalFieldTypes = keepOnlyMatchingFieldTypes o.canonicalFieldTypes o2.canonicalFieldTypes }
                               )
                    )
                |> Dict.values
                |> List.partition .hasUnknownFields
                |> Tuple.mapBoth (List.partition .isSubrecord) (List.partition .isSubrecord)
                |> (\os ->
                        case os of
                            -- Prefer matches without unknown fields and that are not subrecords
                            ( ( unknownIsSubrecord, [] ), ( [], [] ) ) ->
                                ( unknownIsSubrecord, True )

                            ( ( _, unknownNotSubrecord ), ( [], [] ) ) ->
                                ( unknownNotSubrecord, True )

                            ( _, ( noUnknownIsSubrecord, [] ) ) ->
                                ( noUnknownIsSubrecord, False )

                            ( _, ( _, noUnknownNotSubrecord ) ) ->
                                ( noUnknownNotSubrecord, False )
                   )

        alphabetical : Field -> Field -> Order
        alphabetical f1 f2 =
            compare f1.field f2.field

        byFieldOrder : Dict String Int -> Field -> Field -> Order
        byFieldOrder ord f1 f2 =
            let
                o : String -> Int
                o f =
                    Dict.get f ord
                        |> Maybe.withDefault -1
            in
            compare (o f1.field) (o f2.field)

        checkSortingBy : (Field -> Field -> Order) -> List (Error {})
        checkSortingBy o =
            checkSorting context.extractSource "Record fields" [ o ] errorRange fields

        handleUnknown : (Field -> Field -> Order) -> List (Error {})
        handleUnknown whenAlphabetical =
            case sortUnknown of
                Alphabetically ->
                    -- Unknown record, so sort alphabetically if config says to
                    checkSortingBy whenAlphabetical

                ReportOnly ->
                    -- Unknown record, so report without fixes
                    unknownRecordError record fullRange

                DoNotSort ->
                    -- Unknown record; don't sort it
                    []
    in
    case matchingOrders of
        ( [], _ ) ->
            -- Completely unknown record, so no type data
            -- Sort alphabetically if config says to
            ( handleUnknown alphabetical, Dict.empty )

        ( [ { fieldOrder, canonicalFieldTypes } ], True ) ->
            -- Generic with unknown fields, so sort accordingly and use what type info we have
            ( handleUnknown (byFieldOrder fieldOrder), canonicalFieldTypes )

        ( [ { fieldOrder, canonicalFieldTypes } ], False ) ->
            -- Unambiguous record
            ( checkSortingBy <| byFieldOrder fieldOrder, canonicalFieldTypes )

        ( ambiguous, _ ) ->
            --Ambiguous record
            let
                unambiguousFieldTypes : Dict String Type
                unambiguousFieldTypes =
                    -- Keep any fields that have identical types despite the ambiguity
                    List.foldl (\{ canonicalFieldTypes } acc -> keepOnlyMatchingFieldTypes canonicalFieldTypes acc)
                        Dict.empty
                        ambiguous
            in
            case sortAmbiguous of
                Alphabetically ->
                    -- Sort alphabetically if config says to
                    ( checkSortingBy alphabetical, unambiguousFieldTypes )

                ReportOnly ->
                    -- Report without fixes if config says to
                    ( ambiguousRecordError record (List.map (List.map (\( m, n ) -> String.join "." <| m ++ [ n ]) << .typeName) ambiguous) fullRange
                    , unambiguousFieldTypes
                    )

                DoNotSort ->
                    -- Do not sort if config says not to
                    ( [], unambiguousFieldTypes )


{-| Given how to sort generics, a list of fields to sort, and a `FieldOrder`,
return an ordering of fields, whether or not any of them were an unknown record
(via generics), and any canonical field types.
-}
makeOrder : SortGenerics -> List Field -> FieldOrder -> { fieldOrder : Dict String Int, hasUnknownFields : Bool, canonicalFieldTypes : Dict String Type }
makeOrder sortGenerics inFields (FieldOrder inOrder) =
    let
        genericOffset : Int
        genericOffset =
            -- This is an ugly hack to put generic fields first/last,
            -- but it works as long as a record doesn't have over 1,000,000 fields, which seems safe
            case sortGenerics of
                GenericFieldsFirst ->
                    -1000000

                GenericFieldsLast ->
                    1000000

        go : Int -> List Field -> FieldOrder -> ( List ( String, Int ), Bool )
        go offsetMult fieldsToMake (FieldOrder { canonical, generic }) =
            let
                step : Field -> ( List ( String, Int ), List Field ) -> ( List ( String, Int ), List Field )
                step f ( canAcc, genAcc ) =
                    case Dict.get f.field canonical of
                        Just ( i, _ ) ->
                            ( ( f.field
                              , i + offsetMult * genericOffset
                              )
                                :: canAcc
                            , genAcc
                            )

                        Nothing ->
                            ( canAcc, f :: genAcc )
            in
            List.foldl step ( [], [] ) fieldsToMake
                |> Tuple.mapSecond
                    (\fs ->
                        case generic of
                            Just (OrderedFields order) ->
                                go (offsetMult + 1) fs order

                            Just (UnknownFields fs_) ->
                                let
                                    alph : List String
                                    alph =
                                        List.sort fs_
                                in
                                ( List.map
                                    (\f ->
                                        ( f.field
                                        , ListX.elemIndex f.field alph
                                            |> Maybe.withDefault -1
                                            |> (+) ((offsetMult + 1) * genericOffset)
                                        )
                                    )
                                    fs
                                , True
                                )

                            Nothing ->
                                ( [], False )
                    )
                |> (\( f1s, ( f2s, unknown ) ) -> ( f1s ++ f2s, unknown ))
    in
    go 0 inFields (FieldOrder inOrder)
        |> (\( fieldOrder, hasUnknownFields ) ->
                { fieldOrder =
                    fieldOrder
                        |> List.sortBy Tuple.second
                        -- Reindex sorting from 0, since fields may be missing
                        |> List.indexedMap (\i ( f, _ ) -> ( f, i ))
                        |> Dict.fromList
                , hasUnknownFields = hasUnknownFields
                , canonicalFieldTypes = Dict.map (\_ v -> Tuple.second v) inOrder.canonical
                }
           )


{-| Given context of known types, any information that would help find the
appropriate canonical ordering, and a list of fields, return all matching field
orders.
-}
findMatchingTypes : RuleConfig -> ModuleContext -> Maybe OrderInfo -> List Field -> List { typeName : List ( ModuleName, String ), fieldOrder : FieldOrder, isSubrecord : Bool }
findMatchingTypes config context info matchFields =
    let
        getMatches : List Field -> List { typeName : List ( ModuleName, String ), fieldOrder : FieldOrder, isSubrecord : Bool }
        getMatches fs =
            let
                { canonicalMatches, genericMatches } =
                    if List.isEmpty fs then
                        { canonicalMatches = [], genericMatches = [] }

                    else
                        searchOrders config context hasAllFields fs
            in
            List.map
                (\( name, k ) ->
                    { typeName = [ name ]
                    , fieldOrder = toFieldOrder Nothing k
                    , isSubrecord = k.isSubrecord
                    }
                )
                canonicalMatches
                ++ List.concatMap makeGeneric genericMatches

        toFieldOrder : Maybe Generic -> KnownRecord -> FieldOrder
        toFieldOrder generic { order } =
            FieldOrder
                { canonical = order
                , generic = generic
                }

        makeGeneric : { type_ : ( ( ModuleName, String ), KnownRecord ), missing : List Field } -> List { typeName : List ( ModuleName, String ), fieldOrder : FieldOrder, isSubrecord : Bool }
        makeGeneric { type_, missing } =
            let
                ( n, rec ) =
                    type_
            in
            getMatches missing
                |> List.map
                    (\{ typeName, fieldOrder, isSubrecord } ->
                        { typeName = n :: typeName
                        , fieldOrder = toFieldOrder (Just <| OrderedFields fieldOrder) rec
                        , isSubrecord = rec.isSubrecord || isSubrecord
                        }
                    )
                |> (\ls ->
                        if List.isEmpty ls then
                            -- Either it's an empty generic or nothing matched; either way just say what fields are missing
                            [ { typeName = [ n ]
                              , fieldOrder = toFieldOrder (Just <| UnknownFields <| List.map .field missing) rec
                              , isSubrecord = rec.isSubrecord
                              }
                            ]

                        else
                            ls
                   )

        hasAllFields : Bool
        hasAllFields =
            case info of
                Just HasAllFields ->
                    True

                Just (HasTheseFields r) ->
                    r.hasAllFields

                _ ->
                    False
    in
    case info of
        Just (HasFieldOrder f) ->
            -- Don't worry about module name, because this will never be ambiguous
            [ { typeName = []
              , fieldOrder = f
              , isSubrecord = False
              }
            ]

        Just (HasTheseFields { fields }) ->
            -- If we know that the record has extra fields, add them in and search for them as well
            List.map (\f -> ( f.field, f )) matchFields
                |> Dict.fromList
                |> (\d ->
                        Dict.map (\f t -> { field = f, type_ = Just t, range = Range.empty }) fields
                            |> Dict.union d
                   )
                |> Dict.values
                |> getMatches

        _ ->
            getMatches matchFields


{-| A list of orders that match a list of fields, including both full matches
and generic matches (with fields not present in the generic).
-}
type alias OrderMatches =
    { canonicalMatches : List ( ( ModuleName, String ), KnownRecord )
    , genericMatches :
        List
            { type_ :
                ( ( ModuleName, String )
                , KnownRecord
                )
            , missing : List Field
            }
    }


{-| Given context and a predicate to filter valid records, find a list of
records that match a list of fields.
-}
searchOrders : RuleConfig -> ModuleContext -> Bool -> List Field -> OrderMatches
searchOrders (RuleConfig { typecheckUnambiguousRecords }) context hasAllFields fields =
    let
        matchFields : Dict String Field
        matchFields =
            List.map (\f -> ( f.field, f )) fields
                |> Dict.fromList

        checkTypes : KnownRecord -> Bool
        checkTypes { order } =
            List.foldl
                (\{ field, type_ } ( varAcc, matchAcc ) ->
                    let
                        assignedVars : Dict ( Int, String ) Type
                        assignedVars =
                            -- Only keep type variables assigned on the known record, since fields are independent
                            -- For example, { a = Nothing, b = Nothing } should not require `a` and `b` to have the
                            -- same type variable (but it does for the known record if they are both `Maybe var`)
                            Dict.filter (\( i, _ ) _ -> i == 2) varAcc
                    in
                    Dict.get field order
                        |> Maybe.map (dereferenceType context << Tuple.second)
                        |> Maybe.map2 (typesMatch assignedVars) type_
                        -- If any `Nothing`s were encountered, we're missing type info
                        |> Maybe.withDefault ( assignedVars, True )
                        |> Tuple.mapSecond ((&&) matchAcc)
                )
                ( Dict.empty, True )
                fields
                |> Tuple.second

        missingFieldLimit : KnownRecord -> Int
        missingFieldLimit =
            if hasAllFields then
                always 1

            else
                Dict.size << .order

        step : ModuleName -> String -> KnownRecord -> OrderMatches -> OrderMatches
        step moduleName name o acc =
            if Dict.size (Dict.diff o.order matchFields) >= missingFieldLimit o then
                -- Too many fields are missing
                acc

            else if o.isGeneric then
                { acc
                    | genericMatches =
                        { type_ = ( ( moduleName, name ), o )
                        , missing = Dict.values <| Dict.diff matchFields o.order
                        }
                            :: acc.genericMatches
                }

            else if Dict.isEmpty <| Dict.diff matchFields o.order then
                -- Record is not generic and has no extra fields, so it is a good match
                { acc | canonicalMatches = ( ( moduleName, name ), o ) :: acc.canonicalMatches }

            else
                -- Record was not generic but extra fields were present, so it wasn't a match
                acc
    in
    Dict.foldl
        (\moduleName moduleTypes outerAcc ->
            Dict.foldl
                (step moduleName)
                outerAcc
                moduleTypes
        )
        { canonicalMatches = [], genericMatches = [] }
        context.canonicalRecords
        |> (\({ canonicalMatches, genericMatches } as res) ->
                if typecheckUnambiguousRecords || List.length canonicalMatches + List.length genericMatches > 1 then
                    -- If there are multiple matches, try to disambiguate by type
                    { canonicalMatches = List.filter (checkTypes << Tuple.second) canonicalMatches
                    , genericMatches = List.filter (checkTypes << Tuple.second << .type_) genericMatches
                    }

                else
                    res
           )


{-| Check two `Type`s and see if they are theoretically equivalent (e.g. type
vars can match anything.
-}
typesMatch : Dict ( Int, String ) Type -> DereferencedType -> DereferencedType -> ( Dict ( Int, String ) Type, Bool )
typesMatch inVars (DereferencedType derefType1) (DereferencedType derefType2) =
    let
        matchTypeVars : Int -> Dict ( Int, String ) Type -> Maybe Typeclass -> String -> Type -> ( Dict ( Int, String ) Type, Bool )
        matchTypeVars side typeVars typeclass name type_ =
            case ( Dict.get ( side, name ) typeVars, type_ ) of
                ( Just t, _ ) ->
                    -- If the type var has been assigned, check if that type matches
                    -- Remove the matched typevar, because if the same exists within it refers to a different value
                    go (Dict.remove ( side, name ) typeVars) t type_
                        |> Tuple.mapFirst (Dict.insert ( side, name ) t)

                ( Nothing, TypeVar _ _ ) ->
                    -- Handle base case.  This isn't right, but not worth going to the effort of matching typeclasses and the like
                    ( typeVars, True )

                ( Nothing, _ ) ->
                    if
                        Maybe.map (matchesTypeClass type_) typeclass
                            |> Maybe.withDefault True
                    then
                        ( Dict.insert ( side, name ) type_ typeVars, True )

                    else
                        ( typeVars, False )

        checkListOfTypes : Dict ( Int, String ) Type -> List Type -> List Type -> ( Dict ( Int, String ) Type, Bool )
        checkListOfTypes typeVars t1s t2s =
            if List.length t1s /= List.length t2s then
                ( typeVars, False )

            else
                ListX.zip t1s t2s
                    |> List.foldl
                        (\( t1_, t2_ ) ( varAcc, matchAcc ) ->
                            go varAcc t1_ t2_
                                |> Tuple.mapSecond ((&&) matchAcc)
                        )
                        ( typeVars, True )

        validateRecordMatch : Dict ( Int, String ) Type -> Maybe Type -> Maybe Type -> { only1 : List ( String, Type ), both : List ( Type, Type ), only2 : List ( String, Type ) } -> ( Dict ( Int, String ) Type, Bool )
        validateRecordMatch typeVars generic1 generic2 { only1, only2, both } =
            -- Fields in both must match
            List.unzip both
                |> (\( l1s, l2s ) -> checkListOfTypes typeVars l1s l2s)
                |> (\( vars, matchAcc ) ->
                        validate (not << List.isEmpty) only1
                            |> Maybe.map2 (\g fs -> go vars (RecordType { canonical = False, generic = Nothing, fields = fs }) g) generic2
                            |> Maybe.withDefault ( vars, List.isEmpty only1 )
                            |> Tuple.mapSecond ((&&) matchAcc)
                   )
                |> (\( vars, matchAcc ) ->
                        validate (not << List.isEmpty) only2
                            |> Maybe.map2 (\g fs -> go vars (RecordType { canonical = False, generic = Nothing, fields = fs }) g) generic1
                            |> Maybe.withDefault ( vars, List.isEmpty only2 )
                            |> Tuple.mapSecond ((&&) matchAcc)
                   )

        go : Dict ( Int, String ) Type -> Type -> Type -> ( Dict ( Int, String ) Type, Bool )
        go typeVars type1 type2 =
            case ( type1, type2 ) of
                ( TypeVar c n, t ) ->
                    matchTypeVars 1 typeVars c n t

                ( t, TypeVar c n ) ->
                    matchTypeVars 2 typeVars c n t

                ( ListType t1, ListType t2 ) ->
                    go typeVars t1 t2

                ( TupleType t1s, TupleType t2s ) ->
                    -- Tuples must be same size
                    checkListOfTypes typeVars t1s t2s

                ( FunctionType f1, FunctionType f2 ) ->
                    checkListOfTypes typeVars [ f1.from, f1.to ] [ f1.from, f2.to ]

                ( UnitType, UnitType ) ->
                    ( typeVars, True )

                ( NamedType n1 args1, NamedType n2 args2 ) ->
                    checkListOfTypes typeVars args1 args2
                        |> Tuple.mapSecond ((&&) (n1 == n2))

                ( RecordType r1, RecordType r2 ) ->
                    -- Divide fields into those that are present in both and those that are missing
                    Dict.merge
                        (\f t1 acc -> { acc | only1 = ( f, t1 ) :: acc.only1 })
                        (\_ t1 t2 acc -> { acc | both = ( t1, t2 ) :: acc.both })
                        (\f t2 acc -> { acc | only2 = ( f, t2 ) :: acc.only2 })
                        (Dict.fromList r1.fields)
                        (Dict.fromList r2.fields)
                        { only1 = [], both = [], only2 = [] }
                        |> validateRecordMatch typeVars r1.generic r2.generic

                _ ->
                    ( typeVars, False )
    in
    go inVars derefType1 derefType2


{-| Check if a type matches a constrained type variable.
-}
matchesTypeClass : Type -> Typeclass -> Bool
matchesTypeClass type_ class =
    let
        typeIsComparable : Type -> Bool
        typeIsComparable t =
            case t of
                NamedType ( [ "String" ], "String" ) [] ->
                    True

                NamedType ( [ "Char" ], "Char" ) [] ->
                    True

                NamedType ( [ "Basics" ], "Int" ) [] ->
                    True

                NamedType ( [ "Basics" ], "Float" ) [] ->
                    True

                ListType t_ ->
                    typeIsComparable t_

                TupleType ts ->
                    List.all typeIsComparable ts

                _ ->
                    -- Nothing else is comparable
                    False

        typeIsAppendable : Type -> Bool
        typeIsAppendable t =
            case t of
                NamedType ( [ "String" ], "String" ) [] ->
                    True

                ListType _ ->
                    True

                _ ->
                    -- Nothing else is appendable
                    False

        typeIsNumber : Type -> Bool
        typeIsNumber t =
            case t of
                NamedType ( [ "Basics" ], "Int" ) [] ->
                    True

                NamedType ( [ "Basics" ], "Float" ) [] ->
                    True

                _ ->
                    -- Nothing else is a number
                    False
    in
    case class of
        Appendable ->
            typeIsAppendable type_

        Comparable ->
            typeIsComparable type_

        CompAppend ->
            typeIsAppendable type_ && typeIsComparable type_

        Number ->
            typeIsNumber type_


{-| Do some half-hearted type inference to help differentiate records.
-}
inferExprType : LocalContext -> Node Expression -> Maybe DereferencedType
inferExprType local =
    let
        makeFunc : String -> Maybe (List DereferencedType) -> Maybe Type
        makeFunc typeVarPrefix =
            Maybe.map (List.map (prefixTypeVars typeVarPrefix))
                >> Maybe.andThen ListX.unconsLast
                >> Maybe.map
                    (Tuple.mapBoth getType (List.map getType)
                        >> (\( r, ts ) -> makeFunctionType r ts)
                    )

        go : String -> Node Expression -> Maybe Type
        go typeVarPrefix node =
            case Node.value node of
                UnitExpr ->
                    -- Mr. Incredible says UNIT IS UNIT.
                    Just UnitType

                Integer _ ->
                    Just <| NamedType ( [ "Basics" ], "Int" ) []

                Hex _ ->
                    Just <| NamedType ( [ "Basics" ], "Int" ) []

                Floatable _ ->
                    Just <| NamedType ( [ "Basics" ], "Float" ) []

                Literal _ ->
                    Just <| NamedType ( [ "String" ], "String" ) []

                CharLiteral _ ->
                    Just <| NamedType ( [ "Char" ], "Char" ) []

                TupledExpression es ->
                    List.indexedMap (\i e -> go (typeVarPrefix ++ "Tuple Expression " ++ String.fromInt i) e) es
                        |> MaybeX.combine
                        |> Maybe.map TupleType

                ListExpr es ->
                    if List.isEmpty es then
                        -- An empty list can be anything
                        Just (ListType (TypeVar Nothing <| typeVarPrefix ++ "inferred empty list typevar"))

                    else
                        -- Have to check all and unify
                        List.indexedMap (\i e -> go (typeVarPrefix ++ "List Element " ++ String.fromInt i) e) es
                            |> MaybeX.values
                            |> unifyTypes
                            |> Maybe.map ListType

                RecordExpr rs ->
                    rs
                        |> List.map
                            (\n ->
                                let
                                    ( f, e ) =
                                        Node.value n
                                in
                                go (typeVarPrefix ++ "Record Field " ++ Node.value f) e
                                    |> Maybe.map (Tuple.pair (Node.value f))
                            )
                        |> MaybeX.combine
                        |> Maybe.map (\fields -> RecordType { generic = Nothing, fields = fields, canonical = False })

                FunctionOrValue _ name ->
                    findFunctionType local Nothing node name
                        |> makeFunc typeVarPrefix

                ParenthesizedExpression e ->
                    -- Type is just whatever is in parentheses
                    go typeVarPrefix e

                Negation e ->
                    go typeVarPrefix e

                IfBlock _ e1 e2 ->
                    -- Try to infer either side and unify
                    [ go (typeVarPrefix ++ "If Block True") e1, go (typeVarPrefix ++ "If Block False") e2 ]
                        |> MaybeX.values
                        |> unifyTypes

                PrefixOperator p ->
                    findOperatorType local.context p
                        |> makeFunc typeVarPrefix

                CaseExpression { cases } ->
                    -- Try to infer all cases and unify
                    List.indexedMap (\i ( _, e ) -> go (typeVarPrefix ++ "Case Expression " ++ String.fromInt i) e) cases
                        |> MaybeX.values
                        |> unifyTypes

                Application es ->
                    inferApplicationChain local es
                        |> Maybe.map getType

                LambdaExpression { args, expression } ->
                    let
                        unwrapArgs : List String -> Maybe Type
                        unwrapArgs xs =
                            ListX.uncons xs
                                |> MaybeX.unpack (\() -> go typeVarPrefix expression)
                                    (\( x, xs_ ) ->
                                        unwrapArgs xs_
                                            |> Maybe.map (\t -> FunctionType { from = TypeVar Nothing <| typeVarPrefix ++ x, to = t })
                                    )
                    in
                    List.indexedMap (\i _ -> "lambda arg" ++ String.fromInt i) args
                        |> unwrapArgs

                RecordAccess e accessFunc ->
                    go typeVarPrefix e
                        |> Maybe.map DereferencedType
                        |> getRecordFieldTypes
                        |> .types
                        |> (\ts ->
                                Node.value accessFunc
                                    |> makeAccessFunc
                                    |> (\f -> Dict.get f ts)
                           )
                        |> Maybe.map getType

                GLSLExpression _ ->
                    Nothing

                RecordAccessFunction accessFunc ->
                    -- We know the expression being accessed is a record with a field of the type of the total expression
                    makeAccessFunc accessFunc
                        |> (\f ->
                                Just <|
                                    FunctionType
                                        { from =
                                            RecordType
                                                -- This is, in essence, a generic record with one field
                                                { generic = Just <| TypeVar Nothing <| typeVarPrefix ++ "record access inferred for " ++ f
                                                , canonical = False
                                                , fields = [ ( f, TypeVar Nothing <| typeVarPrefix ++ "record access inferred field for " ++ f ) ]
                                                }
                                        , to = TypeVar Nothing <| typeVarPrefix ++ "record access inferred field for " ++ f
                                        }
                           )

                Operator _ ->
                    Nothing

                OperatorApplication op _ e1 e2 ->
                    -- Handle a few known operators
                    checkOperatorApplication (inferApplicationChain local) op e1 e2
                        |> Maybe.map getType

                LetExpression { declarations, expression } ->
                    let
                        newBindings : Dict String Type
                        newBindings =
                            List.concatMap (bindingsFromLetDeclaration local << Node.value) declarations
                                |> Dict.fromList
                    in
                    inferExprType { local | localFunctions = Dict.union local.localFunctions newBindings } expression
                        |> Maybe.map (prefixTypeVars typeVarPrefix)
                        |> Maybe.map getType

                RecordUpdateExpression var fs ->
                    findFunctionType local Nothing var (Node.value var)
                        |> makeFunc typeVarPrefix
                        |> MaybeX.orElseLazy
                            (\() ->
                                MaybeX.traverse ((\( f, e ) -> go (typeVarPrefix ++ "Record Update Field " ++ Node.value f) e |> Maybe.map (Tuple.pair (Node.value f))) << Node.value) fs
                                    |> Maybe.map
                                        (\fields ->
                                            RecordType
                                                { generic = Just <| TypeVar Nothing <| typeVarPrefix ++ "inferred update generic"
                                                , canonical = False
                                                , fields = fields
                                                }
                                        )
                            )
    in
    go ""
        >> Maybe.map DereferencedType


{-| Infer type for a chain of expressions being applied to the first one.
-}
inferApplicationChain : LocalContext -> List (Node Expression) -> Maybe DereferencedType
inferApplicationChain local es =
    let
        inferExpr : Node Expression -> Maybe DereferencedType
        inferExpr =
            inferExprType local

        getReturnType : List (Node Expression) -> ( List DereferencedType, DereferencedType ) -> DereferencedType
        getReturnType args ( argTypes, returnType ) =
            List.map inferExpr args
                |> ListX.zip argTypes
                |> List.foldl
                    (\( t1, t2 ) typeVars ->
                        Maybe.map (typesMatch typeVars t1) t2
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault typeVars
                    )
                    Dict.empty
                |> Dict.filter (\( i, _ ) _ -> i == 1)
                |> DictX.mapKeys Tuple.second
                |> (\typeVars -> assignTypeVars typeVars returnType)
    in
    case es of
        [] ->
            Nothing

        [ e ] ->
            inferExpr e

        func :: args ->
            case Node.value func of
                FunctionOrValue _ name ->
                    findFunctionType local Nothing func name
                        |> MaybeX.orElseLazy (\() -> Maybe.map flattenFunctionType <| inferExpr func)
                        |> Maybe.andThen (partiallyApplyArgsAndTypes args)
                        -- Assign type vars
                        |> Maybe.map (getReturnType args)

                PrefixOperator op ->
                    findOperatorType local.context op
                        -- Definitely can't infer an operator type
                        |> Maybe.andThen (partiallyApplyArgsAndTypes args)
                        |> Maybe.map (getReturnType args)

                ParenthesizedExpression func_ ->
                    -- Unwrap the parentheses
                    inferApplicationChain local (func_ :: args)

                RecordAccessFunction accessFunc ->
                    -- Assume there's only one arg, since it's a type error if not
                    List.head args
                        |> Maybe.andThen inferExpr
                        |> getRecordFieldTypes
                        |> .types
                        |> (\ts ->
                                makeAccessFunc accessFunc
                                    |> (\f -> Dict.get f ts)
                           )

                _ ->
                    -- Try to infer the type
                    inferExpr func
                        |> Maybe.map flattenFunctionType
                        |> Maybe.andThen (partiallyApplyArgsAndTypes args)
                        |> Maybe.map (getReturnType args)


{-| Given a list of types, try to reduce them to a single type.
-}
unifyTypes : List Type -> Maybe Type
unifyTypes =
    List.foldl
        (\t acc ->
            -- Dedupe the slow way because custom types aren't comparable and order doesn't matter
            if List.member t acc then
                acc

            else
                t :: acc
        )
        []
        >> (\ts ->
                case ( ts, List.filter ((/=) (TypeVar Nothing "inferred empty list typevar")) ts ) of
                    ( [ t ], _ ) ->
                        Just t

                    ( _, [] ) ->
                        Nothing

                    ( _, [ t ] ) ->
                        Just t

                    ( _, t :: ts_ ) ->
                        List.foldl (\t_ acc -> Maybe.andThen (unifyTwoTypes t_) acc) (Just t) ts_
           )


{-| Given two types that are not the same, try to find a compatible type.
-}
unifyTwoTypes : Type -> Type -> Maybe Type
unifyTwoTypes type1 type2 =
    case ( type1, type2 ) of
        ( TypeVar Nothing _, _ ) ->
            Just type2

        ( _, TypeVar Nothing _ ) ->
            Just type1

        ( TypeVar (Just tc) _, _ ) ->
            validate (\t -> matchesTypeClass t tc) type2

        ( _, TypeVar (Just tc) _ ) ->
            validate (\t -> matchesTypeClass t tc) type1

        ( FunctionType f1, FunctionType f2 ) ->
            Maybe.map2 (\from to -> FunctionType { from = from, to = to })
                (unifyTypes [ f1.from, f2.from ])
                (unifyTypes [ f1.to, f2.to ])

        ( ListType t1, ListType t2 ) ->
            unifyTypes [ t1, t2 ]
                |> Maybe.map ListType

        ( TupleType t1s, TupleType t2s ) ->
            ListX.zip t1s t2s
                |> MaybeX.traverse (\( t1, t2 ) -> unifyTypes [ t1, t2 ])
                |> Maybe.map TupleType

        ( RecordType r1, RecordType r2 ) ->
            -- Divide fields into those that are present in both and those that are missing
            Dict.merge
                (\f t1 acc -> { acc | only1 = ( f, t1 ) :: acc.only1 })
                (\f t1 t2 acc ->
                    { acc
                        | both =
                            unifyTypes [ t1, t2 ]
                                |> Maybe.map (Tuple.pair f)
                                |> (\x -> x :: acc.both)
                    }
                )
                (\f t2 acc -> { acc | only2 = ( f, t2 ) :: acc.only2 })
                (Dict.fromList r1.fields)
                (Dict.fromList r2.fields)
                { only1 = [], both = [], only2 = [] }
                |> (\{ only1, both, only2 } ->
                        MaybeX.combine both
                            |> Maybe.andThen
                                (\both_ ->
                                    case ( ( r1.generic, only2 ), ( r2.generic, only1 ) ) of
                                        ( ( Just (TypeVar _ _), _ ), ( Just (TypeVar _ _), _ ) ) ->
                                            Just <| RecordType { r1 | fields = only1 ++ both_ ++ only2 }

                                        ( ( Nothing, [] ), ( Nothing, [] ) ) ->
                                            Just <| RecordType { r1 | fields = both_ }

                                        ( ( Just (TypeVar _ _), _ ), ( Nothing, [] ) ) ->
                                            Just <| RecordType { r1 | fields = both_ ++ only2 }

                                        ( ( Nothing, [] ), ( Just (TypeVar _ _), _ ) ) ->
                                            Just <| RecordType { r1 | fields = both_ ++ only1 }

                                        _ ->
                                            Nothing
                                )
                   )

        _ ->
            -- UnitType unifies with nothing but itself
            -- NamedType unifies with nothing but itself
            Nothing


{-| Report that an unknown record was encountered without fixes.
-}
unknownRecordError : RecordToCheck -> Range -> List (Error {})
unknownRecordError { fields } range =
    case fields of
        _ :: _ :: _ ->
            [ Rule.error
                { message = "Unknown record encountered."
                , details =
                    [ "This record did not correspond with any known alias or custom type argument record, so whether or not its fields are sorted could not be determined!"
                    , "Create a type alias for this record type, or remove reportUnknownRecordsWithoutFix from your rule configuration."
                    ]
                }
                range
            ]

        _ ->
            -- Do not report 0 or 1 field records
            []


{-| Report that an ambiguous record was encountered, along with a list of
matching type aliases.
-}
ambiguousRecordError : RecordToCheck -> List (List String) -> Range -> List (Error {})
ambiguousRecordError { fields } matching range =
    let
        prettyGeneric : List String -> String
        prettyGeneric s =
            case s of
                [] ->
                    ""

                [ x ] ->
                    x

                x :: xs ->
                    x ++ " (" ++ prettyGeneric xs ++ ")"
    in
    case fields of
        _ :: _ :: _ ->
            [ Rule.error
                { message = "Ambiguous record encountered."
                , details =
                    [ "This record could be one of several possible record aliases, so whether or not its fields are sorted could not be determined!"
                    , "Try adding a type annotation, or remove reportAmbiguousRecordsWithoutFix from your rule configuration."
                    , "The record matched the following possible aliases: " ++ String.join ", " (List.map prettyGeneric (List.sort matching))
                    ]
                }
                range
            ]

        _ ->
            -- Do not report 0 or 1 field records
            []
