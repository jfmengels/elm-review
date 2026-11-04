module ScopeTest exposing (all)

import Dict exposing (Dict)
import Elm.Syntax.File
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Expect
import Review.FileParser
import Review.Project as Project
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.Project.Valid as ValidProject
import Review.Resources.Binding as Binding
import Review.Resources.BindingId as BindingId
import Review.Resources.ImportBinding as ImportBinding
import Review.Resources.Internal.Bindings exposing (Binding)
import Review.Resources.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Resources.Reference as Reference exposing (Reference)
import Review.Resources.Scope as Scope
import Review.Resources.ScopeInternal as Internal exposing (Scope(..), ScopeInfo)
import Review.Resources.ScopeProvider as ScopeProvider exposing (ScopeProvider)
import Review.Resources.Target as Target
import Review.Test.Dependencies
import Set
import Test exposing (Test, describe, test)
import Unicode
import Vendor.ListExtra as ListExtra


all : Test
all =
    describe "Scope & ScopeProvider"
        [ scopeTest
        , qualifyModuleNameForTypeTest
        , qualifyModuleNameForValueTest
        ]


scopeTest : Test
scopeTest =
    describe "Scope"
        [ test "should list all known top-level elements" <|
            \() ->
                """
module A exposing (a, b)
a = 1
b = a
c = a + b
"""
                    |> getScopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.names
                    |> Set.toList
                    |> Expect.equal [ "a", "b", "c" ]
        , test "should get the binding for a variable name" <|
            \() ->
                """
module A exposing (a, b)
a = 1
b = a
c = a + b
"""
                    |> getScopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.bindings
                    |> Dict.toList
                    |> List.map (\( key, binding ) -> ( key, Binding.range binding ))
                    |> Expect.equal
                        [ ( "a", { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } )
                        , ( "b", { start = { row = 4, column = 1 }, end = { row = 4, column = 2 } } )
                        , ( "c", { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } )
                        ]
        , test "should find all references to a top-level variable" <|
            \() ->
                let
                    scopeProvider : ScopeProvider
                    scopeProvider =
                        getScopeProvider
                            """
module A exposing (a, b)
a = 1
b = a
c = a + b
fn r =
  { r | field = a }
record = { field = 1 }
recordUpdate = { record | field = 1 }
"""
                in
                scopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.bindings
                    |> Dict.toList
                    |> List.map
                        (\( key, binding ) ->
                            ( key
                            , Binding.id binding
                                |> ScopeProvider.valueReferences scopeProvider
                                |> List.map Reference.range
                            )
                        )
                    |> Expect.equal
                        [ ( "a"
                          , [ { start = { row = 7, column = 17 }, end = { row = 7, column = 18 } }
                            , { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            , { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            ]
                          )
                        , ( "b", [ { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } ] )
                        , ( "c", [] )
                        , ( "fn", [] )
                        , ( "record", [ { start = { row = 9, column = 18 }, end = { row = 9, column = 24 } } ] )
                        , ( "recordUpdate", [] )
                        ]
        , test "should collect all references in a file" <|
            \() ->
                let
                    (Internal.ScopeProvider { qualifiedReferences, valueReferences }) =
                        getScopeProvider
                            """
module A exposing (a, b)
a = Basics.pi
b = a
c = a + b
fn r =
  { r | field = a }
record = { field = 1 }
recordUpdate x y = { record | field = x |. y }
value = (|.)

infix left 6 (|.) = ignorer
ignorer _ z =
  z
"""
                in
                List.concat
                    [ valueReferences
                        |> Dict.toList
                        |> List.map
                            (\( key, refs ) ->
                                case refs of
                                    [] ->
                                        Debug.todo ("Empty reference list for " ++ String.fromInt key)

                                    (Internal.Reference ref) :: _ ->
                                        ( case ref.target of
                                            Target.LocalBinding binding ->
                                                Binding.name binding

                                            Target.ImportBinding_ importBinding ->
                                                String.join "." (ImportBinding.moduleName importBinding) ++ "." ++ ImportBinding.name importBinding

                                            Target.QualifiedReference importBinding ->
                                                String.join "." importBinding.moduleName ++ "." ++ importBinding.name
                                        , List.map Reference.range refs
                                        )
                            )
                    , qualifiedReferences
                        |> Dict.toList
                        |> List.map
                            (\( moduleName, refs ) ->
                                case refs of
                                    [] ->
                                        Debug.todo ("Empty module reference list for " ++ String.join "." moduleName)

                                    (Internal.Reference ref) :: _ ->
                                        ( case ref.target of
                                            Target.LocalBinding binding ->
                                                Binding.name binding

                                            Target.ImportBinding_ importBinding ->
                                                String.join "." (ImportBinding.moduleName importBinding) ++ "." ++ ImportBinding.name importBinding

                                            Target.QualifiedReference importBinding ->
                                                String.join "." importBinding.moduleName ++ "." ++ importBinding.name
                                        , List.map Reference.range refs
                                        )
                            )
                    ]
                    |> List.sortBy Tuple.first
                    |> Expect.equal
                        [ ( "Basics.+", [ { start = { row = 5, column = 5 }, end = { row = 5, column = 10 } } ] )
                        , ( "Basics.pi", [ { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } ] )
                        , ( "a"
                          , [ { start = { row = 7, column = 17 }, end = { row = 7, column = 18 } }
                            , { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            , { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            ]
                          )
                        , ( "b", [ { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } ] )
                        , ( "ignorer", [ { start = { row = 12, column = 21 }, end = { row = 12, column = 28 } } ] )
                        , ( "r", [ { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } } ] )
                        , ( "record", [ { start = { row = 9, column = 22 }, end = { row = 9, column = 28 } } ] )
                        , ( "x", [ { start = { row = 9, column = 39 }, end = { row = 9, column = 40 } } ] )
                        , ( "y", [ { start = { row = 9, column = 44 }, end = { row = 9, column = 45 } } ] )
                        , ( "z", [ { start = { row = 14, column = 3 }, end = { row = 14, column = 4 } } ] )
                        , ( "|."
                          , [ { start = { row = 10, column = 9 }, end = { row = 10, column = 13 } }
                            , { start = { row = 9, column = 39 }, end = { row = 9, column = 45 } }
                            ]
                          )
                        ]
        , test "top-level scope should have the scope of top-level functions as its children" <|
            \() ->
                let
                    source : String
                    source =
                        """
module A exposing (..)

fn n = n + 1
noScope = "noScope"
nestedScope = (\\n -> n)
letScope =
    let a = "letScope"
        subFnScope n = "subFnScope"
    in a
caseScopes =
    case 0 of
        0 -> "0branch"
        _ -> (\\lambda -> lambda)
"""

                    lines : List String
                    lines =
                        String.lines source
                in
                source
                    |> getScopeProvider
                    |> ScopeProvider.topLevel
                    |> Scope.children
                    |> List.map (\s -> extractSourceCode lines (Scope.range s))
                    |> Expect.equal
                        [ "(\\lambda -> lambda)"
                        , "\"0branch\""
                        , "let a = \"letScope\"\n        subFnScope n = \"subFnScope\"\n    in a"
                        , "n"
                        , "fn n = n + 1"
                        ]
        , test "should get the scope at a given range" <|
            \() ->
                let
                    source : String
                    source =
                        """
module A exposing (..)

fn n = n + 1
noScope = "noScope"
nestedScope = (\\n -> n)
letScope =
    let a = "letScope"
        subFnScope n = "subFnScope"
    in a
caseScopes =
    case 0 of
        0 -> "THISISTHETARGET"
        _ -> (\\lambda -> lambda)
"""

                    lines : List String
                    lines =
                        String.lines source

                    scopeProvider : ScopeProvider
                    scopeProvider =
                        getScopeProvider source
                in
                ScopeProvider.at scopeProvider { start = { row = 13, column = 14 }, end = { row = 13, column = 30 } }
                    |> (\s -> extractSourceCode lines (Scope.range s))
                    |> Expect.equal "\"THISISTHETARGET\""
        , test "should exit nested scopes that end at the same location" <|
            \() ->
                """
module A exposing (fn1)
fn1 a =
    let
        fn2 b =
            let
                fn3 c =
                    ()
            in
            ()
    in
    ()
"""
                    |> getScopeProvider
                    |> (\_ -> Expect.pass)
        ]


qualifyModuleNameForTypeTest : Test
qualifyModuleNameForTypeTest =
    describe "qualifyModuleNameForType"
        [ test "should return [] for types from Basics" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> qualifyModuleNameForType [ "Basics" ] "Bool"
                    |> Expect.equal []
        , test "should return Basics for types from Basics that are shadowed by local custom type" <|
            \() ->
                """module A exposing (..)
type Bool = Bool
"""
                    |> qualifyModuleNameForType [ "Basics" ] "Bool"
                    |> Expect.equal [ "Basics" ]
        , test "should return Basics for types from Basics that are shadowed by local type alias" <|
            \() ->
                """module A exposing (..)
type alias Bool = Bool
"""
                    |> qualifyModuleNameForType [ "Basics" ] "Bool"
                    |> Expect.equal [ "Basics" ]
        , test "should return module alias when module is aliased and a qualified import is needed (prelude)" <|
            \() ->
                """module A exposing (..)
import Basics as B
type alias Bool = Bool
"""
                    |> qualifyModuleNameForType [ "Basics" ] "Bool"
                    |> Expect.equal [ "B" ]
        , test "should return [] for types for a local type when requested module is []" <|
            \() ->
                """module A exposing (..)
type Local = Local
"""
                    |> qualifyModuleNameForType [] "Local"
                    |> Expect.equal []
        , test "should return [] for types for a local type when requested module is the current module name" <|
            \() ->
                """module A exposing (..)
type Local = Local
"""
                    |> qualifyModuleNameForType [ "A" ] "Local"
                    |> Expect.equal []
        , test "should return the module name itself for an imported type" <|
            \() ->
                """module A exposing (..)
import B
type Bool = Bool
"""
                    |> qualifyModuleNameForType [ "B" ] "Type"
                    |> Expect.equal [ "B" ]
        , test "should return the module alias itself for an imported type when one is present" <|
            \() ->
                """module A exposing (..)
import B as C
type Bool = Bool
"""
                    |> qualifyModuleNameForType [ "B" ] "Type"
                    |> Expect.equal [ "C" ]
        , test "should return [] if the type is explicitly imported from module" <|
            \() ->
                """module A exposing (..)
import B exposing (Type)
type Bool = Bool
"""
                    |> qualifyModuleNameForType [ "B" ] "Type"
                    |> Expect.equal []
        , test "should return module name if the type is explicitly imported but shadowed by local custom type" <|
            \() ->
                """module A exposing (..)
import B exposing (Type)
type Type = Type
"""
                    |> qualifyModuleNameForType [ "B" ] "Type"
                    |> Expect.equal [ "B" ]
        , test "should return module name if the type is explicitly imported but shadowed by local type alias" <|
            \() ->
                """module A exposing (..)
import B exposing (Type)
type alias Type = Type
"""
                    |> qualifyModuleNameForType [ "B" ] "Type"
                    |> Expect.equal [ "B" ]
        , test "should return [] if the type is imported by `exposing (..)`" <|
            \() ->
                """module A exposing (..)
import List exposing (..)
a = 1
"""
                    |> qualifyModuleNameForType [ "List" ] "List"
                    |> Expect.equal []
        , test "should return module name if the type is imported by `exposing (..)` but shadowed by local custom type" <|
            \() ->
                """module A exposing (..)
import List exposing (..)
type List = List
"""
                    |> qualifyModuleNameForType [ "List" ] "List"
                    |> Expect.equal [ "List" ]
        , test "should return module name if the type is imported by `exposing (..)` but shadowed by type alias" <|
            \() ->
                """module A exposing (..)
import List exposing (..)
type alias List = List
"""
                    |> qualifyModuleNameForType [ "List" ] "List"
                    |> Expect.equal [ "List" ]
        , test "should return the module name if the type is imported but by another module" <|
            \() ->
                """module A exposing (..)
import B
import C exposing (Type)
type alias List = List
"""
                    |> qualifyModuleNameForType [ "B" ] "Type"
                    |> Expect.equal [ "B" ]
        ]


qualifyModuleNameForType : ModuleName -> String -> String -> ModuleName
qualifyModuleNameForType moduleName typeName source =
    ScopeProvider.qualifyModuleNameForType
        (getScopeProvider source)
        moduleName
        typeName



-- TODO Check for constructor names as well


qualifyModuleNameForValueTest : Test
qualifyModuleNameForValueTest =
    describe "qualifyModuleNameForValue"
        [ test "should return [] for values from Basics" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> qualifyModuleNameForValue [ "Basics" ] "max"
                    |> Expect.equal []
        , test "should return Basics for values from Basics that are shadowed by top-level declaration" <|
            \() ->
                """module A exposing (..)
max = 10
"""
                    |> qualifyModuleNameForValue [ "Basics" ] "max"
                    |> Expect.equal [ "Basics" ]
        , test "should return Basics for values from Basics that are shadowed by a parameter" <|
            \() ->
                """module A exposing (..)
fn max =
  max
    + 10
"""
                    |> qualifyModuleNameForValueAt { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } } [ "Basics" ] "max"
                    |> Expect.equal [ "Basics" ]
        , test "should return [] for values from Basics that are shadowed by a parameter but not in the requested scope" <|
            \() ->
                """module A exposing (..)
fn max =
  max
    + 10
"""
                    |> qualifyModuleNameForValue [ "Basics" ] "max"
                    |> Expect.equal []
        , test "should return module alias when module is aliased and a qualified import is needed (prelude)" <|
            \() ->
                """module A exposing (..)
import Basics as B
max = 10
"""
                    |> qualifyModuleNameForValue [ "Basics" ] "max"
                    |> Expect.equal [ "B" ]
        , test "should return [] for values for a local value when requested module is []" <|
            \() ->
                """module A exposing (..)
xyz = 1
"""
                    |> qualifyModuleNameForValue [] "xyz"
                    |> Expect.equal []
        , test "should return [] for values for a local value when requested module is the current module name" <|
            \() ->
                """module A exposing (..)
xyz = 1
"""
                    |> qualifyModuleNameForValue [ "A" ] "xyz"
                    |> Expect.equal []
        , test "should return the module name itself for an imported value" <|
            \() ->
                """module A exposing (..)
import B
max = 10
"""
                    |> qualifyModuleNameForValue [ "B" ] "max"
                    |> Expect.equal [ "B" ]
        , test "should return the module alias itself for an imported value when one is present" <|
            \() ->
                """module A exposing (..)
import B as C
max = 10
"""
                    |> qualifyModuleNameForValue [ "B" ] "max"
                    |> Expect.equal [ "C" ]
        , test "should return [] if the value is explicitly imported from module" <|
            \() ->
                """module A exposing (..)
import B exposing (max)
a = 1
"""
                    |> qualifyModuleNameForValue [ "B" ] "max"
                    |> Expect.equal []
        , test "should return module name if the value is explicitly imported but shadowed by top-level declaration" <|
            \() ->
                """module A exposing (..)
import B exposing (max)
max = 10
"""
                    |> qualifyModuleNameForValue [ "B" ] "max"
                    |> Expect.equal [ "B" ]
        , test "should return module name if the value is explicitly imported but shadowed by parameter" <|
            \() ->
                """module A exposing (..)
import B exposing (max)
fn max =
  max
    + 10
"""
                    |> qualifyModuleNameForValueAt { start = { row = 3, column = 2 }, end = { row = 3, column = 5 } } [ "B" ] "max"
                    |> Expect.equal [ "B" ]
        , test "should return module name if the value is explicitly imported but shadowed by parameter in a parent non-toplevel scope" <|
            \() ->
                """module A exposing (..)
import B exposing (max)
fn max =
  let a = 1
  in
  max
    + a
"""
                    |> qualifyModuleNameForValueAt { start = { row = 6, column = 2 }, end = { row = 6, column = 5 } } [ "B" ] "max"
                    |> Expect.equal [ "B" ]
        , test "should return [] if the value is explicitly imported but shadowed by a parameter though not in the requested scope" <|
            \() ->
                """module A exposing (..)
import B exposing (max)
fn max =
  max
    + 10
"""
                    |> qualifyModuleNameForValue [ "B" ] "max"
                    |> Expect.equal []
        , test "should return [] if the value is imported by `exposing (..)`" <|
            \() ->
                """module A exposing (..)
import List exposing (..)
a = 1
"""
                    |> qualifyModuleNameForValue [ "List" ] "map"
                    |> Expect.equal []
        , test "should return module name if the value is imported by `exposing (..)` but shadowed by top-level declaration" <|
            \() ->
                """module A exposing (..)
import List exposing (..)
map = []
"""
                    |> qualifyModuleNameForValue [ "List" ] "map"
                    |> Expect.equal [ "List" ]
        , test "should return module name if the value is imported by `exposing (..)` but shadowed by parameter" <|
            \() ->
                """module A exposing (..)
import List exposing (..)
fn map =
  map
    + 10
"""
                    |> qualifyModuleNameForValueAt { start = { row = 3, column = 2 }, end = { row = 3, column = 5 } } [ "List" ] "map"
                    |> Expect.equal [ "List" ]
        , test "should return [] if the value is imported by `exposing (..)` and shadowed by parameter but not in the requested scope" <|
            \() ->
                """module A exposing (..)
import List exposing (..)
fn map =
  map
    + 10
"""
                    |> qualifyModuleNameForValue [ "List" ] "map"
                    |> Expect.equal []
        , test "should return the module name if the value is imported but by another module" <|
            \() ->
                """module A exposing (..)
import B
import C exposing (max)
a = 1
"""
                    |> qualifyModuleNameForValue [ "B" ] "max"
                    |> Expect.equal [ "B" ]
        , test "should return [] if the value is a custom type constructor implicitly imported by the prelude" <|
            \() ->
                """module A exposing (..)
a = 10
"""
                    |> qualifyModuleNameForValue [ "Basics" ] "True"
                    |> Expect.equal []
        ]


qualifyModuleNameForValue : ModuleName -> String -> String -> ModuleName
qualifyModuleNameForValue moduleName typeName source =
    Scope.qualifyModuleNameForValue
        (ScopeProvider.topLevel (getScopeProvider source))
        moduleName
        typeName


qualifyModuleNameForValueAt : Range -> ModuleName -> String -> String -> ModuleName
qualifyModuleNameForValueAt range moduleName typeName source =
    Scope.qualifyModuleNameForValue
        (ScopeProvider.at (getScopeProvider source) range)
        moduleName
        typeName


getScopeProvider : String -> ScopeProvider
getScopeProvider source =
    let
        ast : Elm.Syntax.File.File
        ast =
            parseOrCrash source

        module_ : OpaqueProjectModule
        module_ =
            ProjectModule.create
                { ast = ast
                , isInSourceDirectories = True
                , moduleId = 0
                , path = "src/A.elm"
                , source = source
                }

        moduleName : ModuleName
        moduleName =
            ProjectModule.moduleName module_

        ( _, scopeProvider, projectCache ) =
            ModuleNameLookupTable.compute moduleName
                module_
                { dependencies = ValidProject.computeDependencies (Project.directDependencies Review.Test.Dependencies.projectWithElmCore)
                , elmJsonContentHash = Nothing
                , lookupTables = Dict.empty
                , modules = Dict.empty
                , types = Nothing
                }

        ( _, scopeProviderOnReRun, _ ) =
            ModuleNameLookupTable.compute moduleName module_ projectCache
    in
    if scopeProvider == scopeProviderOnReRun then
        runChecks scopeProvider

    else
        Debug.todo "Oh no, scope provider is not the same on rerun!"


parseOrCrash : String -> Elm.Syntax.File.File
parseOrCrash source =
    case Review.FileParser.parse source of
        Ok ast ->
            ast

        Err err ->
            Debug.todo ("Invalid source:\n" ++ Debug.toString err)


runChecks : ScopeProvider -> ScopeProvider
runChecks scopeProvider =
    case
        topLevelIsActuallyTopLevel scopeProvider
            |> Result.andThen
                (\() ->
                    scopeIdsAreUnique Dict.empty [ ScopeProvider.topLevel scopeProvider ]
                        |> Result.andThen (scopesAreAccessible scopeProvider)
                )
            --|> allParentsPointToParent
            |> Result.andThen (\() -> bindingIdsAreUnique scopeProvider Dict.empty [ ScopeProvider.topLevel scopeProvider ])
    of
        Ok () ->
            scopeProvider

        Err error ->
            Debug.todo error


topLevelIsActuallyTopLevel : ScopeProvider -> Result String ()
topLevelIsActuallyTopLevel scopeProvider =
    let
        topLevel : Scope
        topLevel =
            ScopeProvider.topLevel scopeProvider
    in
    case Scope.parentId topLevel of
        Just _ ->
            Err ("Oh no! The top-level is not really the top-level!\n\n" ++ Debug.toString topLevel)

        Nothing ->
            Ok ()


scopeIdsAreUnique : Dict Int ScopeInfo -> List Scope -> Result String (Dict Int ScopeInfo)
scopeIdsAreUnique seenScopes scopes =
    case scopes of
        [] ->
            Ok seenScopes

        (Scope scope) :: rest ->
            case Dict.get scope.id seenScopes of
                Nothing ->
                    scopeIdsAreUnique (Dict.insert scope.id scope seenScopes) (scope.children ++ rest)

                Just previousScope ->
                    [ "Duplicate scope ids!"
                    , Debug.toString previousScope
                    , Debug.toString scope
                    ]
                        |> String.join "\n\n"
                        |> Err


scopesAreAccessible : ScopeProvider -> Dict Int ScopeInfo -> Result String ()
scopesAreAccessible (Internal.ScopeProvider scopeProvider) seenScopes =
    if Dict.keys seenScopes == Dict.keys scopeProvider.scopes then
        Ok ()

    else
        ("""Some scopes are not accessible through scope.children!

All scopes:       """ ++ Debug.toString (Dict.keys scopeProvider.scopes) ++ """

Through children: """ ++ Debug.toString (Dict.keys seenScopes))
            |> Err


bindingIdsAreUnique : ScopeProvider -> Dict Int Binding -> List Scope -> Result String ()
bindingIdsAreUnique scopeProvider seenBindings scopes =
    case scopes of
        [] ->
            Ok ()

        (Scope scope) :: rest ->
            case
                Dict.foldl
                    (\_ binding acc ->
                        case acc of
                            Err _ ->
                                acc

                            Ok soFar ->
                                let
                                    (BindingId.Id id) =
                                        Binding.id binding
                                in
                                case Dict.get id soFar of
                                    Nothing ->
                                        case ListExtra.findMap (referencePointTo binding) (ScopeProvider.valueReferences scopeProvider (Binding.id binding)) of
                                            Just err ->
                                                Err err

                                            Nothing ->
                                                Ok (Dict.insert id binding soFar)

                                    Just previousBinding ->
                                        [ "Duplicate binding ids!"
                                        , Debug.toString previousBinding
                                        , Debug.toString binding
                                        ]
                                            |> String.join "\n\n"
                                            |> Err
                    )
                    (Ok seenBindings)
                    scope.valueBindings
            of
                Ok bindings ->
                    bindingIdsAreUnique scopeProvider bindings (scope.children ++ rest)

                Err error ->
                    Err error


referencePointTo : Binding -> Reference -> Maybe String
referencePointTo binding (Internal.Reference reference) =
    case reference.target of
        Target.LocalBinding localBinding ->
            if localBinding /= binding then
                "Binding has a reference that doesn't point to itself!\n"
                    ++ Debug.toString binding
                    ++ "\nhas a reference\n"
                    ++ Debug.toString reference
                    ++ "\nthat points to\n"
                    ++ Debug.toString localBinding
                    |> Just

            else
                Nothing

        _ ->
            "Binding has a reference that doesn't point to itself!\n"
                ++ Debug.toString binding
                ++ "\nhas a reference\n"
                ++ Debug.toString reference
                ++ "\nthat points to\n"
                ++ Debug.toString reference.target
                |> Just


extractSourceCode : List String -> Range -> String
extractSourceCode lines { start, end } =
    case List.drop (start.row - 1) lines of
        [] ->
            ""

        firstLine :: rest ->
            if start.row == end.row then
                Unicode.slice
                    (start.column - 1)
                    (end.column - 1)
                    firstLine

            else
                let
                    { lastLine, linesTaken } =
                        takeNLines (end.row - start.row - 1) rest ""
                in
                Unicode.dropLeft (start.column - 1) firstLine
                    ++ linesTaken
                    ++ (case lastLine of
                            Just lastLine_ ->
                                "\n" ++ Unicode.left (end.column - 1) lastLine_

                            Nothing ->
                                ""
                       )


takeNLines : Int -> List String -> String -> { lastLine : Maybe String, linesTaken : String }
takeNLines n lines linesTaken =
    if n <= 0 then
        { lastLine = List.head lines, linesTaken = linesTaken }

    else
        case lines of
            [] ->
                { lastLine = Nothing, linesTaken = linesTaken }

            line :: rest ->
                takeNLines (n - 1)
                    rest
                    (linesTaken ++ "\n" ++ line)
