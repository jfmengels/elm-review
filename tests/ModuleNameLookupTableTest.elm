module ModuleNameLookupTableTest exposing (all)

import Elm.Docs
import Elm.License
import Elm.Package
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Version
import Fixtures.Dependencies as Dependencies
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Review.Test.Dependencies
import Review.Test.Dependencies.Unsafe as Unsafe
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ModuleNameLookupTable"
        [ moduleNameAtTest
        , fullModuleNameAtest
        , dependenciesTest
        ]


moduleNameAtTest : Test
moduleNameAtTest =
    describe "ModuleNameLookupTable.moduleNameAt"
        [ test "should return the module that defined the value" <|
            \() ->
                let
                    lookupFunction : ModuleNameLookupTable -> Range -> Maybe ModuleName
                    lookupFunction =
                        ModuleNameLookupTable.moduleNameAt

                    rule : Rule
                    rule =
                        createRule
                            (Rule.withExpressionEnterVisitor (expressionVisitor lookupFunction)
                                >> Rule.withDeclarationEnterVisitor (declarationVisitor lookupFunction)
                            )
                in
                [ """module A exposing (..)
import Bar as Baz exposing (baz)
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import ExposesEverything as ExposesEverythingAlias
import Foo.Bar
import Html exposing (..)
import Http exposing (get)
import Something.B as Something
import Something.C as Something
import Url.Parser exposing (..)
-- NOTE: The behavior of the compiler if duplicate infix operators are imported is for the second-imported one to overwrite the first.
import Parser exposing ((|=))
import Parser.Advanced exposing ((|=))

localValue = 1
localValueValueToBeShadowed = 1
type Msg = SomeMsgToBeShadowed

a = localValue
 localValueValueToBeShadowed
 SomeMsgToBeShadowed
 SomeOtherMsg
 Something.b
 Something.c
 Something.BAlias
 Something.Bar
 unknownValue
 exposedElement
 nonExposedElement
 elementFromExposesEverything
 VariantA
 Foo.bar
 Foo.Bar
 Baz.foo
 baz
 { baz | a = 1 }
 button
 Http.get
 get
 always
 True
 Just
 Cmd.none
 (let foo get = get
  in
  get
 )
 (+)
 (117 + 3)
 (<?>)
 ("x" </> "y")
 (|=)
 ("pars" |= "er")
b = case () of
  VariantA -> ()
  (ExposesEverything.VariantA as foo) -> foo
  ExposesEverythingAlias.VariantA -> ()

someFunction Something.B.Bar =
    let Something.B.Bar = ()
    in ()
""", """module ExposesSomeThings exposing (SomeOtherTypeAlias, exposedElement)
type NonExposedCustomType = Variant
type alias SomeOtherTypeAlias = {}
exposedElement = 1
nonExposedElement = 2
""", """module ExposesEverything exposing (..)
type SomeCustomType = VariantA | VariantB
type alias SomeTypeAlias = {}
type Msg = SomeMsgToBeShadowed | SomeOtherMsg
elementFromExposesEverything = 1
localValueValueToBeShadowed = 1
""", """module Something.B exposing (..)
b = 1
type Foo = Bar
type alias BAlias = {}
""", """module Something.C exposing (..)
c = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = """
<nothing>.localValue -> <nothing>.localValue
<nothing>.localValueValueToBeShadowed -> <nothing>.localValueValueToBeShadowed
<nothing>.SomeMsgToBeShadowed -> <nothing>.SomeMsgToBeShadowed
<nothing>.SomeOtherMsg -> ExposesEverything.SomeOtherMsg
Something.b -> Something.B.b
Something.c -> Something.C.c
Something.BAlias -> Something.B.BAlias
Something.Bar -> Something.B.Bar
<nothing>.unknownValue -> <nothing>.unknownValue
<nothing>.exposedElement -> ExposesSomeThings.exposedElement
<nothing>.nonExposedElement -> <nothing>.nonExposedElement
<nothing>.elementFromExposesEverything -> ExposesEverything.elementFromExposesEverything
<nothing>.VariantA -> ExposesEverything.VariantA
Foo.bar -> Foo.bar
Foo.Bar -> Foo.Bar
Baz.foo -> Bar.foo
<nothing>.baz -> Bar.baz
<nothing>.baz -> Bar.baz
<nothing>.button -> Html.button
Http.get -> Http.get
<nothing>.get -> Http.get
<nothing>.always -> Basics.always
<nothing>.True -> Basics.True
<nothing>.Just -> Maybe.Just
Cmd.none -> Platform.Cmd.none
<nothing>.get -> <nothing>.get
<nothing>.get -> Http.get
<nothing>.+ -> Basics.+
<nothing>.+ -> Basics.+
<nothing>.<?> -> Url.Parser.<?>
<nothing>.</> -> Url.Parser.</>
<nothing>.|= -> Parser.Advanced.|=
<nothing>.|= -> Parser.Advanced.|=
<nothing>.VariantA -> ExposesEverything.VariantA
ExposesEverything.VariantA -> ExposesEverything.VariantA
ExposesEverythingAlias.VariantA -> ExposesEverything.VariantA
<nothing>.foo -> <nothing>.foo
Something.B.Bar -> Something.B.Bar
Something.B.Bar -> Something.B.Bar
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should return the module that defined the type" <|
            \() ->
                let
                    lookupFunction : ModuleNameLookupTable -> Range -> Maybe ModuleName
                    lookupFunction =
                        ModuleNameLookupTable.moduleNameAt

                    rule : Rule
                    rule =
                        createRule
                            (Rule.withDeclarationEnterVisitor (declarationVisitor lookupFunction))
                in
                [ """module A exposing (..)
import Bar as Baz exposing (baz)
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import Foo.Bar
import Html exposing (..)
import Http exposing (get)
import Something.B as Something

type A = B | C
type Role = NormalUser Bool | Admin (Maybe A)
type alias User =
  { role : Role
  , age : ( Msg, Unknown )
  }
type alias GenericRecord generic = { generic | foo : A }

a : SomeCustomType -> SomeTypeAlias -> SomeOtherTypeAlias -> NonExposedCustomType
a = 1
""", """module ExposesSomeThings exposing (SomeOtherTypeAlias)
type NonExposedCustomType = Variant
type alias SomeOtherTypeAlias = {}
""", """module ExposesEverything exposing (..)
type SomeCustomType = VariantA | VariantB
type alias SomeTypeAlias = {}
type Msg = SomeMsgToBeShadowed | SomeOtherMsg
""", """module Something.B exposing (..)
b = 1
type Foo = Bar
type alias BAlias = {}
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = """
<nothing>.Bool -> Basics.Bool
<nothing>.Maybe -> Maybe.Maybe
<nothing>.A -> <nothing>.A
<nothing>.Role -> <nothing>.Role
<nothing>.Msg -> ExposesEverything.Msg
<nothing>.Unknown -> <nothing>.Unknown
<nothing>.A -> <nothing>.A
<nothing>.SomeCustomType -> ExposesEverything.SomeCustomType
<nothing>.SomeTypeAlias -> ExposesEverything.SomeTypeAlias
<nothing>.SomeOtherTypeAlias -> ExposesSomeThings.SomeOtherTypeAlias
<nothing>.NonExposedCustomType -> <nothing>.NonExposedCustomType
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]


fullModuleNameAtest : Test
fullModuleNameAtest =
    describe "ModuleNameLookupTable.fullModuleNameAt"
        [ test "should return the module that defined the value" <|
            \() ->
                let
                    lookupFunction : ModuleNameLookupTable -> Range -> Maybe ModuleName
                    lookupFunction =
                        ModuleNameLookupTable.fullModuleNameAt

                    rule : Rule
                    rule =
                        createRule
                            (Rule.withExpressionEnterVisitor (expressionVisitor lookupFunction)
                                >> Rule.withDeclarationEnterVisitor (declarationVisitor lookupFunction)
                            )
                in
                [ """module Abc.Xyz exposing (..)
import Bar as Baz exposing (baz)
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import ExposesEverything as ExposesEverythingAlias
import Foo.Bar
import Html exposing (..)
import Http exposing (get)
import Something.B as Something
import Something.C as Something
import Url.Parser exposing (..)
-- NOTE: The behavior of the compiler if duplicate infix operators are imported is for the second-imported one to overwrite the first.
import Parser exposing ((|=))
import Parser.Advanced exposing ((|=))

localValue = 1
localValueValueToBeShadowed = 1
type Msg = SomeMsgToBeShadowed

a = localValue
 localValueValueToBeShadowed
 SomeMsgToBeShadowed
 SomeOtherMsg
 Something.b
 Something.c
 Something.BAlias
 Something.Bar
 unknownValue
 exposedElement
 nonExposedElement
 elementFromExposesEverything
 VariantA
 Foo.bar
 Foo.Bar
 Baz.foo
 baz
 { baz | a = 1 }
 button
 Http.get
 get
 always
 True
 Just
 Cmd.none
 (+)
 (117 + 3)
 (<?>)
 ("x" </> "y")
 (|=)
 ("pars" |= "er")
b = case () of
  VariantA -> ()
  (ExposesEverything.VariantA as foo) -> foo
  ExposesEverythingAlias.VariantA -> ()

someFunction Something.B.Bar =
    let Something.B.Bar = ()
    in ()
""", """module ExposesSomeThings exposing (SomeOtherTypeAlias, exposedElement)
type NonExposedCustomType = Variant
type alias SomeOtherTypeAlias = {}
exposedElement = 1
nonExposedElement = 2
""", """module ExposesEverything exposing (..)
type SomeCustomType = VariantA | VariantB
type alias SomeTypeAlias = {}
type Msg = SomeMsgToBeShadowed | SomeOtherMsg
elementFromExposesEverything = 1
localValueValueToBeShadowed = 1
""", """module Something.B exposing (..)
b = 1
type Foo = Bar
type alias BAlias = {}
""", """module Something.C exposing (..)
c = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Abc.Xyz"
                          , [ Review.Test.error
                                { message = """
<nothing>.localValue -> Abc.Xyz.localValue
<nothing>.localValueValueToBeShadowed -> Abc.Xyz.localValueValueToBeShadowed
<nothing>.SomeMsgToBeShadowed -> Abc.Xyz.SomeMsgToBeShadowed
<nothing>.SomeOtherMsg -> ExposesEverything.SomeOtherMsg
Something.b -> Something.B.b
Something.c -> Something.C.c
Something.BAlias -> Something.B.BAlias
Something.Bar -> Something.B.Bar
<nothing>.unknownValue -> Abc.Xyz.unknownValue
<nothing>.exposedElement -> ExposesSomeThings.exposedElement
<nothing>.nonExposedElement -> Abc.Xyz.nonExposedElement
<nothing>.elementFromExposesEverything -> ExposesEverything.elementFromExposesEverything
<nothing>.VariantA -> ExposesEverything.VariantA
Foo.bar -> Foo.bar
Foo.Bar -> Foo.Bar
Baz.foo -> Bar.foo
<nothing>.baz -> Bar.baz
<nothing>.baz -> Bar.baz
<nothing>.button -> Html.button
Http.get -> Http.get
<nothing>.get -> Http.get
<nothing>.always -> Basics.always
<nothing>.True -> Basics.True
<nothing>.Just -> Maybe.Just
Cmd.none -> Platform.Cmd.none
<nothing>.+ -> Basics.+
<nothing>.+ -> Basics.+
<nothing>.<?> -> Url.Parser.<?>
<nothing>.</> -> Url.Parser.</>
<nothing>.|= -> Parser.Advanced.|=
<nothing>.|= -> Parser.Advanced.|=
<nothing>.VariantA -> ExposesEverything.VariantA
ExposesEverything.VariantA -> ExposesEverything.VariantA
ExposesEverythingAlias.VariantA -> ExposesEverything.VariantA
<nothing>.foo -> Abc.Xyz.foo
Something.B.Bar -> Something.B.Bar
Something.B.Bar -> Something.B.Bar
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should return the module that defined the type" <|
            \() ->
                let
                    lookupFunction : ModuleNameLookupTable -> Range -> Maybe ModuleName
                    lookupFunction =
                        ModuleNameLookupTable.fullModuleNameAt

                    rule : Rule
                    rule =
                        createRule
                            (Rule.withDeclarationEnterVisitor (declarationVisitor lookupFunction))
                in
                [ """module Abc.Xyz exposing (..)
import Bar as Baz exposing (baz)
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import Foo.Bar
import Html exposing (..)
import Http exposing (get)
import Something.B as Something

type A = B | C
type Role = NormalUser Bool | Admin (Maybe A)
type alias User =
  { role : Role
  , age : ( Msg, Unknown )
  }
type alias GenericRecord generic = { generic | foo : A }

a : SomeCustomType -> SomeTypeAlias -> SomeOtherTypeAlias -> NonExposedCustomType
a = 1
""", """module ExposesSomeThings exposing (SomeOtherTypeAlias)
type NonExposedCustomType = Variant
type alias SomeOtherTypeAlias = {}
""", """module ExposesEverything exposing (..)
type SomeCustomType = VariantA | VariantB
type alias SomeTypeAlias = {}
type Msg = SomeMsgToBeShadowed | SomeOtherMsg
""", """module Something.B exposing (..)
b = 1
type Foo = Bar
type alias BAlias = {}
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Abc.Xyz"
                          , [ Review.Test.error
                                { message = """
<nothing>.Bool -> Basics.Bool
<nothing>.Maybe -> Maybe.Maybe
<nothing>.A -> Abc.Xyz.A
<nothing>.Role -> Abc.Xyz.Role
<nothing>.Msg -> ExposesEverything.Msg
<nothing>.Unknown -> Abc.Xyz.Unknown
<nothing>.A -> Abc.Xyz.A
<nothing>.SomeCustomType -> ExposesEverything.SomeCustomType
<nothing>.SomeTypeAlias -> ExposesEverything.SomeTypeAlias
<nothing>.SomeOtherTypeAlias -> ExposesSomeThings.SomeOtherTypeAlias
<nothing>.NonExposedCustomType -> Abc.Xyz.NonExposedCustomType
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]


dependenciesTest : Test
dependenciesTest =
    describe "Dependencies"
        [ test "should not confuse a function from a local module with a module from an indirect dependency" <|
            \() ->
                let
                    lookupFunction : ModuleNameLookupTable -> Range -> Maybe ModuleName
                    lookupFunction =
                        ModuleNameLookupTable.moduleNameAt

                    rule : Rule
                    rule =
                        createRule
                            (Rule.withExpressionEnterVisitor (expressionVisitor lookupFunction))
                in
                [ """module A exposing (..)
import Element exposing (..)

a = value
""", """module Element exposing (value)
value = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = """
<nothing>.value -> Element.value
"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , texts : List String
    }


project : Project
project =
    Project.new
        |> Project.addElmJson applicationElmJson
        |> Project.addDependency Dependencies.elmCore
        |> Project.addDependency Dependencies.elmHtml
        |> Project.addDependency Review.Test.Dependencies.elmParser
        |> Project.addDependency Review.Test.Dependencies.elmUrl
        |> Project.addDependency xyz2dependency


applicationElmJson : { path : String, raw : String, project : Elm.Project.Project }
applicationElmJson =
    { path = "elm.json"
    , raw = """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "elm/html": "1.0.0",
            "elm/parser": "1.0.0",
            "elm/url": "1.0.0",
            "abc/xyz": "1.0.0"
        },
        "indirect": {
            "abc/xyz2": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""
    , project =
        Elm.Project.Application
            { elm = Elm.Version.one
            , dirs = []
            , depsDirect =
                [ ( unsafePackageName "elm/core", Elm.Version.one )
                , ( unsafePackageName "elm/html", Elm.Version.one )
                , ( unsafePackageName "elm/parser", Elm.Version.one )
                , ( unsafePackageName "elm/url", Elm.Version.one )
                , ( unsafePackageName "abc/xyz", Elm.Version.one )
                ]
            , depsIndirect = [ ( unsafePackageName "abc/xyz2", Elm.Version.one ) ]
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }


xyz2dependency : Dependency
xyz2dependency =
    Dependency.create "abc/xyz2"
        xyz2elmJson
        xyz2dependencyModules


xyz2elmJson : Elm.Project.Project
xyz2elmJson =
    Elm.Project.Package
        { elm = Unsafe.constraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ Unsafe.moduleName "Element" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = Unsafe.packageName "elm/xyz"
        , summary = "Fake stuff"
        , deps = [ ( Unsafe.packageName "elm/core", Unsafe.constraint "1.0.0 <= v < 2.0.0" ) ]
        , testDeps = []
        , version = Elm.Version.fromString "1.0.0" |> Maybe.withDefault Elm.Version.one
        }


xyz2dependencyModules : List Elm.Docs.Module
xyz2dependencyModules =
    [ { name = "Element"
      , comment = ""
      , unions = []
      , aliases = []
      , values = []
      , binops = []
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


createRule : (Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext) -> Rule
createRule visitor =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" contextCreator
        |> visitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


contextCreator : Rule.ContextCreator () ModuleContext
contextCreator =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , texts = []
            }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : (ModuleNameLookupTable -> Range -> Maybe ModuleName) -> Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor lookupFunction node context =
    case Node.value node of
        Expression.FunctionOrValue moduleName name ->
            ( [], { context | texts = context.texts ++ [ getRealName lookupFunction context moduleName (Node.range node) name ] } )

        Expression.RecordUpdateExpression (Node range name) _ ->
            ( [], { context | texts = context.texts ++ [ getRealName lookupFunction context [] range name ] } )

        Expression.PrefixOperator op ->
            ( [], { context | texts = context.texts ++ [ getRealName lookupFunction context [] (Node.range node) op ] } )

        Expression.OperatorApplication op _ _ _ ->
            ( [], { context | texts = context.texts ++ [ getRealName lookupFunction context [] (Node.range node) op ] } )

        Expression.CaseExpression { cases } ->
            let
                texts : List String
                texts =
                    List.concatMap (Tuple.first >> collectPatterns lookupFunction context) cases
            in
            ( [], { context | texts = context.texts ++ texts } )

        Expression.LetExpression { declarations } ->
            let
                texts : List String
                texts =
                    List.concatMap
                        (\declaration ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    let
                                        typeAnnotationTexts : List String
                                        typeAnnotationTexts =
                                            case function.signature |> Maybe.map (Node.value >> .typeAnnotation) of
                                                Nothing ->
                                                    []

                                                Just typeAnnotation ->
                                                    typeAnnotationNames lookupFunction context typeAnnotation

                                        signatureTexts : List String
                                        signatureTexts =
                                            function.declaration
                                                |> Node.value
                                                |> .arguments
                                                |> List.concatMap (collectPatterns lookupFunction context)
                                    in
                                    typeAnnotationTexts ++ signatureTexts

                                Expression.LetDestructuring pattern _ ->
                                    collectPatterns lookupFunction context pattern
                        )
                        declarations
            in
            ( [], { context | texts = context.texts ++ texts } )

        _ ->
            ( [], context )


collectPatterns : (ModuleNameLookupTable -> Range -> Maybe ModuleName) -> ModuleContext -> Node Pattern.Pattern -> List String
collectPatterns lookupFunction context node =
    case Node.value node of
        Pattern.NamedPattern { moduleName, name } _ ->
            [ getRealName lookupFunction context moduleName (Node.range node) name ]

        Pattern.ParenthesizedPattern subPattern ->
            collectPatterns lookupFunction context subPattern

        Pattern.AsPattern subPattern _ ->
            collectPatterns lookupFunction context subPattern

        Pattern.VarPattern _ ->
            []

        _ ->
            Debug.todo ("Other patterns in case expressions are not handled: " ++ Debug.toString node)


getRealName : (ModuleNameLookupTable -> Range -> Maybe ModuleName) -> ModuleContext -> ModuleName -> Range -> String -> String
getRealName lookupFunction context moduleName range name =
    let
        nameInCode : String
        nameInCode =
            case moduleName of
                [] ->
                    "<nothing>." ++ name

                _ ->
                    String.join "." moduleName ++ "." ++ name

        resultingName : String
        resultingName =
            case lookupFunction context.lookupTable range of
                Just [] ->
                    "<nothing>." ++ name

                Just moduleName_ ->
                    String.join "." moduleName_ ++ "." ++ name

                Nothing ->
                    "!!! UNKNOWN !!!"
    in
    nameInCode ++ " -> " ++ resultingName


declarationVisitor : (ModuleNameLookupTable -> Range -> Maybe ModuleName) -> Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor lookupFunction node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { constructors } ->
            let
                types : List String
                types =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.concatMap (typeAnnotationNames lookupFunction context)
            in
            ( [], { context | texts = context.texts ++ types } )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( [], { context | texts = context.texts ++ typeAnnotationNames lookupFunction context typeAnnotation } )

        Declaration.FunctionDeclaration function ->
            let
                typeAnnotationTexts : List String
                typeAnnotationTexts =
                    case function.signature |> Maybe.map (Node.value >> .typeAnnotation) of
                        Nothing ->
                            []

                        Just typeAnnotation ->
                            typeAnnotationNames lookupFunction context typeAnnotation

                signatureTexts : List String
                signatureTexts =
                    function.declaration
                        |> Node.value
                        |> .arguments
                        |> List.concatMap (collectPatterns lookupFunction context)
            in
            ( [], { context | texts = context.texts ++ typeAnnotationTexts ++ signatureTexts } )

        _ ->
            ( [], context )


typeAnnotationNames : (ModuleNameLookupTable -> Range -> Maybe ModuleName) -> ModuleContext -> Node TypeAnnotation -> List String
typeAnnotationNames lookupFunction moduleContext typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.GenericType name ->
            [ "<nothing>." ++ name ++ " -> <generic>" ]

        TypeAnnotation.Typed (Node typeRange ( moduleName, typeName )) typeParameters ->
            getRealName lookupFunction moduleContext moduleName typeRange typeName
                :: List.concatMap (typeAnnotationNames lookupFunction moduleContext) typeParameters

        TypeAnnotation.Unit ->
            []

        TypeAnnotation.Tupled typeAnnotations ->
            List.concatMap (typeAnnotationNames lookupFunction moduleContext) typeAnnotations

        TypeAnnotation.Record typeAnnotations ->
            List.concatMap (Node.value >> Tuple.second >> typeAnnotationNames lookupFunction moduleContext) typeAnnotations

        TypeAnnotation.GenericRecord _ typeAnnotations ->
            List.concatMap (Node.value >> Tuple.second >> typeAnnotationNames lookupFunction moduleContext) (Node.value typeAnnotations)

        TypeAnnotation.FunctionTypeAnnotation arg returnType ->
            typeAnnotationNames lookupFunction moduleContext arg ++ typeAnnotationNames lookupFunction moduleContext returnType


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    if List.isEmpty context.texts then
        []

    else
        [ Rule.error
            { message = "\n" ++ String.join "\n" context.texts ++ "\n"
            , details = [ "details" ]
            }
            { start = { row = 1, column = 1 }
            , end = { row = 1, column = 7 }
            }
        ]
