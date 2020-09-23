module ModuleNameLookupTableTest exposing (all)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Fixtures.Dependencies as Dependencies
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


all : Test
all =
    Test.describe "ModuleNameLookupTable.moduleNameFor"
        [ test "should return the module that defined the value" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        createRule
                            (Rule.withExpressionEnterVisitor expressionVisitor
                                >> Rule.withDeclarationEnterVisitor declarationVisitor
                            )
                in
                [ """module A exposing (..)
import Bar as Baz exposing (baz)
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import Foo.Bar
import Html exposing (..)
import Http exposing (get)
import Something.B as Something
import Something.C as Something

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
b = case () of
  VariantA -> ()
  (ExposesEverything.VariantA as foo) -> foo

someFunction Something.B.Bar =
    let SomeThing.B.Bar = ()
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
<nothing>.VariantA -> ExposesEverything.VariantA
ExposesEverything.VariantA -> ExposesEverything.VariantA
<nothing>.foo -> <nothing>.foo
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
                    rule : Rule
                    rule =
                        createRule (Rule.withDeclarationEnterVisitor declarationVisitor)
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


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , texts : List String
    }


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.elmCore
        |> Project.addDependency Dependencies.elmHtml


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


expressionVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue moduleName name ->
            ( [], { context | texts = context.texts ++ [ getRealName context moduleName (Node.range node) name ] } )

        Expression.RecordUpdateExpression (Node range name) _ ->
            ( [], { context | texts = context.texts ++ [ getRealName context [] range name ] } )

        Expression.CaseExpression { cases } ->
            let
                texts : List String
                texts =
                    List.concatMap (Tuple.first >> collectPatterns context) cases
            in
            ( [], { context | texts = context.texts ++ texts } )

        _ ->
            ( [], context )


collectPatterns : ModuleContext -> Node Pattern.Pattern -> List String
collectPatterns context node =
    case Node.value node of
        Pattern.NamedPattern { moduleName, name } _ ->
            [ getRealName context moduleName (Node.range node) name ]

        Pattern.ParenthesizedPattern subPattern ->
            collectPatterns context subPattern

        Pattern.AsPattern subPattern _ ->
            collectPatterns context subPattern

        _ ->
            Debug.todo "Other patterns in case expressions are not handled"


getRealName : ModuleContext -> ModuleName -> Range -> String -> String
getRealName context moduleName range name =
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
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just [] ->
                    "<nothing>." ++ name

                Just moduleName_ ->
                    String.join "." moduleName_ ++ "." ++ name

                Nothing ->
                    "!!! UNKNOWN !!!"
    in
    nameInCode ++ " -> " ++ resultingName


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { constructors } ->
            let
                types : List String
                types =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.concatMap (typeAnnotationNames context)
            in
            ( [], { context | texts = context.texts ++ types } )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( [], { context | texts = context.texts ++ typeAnnotationNames context typeAnnotation } )

        Declaration.FunctionDeclaration function ->
            let
                typeAnnotationTexts : List String
                typeAnnotationTexts =
                    case function.signature |> Maybe.map (Node.value >> .typeAnnotation) of
                        Nothing ->
                            []

                        Just typeAnnotation ->
                            typeAnnotationNames context typeAnnotation
            in
            ( [], { context | texts = context.texts ++ typeAnnotationTexts } )

        _ ->
            ( [], context )


typeAnnotationNames : ModuleContext -> Node TypeAnnotation -> List String
typeAnnotationNames moduleContext typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.GenericType name ->
            [ "<nothing>." ++ name ++ " -> <generic>" ]

        TypeAnnotation.Typed (Node typeRange ( moduleName, typeName )) typeParameters ->
            getRealName moduleContext moduleName typeRange typeName
                :: List.concatMap (typeAnnotationNames moduleContext) typeParameters

        TypeAnnotation.Unit ->
            []

        TypeAnnotation.Tupled typeAnnotations ->
            List.concatMap (typeAnnotationNames moduleContext) typeAnnotations

        TypeAnnotation.Record typeAnnotations ->
            List.concatMap (Node.value >> Tuple.second >> typeAnnotationNames moduleContext) typeAnnotations

        TypeAnnotation.GenericRecord _ typeAnnotations ->
            List.concatMap (Node.value >> Tuple.second >> typeAnnotationNames moduleContext) (Node.value typeAnnotations)

        TypeAnnotation.FunctionTypeAnnotation arg returnType ->
            typeAnnotationNames moduleContext arg ++ typeAnnotationNames moduleContext returnType


finalEvaluation : ModuleContext -> List (Error {})
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
