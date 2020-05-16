module ModuleNameForTypeTest exposing (all)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Fixtures.Dependencies as Dependencies
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Scope
import Test exposing (Test, test)


all : Test
all =
    Test.describe "Scope.moduleNameForType"
        [ test "should return the module that defined the type" <|
            \() ->
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
                    |> Review.Test.runOnModulesWithProjectData project projectRule
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
    { scope : Scope.ModuleContext
    , texts : List String
    }


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.elmCore
        |> Project.addDependency Dependencies.elmHtml


projectRule : Rule
projectRule =
    Rule.newProjectRuleSchema "TestRule" { scope = Scope.initialProjectContext }
        |> Scope.addProjectVisitors
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule =
                \_ _ projectContext ->
                    { scope = Scope.fromProjectToModule projectContext.scope
                    , texts = []
                    }
            , fromModuleToProject =
                \_ moduleNameNode moduleContext ->
                    { scope = Scope.fromModuleToProject moduleNameNode moduleContext.scope
                    }
            , foldProjectContexts = \a b -> { scope = Scope.foldProjectContexts a.scope b.scope }
            }
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


declarationVisitor : Node Declaration -> Rule.Direction -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.CustomTypeDeclaration { constructors } ) ->
            let
                types : List String
                types =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.concatMap (typeAnnotationNames context.scope)
            in
            ( [], { context | texts = context.texts ++ types } )

        ( Rule.OnEnter, Declaration.AliasDeclaration { typeAnnotation } ) ->
            ( [], { context | texts = context.texts ++ typeAnnotationNames context.scope typeAnnotation } )

        ( Rule.OnEnter, Declaration.FunctionDeclaration function ) ->
            case function.signature |> Maybe.map (Node.value >> .typeAnnotation) of
                Nothing ->
                    ( [], context )

                Just typeAnnotation ->
                    ( [], { context | texts = context.texts ++ typeAnnotationNames context.scope typeAnnotation } )

        _ ->
            ( [], context )


typeAnnotationNames : Scope.ModuleContext -> Node TypeAnnotation -> List String
typeAnnotationNames scope typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.GenericType name ->
            [ "<nothing>." ++ name ++ " -> <generic>" ]

        TypeAnnotation.Typed (Node _ ( moduleName, typeName )) typeParameters ->
            let
                nameInCode : String
                nameInCode =
                    case moduleName of
                        [] ->
                            "<nothing>." ++ typeName

                        _ ->
                            String.join "." moduleName ++ "." ++ typeName

                realName : String
                realName =
                    case Scope.moduleNameForType scope typeName moduleName of
                        [] ->
                            "<nothing>." ++ typeName

                        moduleName_ ->
                            String.join "." moduleName_ ++ "." ++ typeName
            in
            (nameInCode ++ " -> " ++ realName)
                :: List.concatMap (typeAnnotationNames scope) typeParameters

        TypeAnnotation.Unit ->
            []

        TypeAnnotation.Tupled typeAnnotations ->
            List.concatMap (typeAnnotationNames scope) typeAnnotations

        TypeAnnotation.Record typeAnnotations ->
            List.concatMap (Node.value >> Tuple.second >> typeAnnotationNames scope) typeAnnotations

        TypeAnnotation.GenericRecord _ typeAnnotations ->
            List.concatMap (Node.value >> Tuple.second >> typeAnnotationNames scope) (Node.value typeAnnotations)

        TypeAnnotation.FunctionTypeAnnotation arg returnType ->
            typeAnnotationNames scope arg ++ typeAnnotationNames scope returnType


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
