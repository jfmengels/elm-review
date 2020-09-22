module ModuleNameForTypeTest exposing (all)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Fixtures.Dependencies as Dependencies
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


all : Test
all =
    Test.describe "Scope.moduleNameForType"
        [ test "should return the module that defined the type" <|
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
            case function.signature |> Maybe.map (Node.value >> .typeAnnotation) of
                Nothing ->
                    ( [], context )

                Just typeAnnotation ->
                    ( [], { context | texts = context.texts ++ typeAnnotationNames context typeAnnotation } )

        _ ->
            ( [], context )


typeAnnotationNames : ModuleContext -> Node TypeAnnotation -> List String
typeAnnotationNames moduleContext typeAnnotation =
    case Node.value typeAnnotation of
        TypeAnnotation.GenericType name ->
            [ "<nothing>." ++ name ++ " -> <generic>" ]

        TypeAnnotation.Typed (Node typeRange ( moduleName, typeName )) typeParameters ->
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
                    case ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable typeRange of
                        Just [] ->
                            "<nothing>." ++ typeName

                        Just moduleName_ ->
                            String.join "." moduleName_ ++ "." ++ typeName

                        Nothing ->
                            "!!! UNKNOWN !!!"
            in
            (nameInCode ++ " -> " ++ realName)
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
