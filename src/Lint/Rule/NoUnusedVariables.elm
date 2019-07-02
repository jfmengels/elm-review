module Lint.Rule.NoUnusedVariables exposing (rule)

{-|

@docs rule


# Fail

    a n =
        n + 1

    b =
        a 2


# Success

    a n =
        n + 1

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Lint.Rule as Rule exposing (Direction, Error, Rule)
import List.Nonempty as Nonempty exposing (Nonempty)
import Set exposing (Set)


{-| Reports variables that are declared but never used.

    config =
        [ ( Critical, NoUnusedVariables.rule )
        ]

-}
rule : Rule
rule =
    Rule.newSchema "NoUnusedVariables"
        |> Rule.withInitialContext initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withFinalEvaluation finalEvaluation
        |> Rule.fromSchema


type alias Context =
    { scopes : Nonempty Scope
    , exposesEverything : Bool
    }


type alias Scope =
    { declared : Dict String ( VariableType, Range )
    , used : Set String
    }


type VariableType
    = Variable
    | ImportedModule
    | ImportedVariable
    | ImportedType
    | ImportedOperator
    | ModuleAlias
    | Type
    | Port


initialContext : Context
initialContext =
    { scopes = Nonempty.fromElement emptyScope
    , exposesEverything = False
    }


emptyScope : Scope
emptyScope =
    Scope Dict.empty Set.empty


error : VariableType -> Range -> String -> Error
error variableType range_ name =
    Rule.error
        (variableTypeToString variableType ++ " `" ++ name ++ "` is not used" ++ variableTypeWarning variableType)
        range_


variableTypeToString : VariableType -> String
variableTypeToString value =
    case value of
        Variable ->
            "Variable"

        ImportedModule ->
            "Imported module"

        ImportedVariable ->
            "Imported variable"

        ImportedType ->
            "Imported type"

        ImportedOperator ->
            "Imported operator"

        ModuleAlias ->
            "Module alias"

        Type ->
            "Type"

        Port ->
            "Port"


variableTypeWarning : VariableType -> String
variableTypeWarning value =
    case value of
        Variable ->
            ""

        ImportedModule ->
            ""

        ImportedVariable ->
            ""

        ImportedType ->
            ""

        ImportedOperator ->
            ""

        ModuleAlias ->
            ""

        Type ->
            ""

        Port ->
            " (Warning: Removing this port may break your application if it is used in the JS code)"


moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
moduleDefinitionVisitor moduleNode context =
    case Module.exposingList (Node.value moduleNode) of
        All _ ->
            ( [], { context | exposesEverything = True } )

        Explicit list ->
            let
                names =
                    List.filterMap
                        (\node ->
                            case Node.value node of
                                FunctionExpose name ->
                                    Just name

                                TypeOrAliasExpose name ->
                                    Just name

                                TypeExpose { name } ->
                                    Just name

                                InfixExpose name ->
                                    -- Just name
                                    Nothing
                        )
                        list
            in
            ( [], markAllAsUsed names context )


importVisitor : Node Import -> Context -> ( List Error, Context )
importVisitor node context =
    let
        exposed =
            node
                |> Node.value
                |> .exposingList
    in
    case Maybe.map Node.value exposed of
        Nothing ->
            let
                ( variableType, moduleName ) =
                    case Node.value node |> .moduleAlias of
                        Just moduleAlias ->
                            ( ModuleAlias, moduleAlias )

                        Nothing ->
                            ( ImportedModule, Node.value node |> .moduleName )
            in
            ( []
            , register
                variableType
                (Node.range moduleName)
                (Node.value moduleName |> getModuleName)
                context
            )

        Just declaredImports ->
            ( []
            , List.foldl
                (\( variableType, range, name ) context_ -> register variableType range name context_)
                context
                (collectFromExposing declaredImports)
            )


expressionVisitor : Node Expression -> Direction -> Context -> ( List Error, Context )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, FunctionOrValue [] name ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, FunctionOrValue moduleName name ) ->
            ( [], markAsUsed (getModuleName moduleName) context )

        ( Rule.OnEnter, OperatorApplication name _ _ _ ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, PrefixOperator name ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, LetExpression { declarations } ) ->
            let
                newContext =
                    List.foldl
                        (\declaration context_ ->
                            case Node.value declaration of
                                LetFunction function ->
                                    registerFunction function context_

                                LetDestructuring pattern _ ->
                                    context_
                        )
                        { context | scopes = Nonempty.cons emptyScope context.scopes }
                        declarations
            in
            ( [], newContext )

        ( Rule.OnExit, RecordUpdateExpression expr _ ) ->
            ( [], markAsUsed (Node.value expr) context )

        ( Rule.OnExit, LetExpression _ ) ->
            let
                ( errors, remainingUsed ) =
                    makeReport (Nonempty.head context.scopes)

                contextWithPoppedScope =
                    { context | scopes = Nonempty.pop context.scopes }
            in
            ( errors
            , markAllAsUsed remainingUsed contextWithPoppedScope
            )

        _ ->
            ( [], context )


declarationVisitor : Node Declaration -> Direction -> Context -> ( List Error, Context )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, FunctionDeclaration function ) ->
            let
                declaration =
                    Node.value function.declaration

                namesUsedInSignature =
                    function.signature
                        |> Maybe.map (Node.value >> .typeAnnotation >> collectNamesFromTypeAnnotation)
                        |> Maybe.withDefault []

                newContext =
                    context
                        |> register Variable (Node.range declaration.name) (Node.value declaration.name)
                        |> markAllAsUsed namesUsedInSignature
            in
            ( [], newContext )

        ( Rule.OnEnter, CustomTypeDeclaration { name, constructors } ) ->
            let
                variablesFromConstructorArguments : List String
                variablesFromConstructorArguments =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.concatMap collectNamesFromTypeAnnotation
            in
            ( []
            , context
                |> register Type (Node.range name) (Node.value name)
                |> markAllAsUsed variablesFromConstructorArguments
            )

        ( Rule.OnEnter, AliasDeclaration { name, typeAnnotation } ) ->
            ( []
            , context
                |> register Type (Node.range name) (Node.value name)
                |> markAllAsUsed (collectNamesFromTypeAnnotation typeAnnotation)
            )

        ( Rule.OnEnter, PortDeclaration { name, typeAnnotation } ) ->
            ( []
            , context
                |> markAllAsUsed (collectNamesFromTypeAnnotation typeAnnotation)
                |> register Port (Node.range name) (Node.value name)
            )

        ( Rule.OnEnter, InfixDeclaration _ ) ->
            ( [], context )

        ( Rule.OnEnter, Destructuring _ _ ) ->
            ( [], context )

        ( Rule.OnExit, _ ) ->
            ( [], context )


finalEvaluation : Context -> List Error
finalEvaluation context =
    if context.exposesEverything then
        []

    else
        context.scopes
            |> Nonempty.head
            |> makeReport
            |> Tuple.first


registerFunction : Function -> Context -> Context
registerFunction function context =
    let
        declaration =
            Node.value function.declaration
    in
    register Variable (Node.range declaration.name) (Node.value declaration.name) context


collectFromExposing : Exposing -> List ( VariableType, Range, String )
collectFromExposing exposing_ =
    case exposing_ of
        All _ ->
            []

        Explicit list ->
            List.filterMap
                (\node ->
                    case Node.value node of
                        FunctionExpose name ->
                            Just ( ImportedVariable, Node.range node, name )

                        InfixExpose name ->
                            Just ( ImportedOperator, Node.range node, name )

                        TypeOrAliasExpose name ->
                            Just ( ImportedType, Node.range node, name )

                        TypeExpose { name, open } ->
                            case open of
                                Just openRange ->
                                    Nothing

                                Nothing ->
                                    Just ( ImportedType, Node.range node, name )
                )
                list


collectNamesFromTypeAnnotation : Node TypeAnnotation -> List String
collectNamesFromTypeAnnotation node =
    case Node.value node of
        FunctionTypeAnnotation a b ->
            collectNamesFromTypeAnnotation a ++ collectNamesFromTypeAnnotation b

        Typed nameNode params ->
            let
                name =
                    case Node.value nameNode of
                        ( [], str ) ->
                            str

                        ( moduleName, _ ) ->
                            getModuleName moduleName
            in
            name :: List.concatMap collectNamesFromTypeAnnotation params

        Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectNamesFromTypeAnnotation

        GenericRecord name list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectNamesFromTypeAnnotation

        Tupled list ->
            List.concatMap collectNamesFromTypeAnnotation list

        GenericType _ ->
            []

        Unit ->
            []


register : VariableType -> Range -> String -> Context -> Context
register variableType range name context =
    let
        scopes =
            mapNonemptyHead
                (\scope ->
                    { scope | declared = Dict.insert name ( variableType, range ) scope.declared }
                )
                context.scopes
    in
    { context | scopes = scopes }


markAllAsUsed : List String -> Context -> Context
markAllAsUsed names context =
    List.foldl markAsUsed context names


markAsUsed : String -> Context -> Context
markAsUsed name context =
    let
        scopes =
            mapNonemptyHead
                (\scope ->
                    { scope | used = Set.insert name scope.used }
                )
                context.scopes
    in
    { context | scopes = scopes }


getModuleName : List String -> String
getModuleName name =
    String.join "." name


makeReport : Scope -> ( List Error, List String )
makeReport { declared, used } =
    let
        nonUsedVars : List String
        nonUsedVars =
            Set.diff used (Set.fromList <| Dict.keys declared)
                |> Set.toList

        errors : List Error
        errors =
            Dict.filter (\key _ -> not <| Set.member key used) declared
                |> Dict.toList
                |> List.map (\( key, ( variableType, range ) ) -> error variableType range key)
    in
    ( errors, nonUsedVars )


mapNonemptyHead : (a -> a) -> Nonempty a -> Nonempty a
mapNonemptyHead fn nonempty =
    let
        newHead =
            fn (Nonempty.head nonempty)
    in
    Nonempty.replaceHead newHead nonempty
