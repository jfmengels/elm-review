module Scope exposing
    ( Context
    , initialContext, addVisitors
    , realFunctionOrType
    )

{-| Report variables or types that are declared or imported but never used.


# Definition

@docs Context


# Usage

@docs initialContext, addVisitors


# Access

@docs realFunctionOrType

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import NonemptyList exposing (Nonempty)
import Review.Rule as Rule exposing (Direction, Error)



-- DEFINITION


type Context
    = Context InnerContext


type alias InnerContext =
    { scopes : Nonempty Scope
    , importAliases : Dict String (List String)
    , importedFunctionOrTypes : Dict String (List String)
    , dependencies : Dict String Elm.Docs.Module
    }


type alias Scope =
    { names : Dict String VariableInfo
    , cases : List ( Node Expression, Dict String VariableInfo )
    , caseToExit : Node Expression
    }


type alias SetterGetter context =
    { set : Context -> context -> context
    , get : context -> Context
    }



-- USAGE


initialContext : Context
initialContext =
    Context
        { scopes = NonemptyList.fromElement emptyScope
        , importAliases = Dict.empty
        , importedFunctionOrTypes = Dict.empty
        , dependencies = Dict.empty
        }


emptyScope : Scope
emptyScope =
    { names = Dict.empty
    , cases = []
    , caseToExit = Node Range.emptyRange (Expression.Literal "root")
    }


addVisitors :
    { set : Context -> context -> context
    , get : context -> Context
    }
    -> Rule.Schema anyType anything context
    -> Rule.Schema anyType { hasAtLeastOneVisitor : () } context
addVisitors setterGetter schema =
    schema
        |> Rule.withDependenciesVisitor
            (mapInnerContext setterGetter dependenciesVisitor)
        |> Rule.withImportVisitor
            (mapInnerContext setterGetter importVisitor |> pairWithNoErrors)
        |> Rule.withDeclarationListVisitor
            (mapInnerContext setterGetter declarationListVisitor |> pairWithNoErrors)
        |> Rule.withDeclarationVisitor
            (\visitedElement direction outerContext ->
                let
                    innerContext : InnerContext
                    innerContext =
                        outerContext
                            |> setterGetter.get
                            |> unbox
                            |> declarationVisitor visitedElement direction
                in
                ( [], setterGetter.set (Context innerContext) outerContext )
            )
        |> Rule.withExpressionVisitor
            (\visitedElement direction outerContext ->
                let
                    innerContext : InnerContext
                    innerContext =
                        outerContext
                            |> setterGetter.get
                            |> unbox
                            |> popScope visitedElement direction
                in
                ( [], setterGetter.set (Context innerContext) outerContext )
            )
        |> Rule.withExpressionVisitor
            (\visitedElement direction outerContext ->
                let
                    innerContext : InnerContext
                    innerContext =
                        outerContext
                            |> setterGetter.get
                            |> unbox
                            |> expressionVisitor visitedElement direction
                in
                ( [], setterGetter.set (Context innerContext) outerContext )
            )


mapInnerContext : SetterGetter context -> (visitedElement -> InnerContext -> InnerContext) -> visitedElement -> context -> context
mapInnerContext { set, get } visitor visitedElement outerContext =
    let
        innerContext : InnerContext
        innerContext =
            outerContext
                |> get
                |> unbox
                |> visitor visitedElement
    in
    set (Context innerContext) outerContext


pairWithNoErrors : (visited -> context -> context) -> visited -> context -> ( List Error, context )
pairWithNoErrors fn visited context =
    ( [], fn visited context )


dependenciesVisitor : Dict String Elm.Docs.Module -> InnerContext -> InnerContext
dependenciesVisitor dependencies innerContext =
    { innerContext | dependencies = dependencies }
        |> registerPrelude


registerPrelude : InnerContext -> InnerContext
registerPrelude innerContext =
    List.foldl registerExposed innerContext elmCorePrelude


elmCorePrelude : List Import
elmCorePrelude =
    let
        explicit : List TopLevelExpose -> Maybe Exposing
        explicit exposed =
            exposed
                |> List.map (Node Range.emptyRange)
                |> Exposing.Explicit
                |> Just
    in
    -- These are the default imports implicitly added by the Elm compiler
    -- https://package.elm-lang.org/packages/elm/core/latest
    [ createFakeImport
        { moduleName = [ "Basics" ]
        , moduleAlias = Nothing
        , exposingList = Just <| Exposing.All Range.emptyRange
        }
    , createFakeImport
        { moduleName = [ "List" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "List", open = Nothing }
                , Exposing.InfixExpose "::"
                ]
        }
    , createFakeImport
        { moduleName = [ "Maybe" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Maybe", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "Result" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Result", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "String" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Char", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Char" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Tuple" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Debug" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Platform" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Program", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Cmd" ]
        , moduleAlias = Just "Cmd"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Cmd", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Sub" ]
        , moduleAlias = Just "Sub"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Sub", open = Nothing }
                ]
        }
    ]


createFakeImport : { moduleName : List String, exposingList : Maybe Exposing, moduleAlias : Maybe String } -> Import
createFakeImport { moduleName, moduleAlias, exposingList } =
    { moduleName = Node Range.emptyRange moduleName
    , moduleAlias = moduleAlias |> Maybe.map (List.singleton >> Node Range.emptyRange)
    , exposingList = exposingList |> Maybe.map (Node Range.emptyRange)
    }


declarationListVisitor : List (Node Declaration) -> InnerContext -> InnerContext
declarationListVisitor declarations innerContext =
    List.foldl registerDeclaration innerContext declarations


registerDeclaration : Node Declaration -> InnerContext -> InnerContext
registerDeclaration declaration innerContext =
    case declarationNameNode declaration of
        Just nameNode ->
            innerContext.scopes
                |> registerVariable
                    { variableType = TopLevelVariable
                    , node = nameNode
                    }
                    (Node.value nameNode)
                |> updateScope innerContext

        Nothing ->
            innerContext


declarationNameNode : Node Declaration -> Maybe (Node String)
declarationNameNode (Node _ declaration) =
    case declaration of
        Declaration.FunctionDeclaration function ->
            function.declaration
                |> Node.value
                |> .name
                |> Just

        Declaration.CustomTypeDeclaration type_ ->
            Just type_.name

        Declaration.AliasDeclaration alias_ ->
            Just alias_.name

        Declaration.PortDeclaration port_ ->
            Just port_.name

        Declaration.InfixDeclaration _ ->
            Nothing

        Declaration.Destructuring _ _ ->
            Nothing


registerVariable : VariableInfo -> String -> Nonempty Scope -> Nonempty Scope
registerVariable variableInfo name scopes =
    NonemptyList.mapHead
        (\scope -> { scope | names = Dict.insert name variableInfo scope.names })
        scopes


updateScope : InnerContext -> Nonempty Scope -> InnerContext
updateScope context scopes =
    { context | scopes = scopes }


importVisitor : Node Import -> InnerContext -> InnerContext
importVisitor (Node range import_) innerContext =
    innerContext
        |> registerImportAlias import_
        |> registerExposed import_


registerImportAlias : Import -> InnerContext -> InnerContext
registerImportAlias import_ innerContext =
    case import_.moduleAlias of
        Nothing ->
            innerContext

        Just alias_ ->
            { innerContext
                | importAliases =
                    Dict.insert
                        (Node.value alias_ |> getModuleName)
                        (Node.value import_.moduleName)
                        innerContext.importAliases
            }


registerExposed : Import -> InnerContext -> InnerContext
registerExposed import_ innerContext =
    case import_.exposingList |> Maybe.map Node.value of
        Nothing ->
            innerContext

        Just exposing_ ->
            let
                moduleName : List String
                moduleName =
                    Node.value import_.moduleName

                module_ : Elm.Docs.Module
                module_ =
                    Dict.get (getModuleName moduleName) innerContext.dependencies
                        |> Maybe.withDefault
                            { name = getModuleName moduleName
                            , comment = ""
                            , unions = []
                            , values = []
                            , aliases = []
                            , binops = []
                            }
            in
            case exposing_ of
                Exposing.All _ ->
                    let
                        nameWithModuleName : { r | name : String } -> ( String, List String )
                        nameWithModuleName { name } =
                            ( name, moduleName )

                        exposedValues : Dict String (List String)
                        exposedValues =
                            List.concat
                                [ List.map nameWithModuleName module_.unions
                                , List.map nameWithModuleName module_.values
                                , List.map nameWithModuleName module_.aliases
                                , List.map nameWithModuleName module_.binops
                                ]
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctionOrTypes =
                            Dict.union innerContext.importedFunctionOrTypes exposedValues
                    }

                Exposing.Explicit topLevelExposeList ->
                    let
                        exposedValues : Dict String (List String)
                        exposedValues =
                            topLevelExposeList
                                |> List.concatMap (namesFromExposingList module_)
                                |> List.map (\name -> ( name, moduleName ))
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctionOrTypes =
                            Dict.union innerContext.importedFunctionOrTypes exposedValues
                    }


namesFromExposingList : Elm.Docs.Module -> Node TopLevelExpose -> List String
namesFromExposingList module_ topLevelExpose =
    case Node.value topLevelExpose of
        Exposing.InfixExpose operator ->
            [ operator ]

        Exposing.FunctionExpose function ->
            [ function ]

        Exposing.TypeOrAliasExpose type_ ->
            [ type_ ]

        Exposing.TypeExpose { name, open } ->
            case open of
                Just _ ->
                    name
                        :: (module_.unions
                                |> List.filter (\union -> union.name == name)
                                |> List.concatMap .tags
                                |> List.map Tuple.first
                           )

                Nothing ->
                    [ name ]


unbox : Context -> InnerContext
unbox (Context context) =
    context


type alias VariableInfo =
    { variableType : VariableType
    , node : Node String
    }


type VariableType
    = TopLevelVariable
    | FunctionParameter
    | LetVariable
    | PatternVariable
    | ImportedModule
    | ImportedItem ImportType
    | ModuleAlias { originalNameOfTheImport : String, exposesSomething : Bool }
    | Type
    | Port


type ImportType
    = ImportedVariable
    | ImportedType
    | ImportedOperator


declarationVisitor : Node Declaration -> Rule.Direction -> InnerContext -> InnerContext
declarationVisitor declaration direction context =
    case ( direction, Node.value declaration ) of
        ( Rule.OnEnter, Declaration.FunctionDeclaration function ) ->
            let
                newScope : Scope
                newScope =
                    { emptyScope | names = parameters <| .arguments <| Node.value function.declaration }
            in
            context.scopes
                |> NonemptyList.cons newScope
                |> updateScope context

        ( Rule.OnExit, Declaration.FunctionDeclaration function ) ->
            { context | scopes = NonemptyList.pop context.scopes }

        _ ->
            context


parameters : List (Node Pattern) -> Dict String VariableInfo
parameters patterns =
    List.concatMap collectNamesFromPattern patterns
        |> List.map
            (\node ->
                ( Node.value node
                , { node = node
                  , variableType = FunctionParameter
                  }
                )
            )
        |> Dict.fromList


collectNamesFromPattern : Node Pattern -> List (Node String)
collectNamesFromPattern pattern =
    case Node.value pattern of
        Pattern.AllPattern ->
            []

        Pattern.UnitPattern ->
            []

        Pattern.CharPattern _ ->
            []

        Pattern.StringPattern _ ->
            []

        Pattern.IntPattern _ ->
            []

        Pattern.HexPattern _ ->
            []

        Pattern.FloatPattern _ ->
            []

        Pattern.TuplePattern subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.RecordPattern names ->
            names

        Pattern.UnConsPattern left right ->
            List.concatMap collectNamesFromPattern [ left, right ]

        Pattern.ListPattern subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.VarPattern name ->
            [ Node (Node.range pattern) name ]

        Pattern.NamedPattern _ subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.AsPattern subPattern alias_ ->
            alias_ :: collectNamesFromPattern subPattern

        Pattern.ParenthesizedPattern subPattern ->
            collectNamesFromPattern subPattern


popScope : Node Expression -> Direction -> InnerContext -> InnerContext
popScope ((Node range value) as node) direction context =
    let
        currentScope : Scope
        currentScope =
            NonemptyList.head context.scopes
    in
    case direction of
        Rule.OnEnter ->
            let
                caseExpression : Maybe ( Node Expression, Dict String VariableInfo )
                caseExpression =
                    findInList (\( expressionNode, _ ) -> node == expressionNode) currentScope.cases
            in
            case caseExpression of
                Nothing ->
                    context

                Just ( _, names ) ->
                    { context | scopes = NonemptyList.cons { emptyScope | names = names, caseToExit = node } context.scopes }

        Rule.OnExit ->
            if node == currentScope.caseToExit then
                { context | scopes = NonemptyList.pop context.scopes }

            else
                context


expressionVisitor : Node Expression -> Direction -> InnerContext -> InnerContext
expressionVisitor ((Node range value) as node) direction context =
    case ( direction, value ) of
        ( Rule.OnEnter, Expression.LetExpression { declarations, expression } ) ->
            List.foldl
                (\declaration scopes ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            let
                                nameNode : Node String
                                nameNode =
                                    function.declaration
                                        |> Node.value
                                        |> .name
                            in
                            registerVariable
                                { variableType = LetVariable, node = nameNode }
                                -- TODO Check if the name as 2nd arg is not redundant with the 1st argument's node field
                                (Node.value nameNode)
                                scopes

                        Expression.LetDestructuring pattern _ ->
                            scopes
                )
                (NonemptyList.cons emptyScope context.scopes)
                declarations
                |> updateScope context

        ( Rule.OnExit, Expression.LetExpression _ ) ->
            { context | scopes = NonemptyList.pop context.scopes }

        ( Rule.OnEnter, Expression.CaseExpression caseBlock ) ->
            let
                cases : List ( Node Expression, Dict String VariableInfo )
                cases =
                    caseBlock.cases
                        |> List.map
                            (\( pattern, expression ) ->
                                ( expression
                                , collectNamesFromPattern pattern
                                    |> List.map
                                        (\node_ ->
                                            ( Node.value node_
                                            , { node = node_
                                              , variableType = PatternVariable
                                              }
                                            )
                                        )
                                    |> Dict.fromList
                                )
                            )
            in
            { context | scopes = NonemptyList.mapHead (\scope -> { scope | cases = cases }) context.scopes }

        ( Rule.OnExit, Expression.CaseExpression caseBlock ) ->
            { context | scopes = NonemptyList.mapHead (\scope -> { scope | cases = [] }) context.scopes }

        _ ->
            context


findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate list =
    case list of
        [] ->
            Nothing

        a :: rest ->
            if predicate a then
                Just a

            else
                findInList predicate rest



-- ACCESS


realFunctionOrType : List String -> String -> Context -> ( List String, String )
realFunctionOrType moduleName functionOrType (Context context) =
    if List.length moduleName == 0 then
        ( if isInScope functionOrType context.scopes then
            []

          else
            case Dict.get functionOrType context.importedFunctionOrTypes of
                Just importedFunctionOrType ->
                    importedFunctionOrType

                Nothing ->
                    []
        , functionOrType
        )

    else if List.length moduleName == 1 then
        ( Dict.get (getModuleName moduleName) context.importAliases
            |> Maybe.withDefault moduleName
        , functionOrType
        )

    else
        ( moduleName, functionOrType )


isInScope : String -> Nonempty Scope -> Bool
isInScope name scopes =
    NonemptyList.any (.names >> Dict.member name) scopes



-- MISC


getModuleName : List String -> String
getModuleName name =
    String.join "." name
