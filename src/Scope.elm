module Scope exposing
    ( Context, SetterGetter
    , initialContext, dependenciesVisitor, importVisitor, declarationListVisitor
    , realFunctionOrType
    )

{-| Report variables or types that are declared or imported but never used.


# Definition

@docs Context, SetterGetter


# Usage

@docs initialContext, dependenciesVisitor, importVisitor, declarationListVisitor


# Access

@docs realFunctionOrType

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import NonemptyList as Nonempty exposing (Nonempty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Direction, Error, Rule)
import Set exposing (Set)



-- DEFINITION


type Context
    = Context InnerContext


type alias InnerContext =
    { scopes : Nonempty (Dict String VariableInfo)
    , importAliases : Dict String (List String)
    , importedFunctionOrTypes : Dict String (List String)
    , dependencies : Dict String Elm.Docs.Module
    }


type alias SetterGetter context =
    { setter : Context -> context -> context
    , getter : context -> Context
    }



-- USAGE


initialContext : Context
initialContext =
    Context
        { scopes = Nonempty.fromElement Dict.empty
        , importAliases = Dict.empty
        , importedFunctionOrTypes = Dict.empty
        , dependencies = Dict.empty
        }


dependenciesVisitor : SetterGetter context -> Maybe (Dict String Elm.Docs.Module -> context -> context) -> Dict String Elm.Docs.Module -> context -> context
dependenciesVisitor { setter, getter } maybeVisitor =
    let
        visitor : Dict String Elm.Docs.Module -> context -> context
        visitor =
            case maybeVisitor of
                Nothing ->
                    \dependencies newContext -> newContext

                Just fn ->
                    fn
    in
    \dependencies outerContext ->
        outerContext
            |> getter
            |> unbox
            |> (\innerContext -> { innerContext | dependencies = dependencies })
            |> Context
            |> (\newContext -> setter newContext outerContext)
            |> visitor dependencies


declarationListVisitor : SetterGetter context -> Maybe (List (Node Declaration) -> context -> ( List Error, context )) -> List (Node Declaration) -> context -> ( List Error, context )
declarationListVisitor { setter, getter } maybeVisitor =
    let
        visitor : List (Node Declaration) -> context -> ( List Error, context )
        visitor =
            case maybeVisitor of
                Nothing ->
                    \declarations newContext -> ( [], newContext )

                Just fn ->
                    fn
    in
    \declarations outerContext ->
        outerContext
            |> getter
            |> unbox
            |> (\innerContext -> List.foldl registerDeclaration innerContext declarations)
            |> Context
            |> (\newContext -> setter newContext outerContext)
            |> visitor declarations


registerDeclaration : Node Declaration -> InnerContext -> InnerContext
registerDeclaration declaration innerContext =
    case declarationNameNode declaration of
        Just nameNode ->
            registerVariable
                { variableType = TopLevelVariable
                , node = nameNode
                }
                (Node.value nameNode)
                innerContext

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


registerVariable : VariableInfo -> String -> InnerContext -> InnerContext
registerVariable variableInfo name context =
    let
        scopes : Nonempty (Dict String VariableInfo)
        scopes =
            Nonempty.mapHead
                (Dict.insert name variableInfo)
                context.scopes
    in
    { context | scopes = scopes }


importVisitor : SetterGetter context -> Maybe (Node Import -> context -> ( List Error, context )) -> Node Import -> context -> ( List Error, context )
importVisitor { setter, getter } maybeVisitor =
    let
        visitor : Node Import -> context -> ( List Error, context )
        visitor =
            case maybeVisitor of
                Nothing ->
                    \node newContext -> ( [], newContext )

                Just fn ->
                    fn
    in
    \((Node range import_) as node) outerContext ->
        outerContext
            |> getter
            |> unbox
            |> registerImportAlias import_
            |> registerExposed import_
            |> Context
            |> (\newContext -> setter newContext outerContext)
            |> visitor node


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

        Just (Exposing.All _) ->
            let
                moduleName : List String
                moduleName =
                    Node.value import_.moduleName
            in
            case Dict.get (getModuleName moduleName) innerContext.dependencies of
                Just module_ ->
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

                Nothing ->
                    innerContext

        Just (Exposing.Explicit topLevelExposeList) ->
            let
                moduleName : List String
                moduleName =
                    Node.value import_.moduleName

                exposedValues : Dict String (List String)
                exposedValues =
                    topLevelExposeList
                        |> List.map
                            (\topLevelExpose ->
                                ( nameOfTopLevelExpose <| Node.value topLevelExpose
                                , moduleName
                                )
                            )
                        |> Dict.fromList
            in
            { innerContext
                | importedFunctionOrTypes =
                    Dict.union innerContext.importedFunctionOrTypes exposedValues
            }


nameOfTopLevelExpose : TopLevelExpose -> String
nameOfTopLevelExpose topLevelExpose =
    case topLevelExpose of
        Exposing.InfixExpose operator ->
            operator

        Exposing.FunctionExpose function ->
            function

        Exposing.TypeOrAliasExpose type_ ->
            type_

        Exposing.TypeExpose { name } ->
            name


unbox : Context -> InnerContext
unbox (Context context) =
    context


type alias VariableInfo =
    { variableType : VariableType
    , node : Node String
    }


type VariableType
    = TopLevelVariable
    | LetVariable
    | ImportedModule
    | ImportedItem ImportType
    | ModuleAlias { originalNameOfTheImport : String, exposesSomething : Bool }
    | Type
    | Port


type ImportType
    = ImportedVariable
    | ImportedType
    | ImportedOperator


getUsedTypesFromPattern : Node Pattern -> List String
getUsedTypesFromPattern patternNode =
    case Node.value patternNode of
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

        Pattern.TuplePattern patterns ->
            List.concatMap getUsedTypesFromPattern patterns

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern pattern1 pattern2 ->
            List.concatMap getUsedTypesFromPattern [ pattern1, pattern2 ]

        Pattern.ListPattern patterns ->
            List.concatMap getUsedTypesFromPattern patterns

        Pattern.VarPattern _ ->
            []

        Pattern.NamedPattern qualifiedNameRef patterns ->
            let
                usedVariable : List String
                usedVariable =
                    case qualifiedNameRef.moduleName of
                        [] ->
                            [ qualifiedNameRef.name ]

                        moduleName ->
                            []
            in
            usedVariable ++ List.concatMap getUsedTypesFromPattern patterns

        Pattern.AsPattern pattern alias_ ->
            getUsedTypesFromPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedTypesFromPattern pattern


getUsedModulesFromPattern : Node Pattern -> List String
getUsedModulesFromPattern patternNode =
    case Node.value patternNode of
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

        Pattern.TuplePattern patterns ->
            List.concatMap getUsedModulesFromPattern patterns

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern pattern1 pattern2 ->
            List.concatMap getUsedModulesFromPattern [ pattern1, pattern2 ]

        Pattern.ListPattern patterns ->
            List.concatMap getUsedModulesFromPattern patterns

        Pattern.VarPattern _ ->
            []

        Pattern.NamedPattern qualifiedNameRef patterns ->
            let
                usedVariable : List String
                usedVariable =
                    case qualifiedNameRef.moduleName of
                        [] ->
                            []

                        moduleName ->
                            [ getModuleName moduleName ]
            in
            usedVariable ++ List.concatMap getUsedModulesFromPattern patterns

        Pattern.AsPattern pattern alias_ ->
            getUsedModulesFromPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedModulesFromPattern pattern


collectTypesFromTypeAnnotation : Node TypeAnnotation -> List String
collectTypesFromTypeAnnotation node =
    case Node.value node of
        FunctionTypeAnnotation a b ->
            collectTypesFromTypeAnnotation a ++ collectTypesFromTypeAnnotation b

        Typed nameNode params ->
            let
                name : List String
                name =
                    case Node.value nameNode of
                        ( [], str ) ->
                            [ str ]

                        ( moduleName, _ ) ->
                            []
            in
            name ++ List.concatMap collectTypesFromTypeAnnotation params

        Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectTypesFromTypeAnnotation

        GenericRecord name list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectTypesFromTypeAnnotation

        Tupled list ->
            List.concatMap collectTypesFromTypeAnnotation list

        GenericType _ ->
            []

        Unit ->
            []


collectModuleNamesFromTypeAnnotation : Node TypeAnnotation -> List String
collectModuleNamesFromTypeAnnotation node =
    case Node.value node of
        FunctionTypeAnnotation a b ->
            collectModuleNamesFromTypeAnnotation a ++ collectModuleNamesFromTypeAnnotation b

        Typed nameNode params ->
            let
                name : List String
                name =
                    case Node.value nameNode of
                        ( [], str ) ->
                            []

                        ( moduleName, _ ) ->
                            [ getModuleName moduleName ]
            in
            name ++ List.concatMap collectModuleNamesFromTypeAnnotation params

        Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectModuleNamesFromTypeAnnotation

        GenericRecord name list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectModuleNamesFromTypeAnnotation

        Tupled list ->
            List.concatMap collectModuleNamesFromTypeAnnotation list

        GenericType _ ->
            []

        Unit ->
            []



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


isInScope : String -> Nonempty (Dict String VariableInfo) -> Bool
isInScope name scopes =
    Nonempty.any (Dict.member name) scopes



-- MISC


getModuleName : List String -> String
getModuleName name =
    String.join "." name
