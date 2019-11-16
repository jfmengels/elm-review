module Scope exposing (Context, SetterGetter, importVisitor, initialContext, realFunctionOrType)

{-| Report variables or types that are declared or imported but never used.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
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


type Context
    = Context InnerContext


type alias InnerContext =
    { scopes : Nonempty Scope
    , importAliases : Dict String (List String)
    , explicitlyImportedFunctionOrTypes : Dict String (List String)
    }


initialContext : Context
initialContext =
    Context
        { scopes = Nonempty.fromElement emptyScope
        , importAliases = Dict.empty
        , explicitlyImportedFunctionOrTypes = Dict.empty
        }


realFunctionOrType : List String -> String -> Context -> ( List String, String )
realFunctionOrType moduleName functionOrType (Context context) =
    if List.length moduleName == 0 then
        ( Dict.get functionOrType context.explicitlyImportedFunctionOrTypes
            |> Maybe.withDefault moduleName
        , functionOrType
        )

    else if List.length moduleName == 1 then
        ( Dict.get (String.join "." moduleName) context.importAliases
            |> Maybe.withDefault moduleName
        , functionOrType
        )

    else
        ( moduleName, functionOrType )


type alias SetterGetter context =
    { setter : Context -> context -> context
    , getter : context -> Context
    }


importVisitor : SetterGetter context -> Maybe (Node Import -> context -> ( List Error, context )) -> Node Import -> context -> ( List Error, context )
importVisitor { setter, getter } maybeVisitor =
    let
        visitor : Node Import -> context -> ( List Error, context )
        visitor =
            case maybeVisitor of
                Nothing ->
                    \node newContext -> ( [], newContext )

                Just fn ->
                    \node newContext -> fn node newContext
    in
    \((Node range import_) as node) outerContext ->
        outerContext
            |> getter
            |> unbox
            |> registerImportAlias import_
            |> registerExposed import_
            |> Context
            |> (\newContext ->
                    setter newContext outerContext
               )
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
                        (Node.value alias_ |> String.join ".")
                        (Node.value import_.moduleName)
                        innerContext.importAliases
            }


registerExposed : Import -> InnerContext -> InnerContext
registerExposed import_ innerContext =
    case import_.exposingList |> Maybe.map Node.value of
        Nothing ->
            innerContext

        Just (Exposing.All _) ->
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
                                case Node.value topLevelExpose of
                                    Exposing.InfixExpose operator ->
                                        ( operator, moduleName )

                                    Exposing.FunctionExpose function ->
                                        ( function, moduleName )

                                    Exposing.TypeOrAliasExpose type_ ->
                                        ( type_, moduleName )

                                    Exposing.TypeExpose { name } ->
                                        ( name, moduleName )
                            )
                        |> Dict.fromList
            in
            { innerContext
                | explicitlyImportedFunctionOrTypes =
                    Dict.union innerContext.explicitlyImportedFunctionOrTypes exposedValues
            }


unbox : Context -> InnerContext
unbox (Context context) =
    context


type alias Scope =
    { declared : Dict String VariableInfo
    , used : Set String
    }


type alias VariableInfo =
    { variableType : VariableType
    , under : Range
    , rangeToRemove : Range
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


emptyScope : Scope
emptyScope =
    { declared = Dict.empty
    , used = Set.empty
    }


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


getModuleName : List String -> String
getModuleName name =
    String.join "." name
