module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleInformation as ModuleInformation exposing (ModuleInformation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Type.Union as Union
import Set exposing (Set)
import Vendor.NameVisitor as NameVisitor


{-| Forbids importing everything from a module.

When you import everything from a module, it becomes harder to know where a function
or a type comes from. The official guide even
[recommends against importing everything](https://guide.elm-lang.org/webapps/modules.html#using-modules).

    config =
        [ NoImportingEverything.rule []
        ]

Teams often have an agreement on the list of imports from which it is okay to expose everything, so
you can configure a list of exceptions.

    config =
        [ NoImportingEverything.rule [ "Html", "Some.Module" ]
        ]


## Fail

    import A exposing (..)
    import A as B exposing (..)


## Success

    import A as B exposing (B(..), C, d)

    -- If configured with `[ "Html" ]`
    import Html exposing (..)


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoImportingEverything
```

-}
rule : List String -> Rule
rule exceptions =
    Rule.newModuleRuleSchemaUsingContextCreator "NoImportingEverything" initialContext
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> NameVisitor.withValueAndTypeVisitors { valueVisitor = valueVisitor, typeVisitor = typeVisitor }
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , imports : Dict ModuleName ImportData
    , importedModulesAPI : Dict ModuleName ModuleInformation
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable importedModulesAPI () ->
            { lookupTable = lookupTable
            , imports = Dict.empty
            , importedModulesAPI = importedModulesAPI
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withImportedModulesAPI


type alias ImportData =
    { dotdot : Range
    , used : Set String
    , importedCustomTypes : Set String
    }



-- IMPORT VISITOR


exceptionsToSet : List String -> Set (List String)
exceptionsToSet exceptions =
    exceptions
        |> List.map (String.split ".")
        |> Set.fromList


importVisitor : Set (List String) -> Node Import -> Context -> ( List nothing, Context )
importVisitor exceptions node context =
    let
        moduleName : ModuleName
        moduleName =
            node
                |> Node.value
                |> .moduleName
                |> Node.value
    in
    if Set.member moduleName exceptions then
        ( [], context )

    else
        case
            Node.value node
                |> .exposingList
                |> Maybe.map Node.value
        of
            Just (Exposing.All range) ->
                ( []
                , { context
                    | imports =
                        Dict.insert moduleName
                            { dotdot = range
                            , used = Set.empty
                            , importedCustomTypes = Set.empty
                            }
                            context.imports
                  }
                )

            _ ->
                ( [], context )



-- NAME VISITOR


valueVisitor : Node ( ModuleName, String ) -> Context -> ( List nothing, Context )
valueVisitor (Node range ( moduleName, name )) context =
    case moduleName of
        [] ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just realModuleName ->
                    ( []
                    , { context | imports = Dict.update realModuleName (Maybe.map (registerUseOfValue context.importedModulesAPI realModuleName name)) context.imports }
                    )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


registerUseOfValue : Dict ModuleName ModuleInformation -> ModuleName -> String -> ImportData -> ImportData
registerUseOfValue importedModulesAPI moduleName name v =
    case Dict.get moduleName importedModulesAPI of
        Just api ->
            case find (\union -> List.any (\( constructor, _ ) -> constructor == name) (Union.constructors union)) (ModuleInformation.unions api) of
                Just union ->
                    { v | importedCustomTypes = Set.insert (Union.name union) v.importedCustomTypes }

                Nothing ->
                    { v | used = Set.insert name v.used }

        Nothing ->
            { v | used = Set.insert name v.used }


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if predicate head then
                Just head

            else
                find predicate rest


typeVisitor : Node ( ModuleName, String ) -> Context -> ( List nothing, Context )
typeVisitor (Node range ( moduleName, name )) context =
    case moduleName of
        [] ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just realModuleName ->
                    ( []
                    , { context
                        | imports =
                            Dict.update realModuleName
                                (\value ->
                                    case value of
                                        Just v ->
                                            Just { v | used = Set.insert name v.used }

                                        Nothing ->
                                            Nothing
                                )
                                context.imports
                      }
                    )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )



-- FINAL EVALUATION


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context.imports
        |> Dict.values
        |> List.map
            (\importData ->
                Rule.errorWithFix
                    { message = "Prefer listing what you wish to import and/or using qualified imports"
                    , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                    }
                    { start = { row = importData.dotdot.start.row, column = importData.dotdot.start.column - 1 }
                    , end = { row = importData.dotdot.end.row, column = importData.dotdot.end.column + 1 }
                    }
                    (fixForModule importData)
            )


fixForModule : ImportData -> List Fix
fixForModule importData =
    if Set.isEmpty importData.used && Set.isEmpty importData.importedCustomTypes then
        []

    else
        [ Fix.replaceRangeBy importData.dotdot (expose importData.importedCustomTypes importData.used) ]


expose : Set String -> Set String -> String
expose importedCustomTypes used =
    Set.union
        (Set.map (\typeName -> typeName ++ "(..)") importedCustomTypes)
        used
        |> Set.toList
        |> String.join ", "
