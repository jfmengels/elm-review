module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs exposing (Module)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Type
import NoUnused.Patterns.NameVisitor as NameVisitor
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


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
    Rule.newProjectRuleSchema "NoImportingEverything" ()
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor (moduleVisitor exceptions)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = \_ previousContext -> previousContext
            }
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    ()


fromProjectToModule : Rule.ContextCreator ProjectContext Context
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , imports = Dict.empty
            , knownModules =
                Dict.singleton [ "OtherModule" ]
                    { name = ""
                    , comment = ""
                    , unions =
                        [ { name = "Custom"
                          , comment = ""
                          , args = []
                          , tags = [ ( "Variant", [] ) ]
                          }
                        ]
                    , aliases = []
                    , values = []
                    , binops = []
                    }
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator Context ProjectContext
fromModuleToProject =
    Rule.initContextCreator (\_ -> ())


moduleVisitor : List String -> Rule.ModuleRuleSchema schemaState Context -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } Context
moduleVisitor exceptions schema =
    schema
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> NameVisitor.withValueAndTypeVisitors { valueVisitor = valueVisitor, typeVisitor = typeVisitor }
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , imports : Dict ModuleName ImportData
    , knownModules : Dict ModuleName Module
    }


type alias ImportData =
    { dotdot : Range
    , used : Set String
    , importedCustomTypes : Set String
    }


exceptionsToSet : List String -> Set (List String)
exceptionsToSet exceptions =
    exceptions
        |> List.map (String.split ".")
        |> Set.fromList



-- DEPENDENCIES VISITOR


dependenciesVisitor : Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dict projectContext =
    ( [], projectContext )



-- IMPORT VISITOR


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
                    , { context | imports = Dict.update realModuleName (Maybe.map (registerUseOfValue context.knownModules realModuleName name)) context.imports }
                    )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )


registerUseOfValue : Dict ModuleName Module -> ModuleName -> String -> ImportData -> ImportData
registerUseOfValue knownModules moduleName name v =
    case Dict.get moduleName knownModules of
        Just { unions } ->
            case find (\union -> List.any (\( constructor, _ ) -> constructor == name) union.tags) unions of
                Just union ->
                    { v | importedCustomTypes = Set.insert union.name v.importedCustomTypes }

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
