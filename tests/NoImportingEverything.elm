module NoImportingEverything exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import NoUnused.Patterns.NameVisitor as NameVisitor
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoImportingEverything" initialContext
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> NameVisitor.withValueVisitor nameVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , imports : Dict ModuleName ImportData
    }


type alias ImportData =
    { dotdot : Range
    , used : Set String
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , imports = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


exceptionsToSet : List String -> Set (List String)
exceptionsToSet exceptions =
    exceptions
        |> List.map (String.split ".")
        |> Set.fromList



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
                ( [], { context | imports = Dict.insert moduleName { dotdot = range, used = Set.empty } context.imports } )

            _ ->
                ( [], context )



-- NAME VISITOR


nameVisitor : Node ( ModuleName, String ) -> Context -> ( List nothing, Context )
nameVisitor node context =
    case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
        Just moduleName ->
            ( []
            , { context
                | imports =
                    Dict.update moduleName
                        (\value ->
                            case value of
                                Just v ->
                                    Just { v | used = Set.insert (Node.value node |> Tuple.second) v.used }

                                Nothing ->
                                    Nothing
                        )
                        context.imports
              }
            )

        Nothing ->
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


fixForModule : ImportData -> List Fix.Fix
fixForModule importData =
    [ Fix.replaceRangeBy importData.dotdot (importData.used |> Set.toList |> String.join ", ") ]
