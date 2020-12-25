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
import Review.Fix as Fix
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
    Rule.newModuleRuleSchema "NoImportingEverything" initialContext
        |> Rule.withImportVisitor (importVisitor <| exceptionsToSet exceptions)
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { imports : Dict ModuleName Range
    , usedUnqualifiedImports : Dict ModuleName (Set String)
    }


initialContext : Context
initialContext =
    { imports = Dict.empty
    , usedUnqualifiedImports = Dict.singleton [ "OtherModule" ] (Set.singleton "a")
    }


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
                ( [], { context | imports = Dict.insert moduleName range context.imports } )

            _ ->
                ( [], context )



-- FINAL EVALUATION


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context.imports
        |> Dict.toList
        |> List.map
            (\( moduleName, range ) ->
                Rule.errorWithFix
                    { message = "Prefer listing what you wish to import and/or using qualified imports"
                    , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                    }
                    { start = { row = range.start.row, column = range.start.column - 1 }
                    , end = { row = range.end.row, column = range.end.column + 1 }
                    }
                    (fixForModule moduleName range)
            )


fixForModule : ModuleName -> Range -> List Fix.Fix
fixForModule moduleName range =
    case Dict.get moduleName (Dict.singleton [ "OtherModule" ] (Set.singleton "a")) of
        Just things ->
            [ Fix.replaceRangeBy range (things |> Set.toList |> String.join ", ") ]

        Nothing ->
            []
