module Docs.NoMissing exposing
    ( rule
    , What, everything, onlyExposed
    , From, allModules, exposedModules
    )

{-|

@docs rule


## Configuration

@docs What, everything, onlyExposed
@docs From, allModules, exposedModules


## When (not) to enable this rule

This rule is useful when you care about having a thoroughly or increasingly documented project.
It is also useful when you write Elm packages, in order to know about missing documentation before you publish.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.NoMissing
```

-}

import Docs.Utils.ExposedFromProject as ExposedFromProject
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports missing or empty documentation for functions and types.

    import Docs.NoMissing exposing (exposedModules, onlyExposed)

    config =
        [ Docs.NoMissing.rule
            { document = onlyExposed
            , from = exposedModules
            }
        ]


## Fail

The rule will report when documentation is missing

    someFunction =
        great Things

or when documentation is present but empty.

    {-| -}
    someOtherFunction =
        other (great Things)

The reasoning for not allowing empty documentation is because of how people consume documentation can vary, and for some
of those ways, empty documentation doesn't lead to a nice experience. For instance, if you are looking at Elm code in
your IDE and want to lookup the definition of a function, an empty documentation will give you no information beyond the
type annotation.

When I write documentation for a module, I try to tell a story or somehow phrase it as a tutorial, so that people can
learn the easier concepts first, and gradually as they read more and learn more about the ideas and concepts, I will
assume that they read most of the documentation above.

But for every function or type, I also imagine that they'll be read on their own from an IDE for instance, and therefore
try to make the documentation as light as possible while giving a helpful description and an example, without relying
too much on the assumption that the user has read the rest of the module.

A common case where people don't give an example is when exposing functions such as `map2`, `map3`, `map4`, etc., usually
documented in that order and next to each other. While `map2` is usually properly documented, the following ones would
have empty documentation, which I believe would be because the author assumes that the user went through the documentation on
the package registry and has read the documentation for `map2`. But if someone unfamiliar with Elm or an API looks up
`map3`, they may have trouble finding the information they were looking for.

I would recommend to make the documentation for each element as understandable out of context as possible. At the very least,
I would advise to say something like "This function is like `map2` but with X arguments" with a link to `map2`, so that
relevant information _can_ be found without too much effort.


## Success

    {-| someFunction does great things
    -}
    someFunction =
        great Things

-}
rule : { document : What, from : From } -> Rule
rule configuration =
    Rule.newModuleRuleSchema "Docs.NoMissing" initialContext
        |> Rule.withElmJsonModuleVisitor elmJsonVisitor
        |> Rule.withModuleDefinitionVisitor (moduleDefinitionVisitor configuration.from)
        |> Rule.withModuleDocumentationVisitor moduleDocumentationVisitor
        |> Rule.withDeclarationEnterVisitor (declarationVisitor configuration.document)
        |> Rule.fromModuleRuleSchema


type alias Context =
    { moduleNameNode : Node String
    , exposedModules : Set String
    , exposedElements : Exposed
    , shouldBeReported : Bool
    }


initialContext : Context
initialContext =
    { moduleNameNode = Node Range.emptyRange ""
    , exposedModules = Set.empty
    , exposedElements = EverythingIsExposed
    , shouldBeReported = True
    }


type Exposed
    = EverythingIsExposed
    | ExplicitList (Set String)


{-| Which elements from a module should be documented. Possible options are [`everything`](#everything) in a module or
only the exposed elements of a module ([`onlyExposed`](#onlyExposed)).
-}
type What
    = Everything
    | OnlyExposed


{-| Every function and type from a module should be documented. The module definition should also be documented.
-}
everything : What
everything =
    Everything


{-| Only exposed functions and types from a module should be documented. The module definition should also be documented.
-}
onlyExposed : What
onlyExposed =
    OnlyExposed


{-| Which modules should be documented. Possible options are [`allModules`](#allModules) of a project or
only the [`exposedModules`](#exposedModules) (only for packages).
-}
type From
    = AllModules
    | ExposedModules


{-| All modules from the project should be documented.
-}
allModules : From
allModules =
    AllModules


{-| Only exposed modules from the project will need to be documented.

If your project is an application, you should not use this option. An application does not expose modules which would
mean there isn't any module to report errors for.

-}
exposedModules : From
exposedModules =
    -- TODO Report a global error if used inside an application
    ExposedModules



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe Elm.Project.Project -> Context -> Context
elmJsonVisitor maybeProject context =
    let
        exposedModules_ : Set String
        exposedModules_ =
            case maybeProject of
                Just project ->
                    ExposedFromProject.exposedModules project

                _ ->
                    Set.empty
    in
    { context | exposedModules = exposedModules_ }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : From -> Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor fromConfig node context =
    let
        moduleNameNode : Node String
        moduleNameNode =
            case Node.value node of
                Module.NormalModule x ->
                    Node
                        (Node.range x.moduleName)
                        (Node.value x.moduleName |> String.join ".")

                Module.PortModule x ->
                    Node
                        (Node.range x.moduleName)
                        (Node.value x.moduleName |> String.join ".")

                Module.EffectModule x ->
                    Node
                        (Node.range x.moduleName)
                        (Node.value x.moduleName |> String.join ".")

        shouldBeReported : Bool
        shouldBeReported =
            case fromConfig of
                AllModules ->
                    True

                ExposedModules ->
                    Set.member (Node.value moduleNameNode) context.exposedModules

        exposed : Exposed
        exposed =
            case Node.value node |> Module.exposingList of
                Exposing.All _ ->
                    EverythingIsExposed

                Exposing.Explicit list ->
                    ExplicitList (List.map collectExposing list |> Set.fromList)
    in
    ( []
    , { context
        | moduleNameNode = moduleNameNode
        , shouldBeReported = shouldBeReported
        , exposedElements = exposed
      }
    )


collectExposing : Node Exposing.TopLevelExpose -> String
collectExposing node =
    case Node.value node of
        Exposing.InfixExpose name ->
            name

        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.TypeExpose exposedType ->
            exposedType.name



-- MODULE DOCUMENTATION VISITOR


moduleDocumentationVisitor : Maybe (Node String) -> Context -> ( List (Rule.Error {}), Context )
moduleDocumentationVisitor moduleDocumentation context =
    if context.shouldBeReported then
        ( checkModuleDocumentation moduleDocumentation context.moduleNameNode
        , context
        )

    else
        ( [], context )



-- DECLARATION VISITOR


declarationVisitor : What -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor documentWhat node context =
    if context.shouldBeReported then
        ( reportDeclarationDocumentation documentWhat context node
        , context
        )

    else
        ( [], context )


reportDeclarationDocumentation : What -> Context -> Node Declaration -> List (Error {})
reportDeclarationDocumentation documentWhat context node =
    case Node.value node of
        Declaration.FunctionDeclaration { documentation, declaration } ->
            let
                nameNode : Node String
                nameNode =
                    (Node.value declaration).name
            in
            if shouldBeDocumented documentWhat context (Node.value nameNode) then
                checkElementDocumentation documentation nameNode

            else
                []

        Declaration.CustomTypeDeclaration { documentation, name } ->
            if shouldBeDocumented documentWhat context (Node.value name) then
                checkElementDocumentation documentation name

            else
                []

        Declaration.AliasDeclaration { documentation, name } ->
            if shouldBeDocumented documentWhat context (Node.value name) then
                checkElementDocumentation documentation name

            else
                []

        _ ->
            []


shouldBeDocumented : What -> Context -> String -> Bool
shouldBeDocumented documentWhat context name =
    case documentWhat of
        Everything ->
            True

        OnlyExposed ->
            case context.exposedElements of
                EverythingIsExposed ->
                    True

                ExplicitList exposedElements ->
                    Set.member name exposedElements


checkModuleDocumentation : Maybe (Node String) -> Node String -> List (Error {})
checkModuleDocumentation documentation nameNode =
    case documentation of
        Just doc ->
            if isDocumentationEmpty doc then
                [ Rule.error
                    { message = "The documentation for module `" ++ Node.value nameNode ++ "` is empty"
                    , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                    }
                    (Node.range doc)
                ]

            else
                []

        Nothing ->
            [ Rule.error
                { message = "Missing documentation for module `" ++ Node.value nameNode ++ "`"
                , details = documentationErrorDetails
                }
                (Node.range nameNode)
            ]


documentationErrorDetails : List String
documentationErrorDetails =
    [ "A module documentation summarizes what a module is for, the responsibilities it has and how to use it. Providing a good module documentation will be useful for your users or colleagues."
    ]


checkElementDocumentation : Maybe (Node String) -> Node String -> List (Error {})
checkElementDocumentation documentation nameNode =
    case documentation of
        Just doc ->
            if isDocumentationEmpty doc then
                [ Rule.error
                    { message = "The documentation for `" ++ Node.value nameNode ++ "` is empty"
                    , details = [ "Empty documentation is not useful for the users. Please give explanations or examples." ]
                    }
                    (Node.range doc)
                ]

            else
                []

        Nothing ->
            [ Rule.error
                { message = "Missing documentation for `" ++ Node.value nameNode ++ "`"
                , details = [ "Documentation can help developers use this API." ]
                }
                (Node.range nameNode)
            ]


isDocumentationEmpty : Node String -> Bool
isDocumentationEmpty doc =
    doc
        |> Node.value
        |> String.dropLeft 3
        |> String.dropRight 2
        |> String.trim
        |> String.isEmpty
