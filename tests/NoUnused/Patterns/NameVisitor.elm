module NoUnused.Patterns.NameVisitor exposing (withValueVisitor)

{-| Visit each name in the module.

A "name" is a `Node ( ModuleName, String )` and represents a value or type reference. Here are some examples:

  - `Json.Encode.Value` -> `( [ "Json", "Encode" ], "Value" )`
  - `Html.Attributes.class` -> `( [ "Html", "Attributes" ], "class" )`
  - `Page` -> `( [], "Page" )`
  - `view` -> `( [], "view" )`

These can appear in many places throughout declarations and expressions, and picking them out each time is a lot of work. Instead of writing 1000 lines of code and tests each time, you can write one `nameVisitor` and plug it straight into your module schema, or separate `valueVisitor` and `typeVisitor`s.

@docs withNameVisitor, withValueVisitor, withTypeVisitor, withValueAndTypeVisitors


## Scope

This makes no attempt to resolve module names from imports, it just returns what's written in the code. It would be trivial to connect [elm-review-scope] with the name visitor if you want to do this.

[elm-review-scope]: http://github.com/jfmengels/elm-review-scope/


## Version

Version: 0.2.0

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Error)


type Visitor context
    = NameVisitor (VisitorFunction context)
    | ValueVisitor (VisitorFunction context)
    | TypeVisitor (VisitorFunction context)
    | ValueAndTypeVisitor (VisitorFunction context) (VisitorFunction context)


type alias VisitorFunction context =
    Node ( ModuleName, String ) -> context -> ( List (Error {}), context )


type Name
    = Value (Node ( ModuleName, String ))
    | Type (Node ( ModuleName, String ))


{-| This will apply the `nameVisitor` to every value and type in the module, you will get no information about whether the name is a value or type.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
            |> NameVisitor.withNameVisitor nameVisitor
            |> Rule.fromModuleRuleSchema

    nameVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    nameVisitor node context =
        -- Do what you want with the name
        ( [], context )

-}
withNameVisitor :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> Rule.ModuleRuleSchema state context
    -> Rule.ModuleRuleSchema { state | hasAtLeastOneVisitor : () } context
withNameVisitor nameVisitor rule =
    let
        visitor =
            NameVisitor nameVisitor
    in
    rule
        |> Rule.withDeclarationListVisitor (declarationListVisitor visitor)
        |> Rule.withExpressionEnterVisitor (expressionVisitor visitor)


{-| This will apply the `valueVisitor` to every value in the module, and ignore any types.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
            |> NameVisitor.withValueVisitor valueVisitor
            |> Rule.fromModuleRuleSchema

    valueVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    valueVisitor node context =
        -- Do what you want with the value
        ( [], context )

-}
withValueVisitor :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> Rule.ModuleRuleSchema state context
    -> Rule.ModuleRuleSchema { state | hasAtLeastOneVisitor : () } context
withValueVisitor valueVisitor rule =
    let
        visitor =
            ValueVisitor valueVisitor
    in
    rule
        |> Rule.withDeclarationListVisitor (declarationListVisitor visitor)
        |> Rule.withExpressionEnterVisitor (expressionVisitor visitor)


{-| This will apply the `typeVisitor` to every type in the module, and ignore any values.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
            |> NameVisitor.withTypeVisitor typeVisitor
            |> Rule.fromModuleRuleSchema

    typeVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    typeVisitor node context =
        -- Do what you want with the type
        ( [], context )

-}
withTypeVisitor :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> Rule.ModuleRuleSchema state context
    -> Rule.ModuleRuleSchema { state | hasAtLeastOneVisitor : () } context
withTypeVisitor typeVisitor rule =
    let
        visitor =
            TypeVisitor typeVisitor
    in
    rule
        |> Rule.withDeclarationListVisitor (declarationListVisitor visitor)
        |> Rule.withExpressionEnterVisitor (expressionVisitor visitor)


{-| This will apply the `valueVisitor` to every value and the `typeVisitor` to every type in the module.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
            |> NameVisitor.withValueAndTypeVisitors
                { valueVisitor = valueVisitor
                , typeVisitor = typeVisitor
                }
            |> Rule.fromModuleRuleSchema

    valueVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    valueVisitor node context =
        -- Do what you want with the value
        ( [], context )

    typeVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    typeVisitor node context =
        -- Do what you want with the type
        ( [], context )

-}
withValueAndTypeVisitors :
    { valueVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    , typeVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    }
    -> Rule.ModuleRuleSchema state context
    -> Rule.ModuleRuleSchema { state | hasAtLeastOneVisitor : () } context
withValueAndTypeVisitors { valueVisitor, typeVisitor } rule =
    let
        visitor =
            ValueAndTypeVisitor valueVisitor typeVisitor
    in
    rule
        |> Rule.withDeclarationListVisitor (declarationListVisitor visitor)
        |> Rule.withExpressionEnterVisitor (expressionVisitor visitor)



--- VISITORS


declarationListVisitor :
    Visitor context
    -> (List (Node Declaration) -> context -> ( List (Error {}), context ))
declarationListVisitor visitor list context =
    visitDeclarationList list
        |> folder visitor context


expressionVisitor : Visitor context -> (Node Expression -> context -> ( List (Error {}), context ))
expressionVisitor visitor node context =
    visitExpression node
        |> folder visitor context



--- FOLDER


folder :
    Visitor context
    -> context
    -> List Name
    -> ( List (Error {}), context )
folder visitor context list =
    List.foldl (folderHelper visitor) ( [], context ) list


folderHelper :
    Visitor context
    -> Name
    -> ( List (Error {}), context )
    -> ( List (Error {}), context )
folderHelper visitor name ( errors, context ) =
    let
        ( newErrors, newContext ) =
            applyVisitor visitor name context
    in
    ( newErrors ++ errors, newContext )


applyVisitor : Visitor context -> Name -> context -> ( List (Error {}), context )
applyVisitor visitor name context =
    case name of
        Value node ->
            applyValueVisitor visitor node context

        Type node ->
            applyTypeVisitor visitor node context


applyValueVisitor : Visitor context -> VisitorFunction context
applyValueVisitor visitor =
    case visitor of
        NameVisitor function ->
            function

        ValueVisitor function ->
            function

        TypeVisitor _ ->
            noopVisitor

        ValueAndTypeVisitor function _ ->
            function


applyTypeVisitor : Visitor context -> VisitorFunction context
applyTypeVisitor visitor =
    case visitor of
        NameVisitor function ->
            function

        ValueVisitor _ ->
            noopVisitor

        TypeVisitor function ->
            function

        ValueAndTypeVisitor _ function ->
            function


noopVisitor : VisitorFunction context
noopVisitor _ context =
    ( [], context )



--- PRIVATE


visitDeclarationList : List (Node Declaration) -> List Name
visitDeclarationList nodes =
    fastConcatMap visitDeclaration nodes


visitDeclaration : Node Declaration -> List Name
visitDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            visitMaybeSignature signature
                ++ visitFunctionImplementation declaration

        Declaration.AliasDeclaration { typeAnnotation } ->
            visitTypeAnnotation typeAnnotation

        Declaration.CustomTypeDeclaration { constructors } ->
            visitValueConstructorList constructors

        Declaration.PortDeclaration { typeAnnotation } ->
            visitTypeAnnotation typeAnnotation

        _ ->
            []


visitMaybeSignature : Maybe (Node Signature) -> List Name
visitMaybeSignature maybeNode =
    case maybeNode of
        Just node ->
            visitSignature node

        Nothing ->
            []


visitSignature : Node Signature -> List Name
visitSignature node =
    visitTypeAnnotation (node |> Node.value |> .typeAnnotation)


visitFunctionImplementation : Node Expression.FunctionImplementation -> List Name
visitFunctionImplementation node =
    visitPatternList (node |> Node.value |> .arguments)


visitValueConstructorList : List (Node Type.ValueConstructor) -> List Name
visitValueConstructorList list =
    fastConcatMap visitValueConstructor list


visitValueConstructor : Node Type.ValueConstructor -> List Name
visitValueConstructor node =
    visitTypeAnnotationList (node |> Node.value |> .arguments)


visitTypeAnnotationList : List (Node TypeAnnotation) -> List Name
visitTypeAnnotationList list =
    fastConcatMap visitTypeAnnotation list


visitTypeAnnotation : Node TypeAnnotation -> List Name
visitTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.GenericType _ ->
            []

        TypeAnnotation.Typed call types ->
            visitType call
                ++ visitTypeAnnotationList types

        TypeAnnotation.Unit ->
            []

        TypeAnnotation.Tupled list ->
            visitTypeAnnotationList list

        TypeAnnotation.Record list ->
            visitRecordFieldList list

        TypeAnnotation.GenericRecord _ list ->
            visitRecordFieldList (Node.value list)

        TypeAnnotation.FunctionTypeAnnotation argument return ->
            visitTypeAnnotation argument
                ++ visitTypeAnnotation return


visitRecordFieldList : List (Node TypeAnnotation.RecordField) -> List Name
visitRecordFieldList list =
    fastConcatMap visitRecordField list


visitRecordField : Node TypeAnnotation.RecordField -> List Name
visitRecordField node =
    visitTypeAnnotation (node |> Node.value |> Tuple.second)


visitExpression : Node Expression -> List Name
visitExpression (Node range expression) =
    case expression of
        Expression.FunctionOrValue moduleName function ->
            visitValue (Node range ( moduleName, function ))

        Expression.LetExpression { declarations } ->
            visitLetDeclarationList declarations

        Expression.CaseExpression { cases } ->
            visitCaseList cases

        Expression.LambdaExpression { args } ->
            visitPatternList args

        Expression.RecordUpdateExpression name _ ->
            visitValue (Node.map (\function -> ( [], function )) name)

        _ ->
            []


visitLetDeclarationList : List (Node Expression.LetDeclaration) -> List Name
visitLetDeclarationList list =
    fastConcatMap visitLetDeclaration list


visitLetDeclaration : Node Expression.LetDeclaration -> List Name
visitLetDeclaration node =
    case Node.value node of
        Expression.LetFunction { signature, declaration } ->
            visitMaybeSignature signature
                ++ visitFunctionImplementation declaration

        Expression.LetDestructuring pattern _ ->
            visitPattern pattern


visitCaseList : List Expression.Case -> List Name
visitCaseList list =
    fastConcatMap visitCase list


visitCase : Expression.Case -> List Name
visitCase ( pattern, _ ) =
    visitPattern pattern


visitPatternList : List (Node Pattern) -> List Name
visitPatternList list =
    fastConcatMap visitPattern list


visitPattern : Node Pattern -> List Name
visitPattern node =
    case Node.value node of
        Pattern.TuplePattern patterns ->
            visitPatternList patterns

        Pattern.UnConsPattern head rest ->
            visitPattern head ++ visitPattern rest

        Pattern.ListPattern list ->
            visitPatternList list

        Pattern.NamedPattern { moduleName, name } _ ->
            let
                { start } =
                    Node.range node

                newEnd =
                    { start | column = start.column + (name :: moduleName |> String.join "." |> String.length) }

                range =
                    { start = start, end = newEnd }
            in
            visitValue (Node range ( moduleName, name ))

        Pattern.AsPattern pattern _ ->
            visitPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            visitPattern pattern

        _ ->
            []


visitValue : Node ( ModuleName, String ) -> List Name
visitValue node =
    [ Value node ]


visitType : Node ( ModuleName, String ) -> List Name
visitType node =
    [ Type node ]



--- High Performance List


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn =
    List.foldr (fn >> (++)) []
