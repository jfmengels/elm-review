module Elm.TypeInference.Infer exposing
    ( Ctx
    , topLevelMember
    , unifyConfigForGroup
    )

{-| A traversal over elm-syntax AST

  - Every node gets a fresh type ID.
  - Each walk function returns that ID along with the type equations it generated.
  - Parents can refer to children's types directly by inspecting the return value of the recursive call.

Top-level and `let`-bound functions need to be gathered into binding groups and
solved together for mutual recursion and let-polymorphism.
See `topLevelMember` (entry point) and `letFunctionMember` (not exposed).

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Expression.Extra exposing (functionName, referencedNames)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Pattern.Extra
import Elm.Syntax.Signature exposing (Signature)
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.ExternalTypes as ExternalTypes
import Elm.TypeInference.Type.Internal as TypeI
    exposing
        ( Id
        , MonoType(..)
        , Type
        , TypeResolver
        )
import Elm.TypeInference.TypeEquation as TypeEquation exposing (Equations, TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (TypeAlias)
import Regex exposing (Regex)


type alias Ctx =
    { modules : Dict ModuleId ModuleIndex
    , thisModule : ModuleIndex
    , typeAliases : Dict ( ModuleId, PackageName, VarName ) TypeAlias
    , index : ModuleLookup.Index
    , allowKernel : Bool
    , moduleMapping : ModuleIds.Mapping
    }


unifyConfig : Ctx -> Unify.UnifyConfig
unifyConfig ctx =
    unifyConfigForGroup ctx []


unifyConfigForGroup : Ctx -> List VarName -> Unify.UnifyConfig
unifyConfigForGroup ctx declarationNames =
    { typeAliases = ctx.typeAliases
    , moduleName = ctx.thisModule.moduleName
    , declarationNames = declarationNames
    , moduleMapping = ctx.moduleMapping
    }


typeResolver : Ctx -> TypeResolver
typeResolver ctx =
    ModuleLookup.typeResolverFor ctx.moduleMapping ctx.index ctx.modules ctx.thisModule


{-| Wrap details with the current module's location.
Binding-group type errors get their group via `UnifyConfig`;
everything else carries the module with an empty group.
-}
toError : Ctx -> ErrorDetails -> Error
toError ctx details =
    { moduleName = FullModuleName.toModuleName ctx.thisModule.moduleName
    , declarationNames = []
    , details = details
    }


type alias Inferred =
    ( Id, Equations )


inferMany : (a -> StateM Inferred) -> List a -> StateM ( List Id, Equations )
inferMany f items =
    State.traverse f items
        |> State.map
            (\inferred ->
                inferred
                    |> List.foldr
                        (\( id_, eqs ) ( ids, allEqs ) ->
                            ( id_ :: ids
                            , TypeEquation.append eqs allEqs
                            )
                        )
                        ( [], TypeEquation.empty )
            )


{-| `arg1 -> arg2 -> ... -> result`
-}
functionType : List Id -> Id -> MonoType
functionType argIds resultId =
    argIds
        |> List.foldr
            (\argId toType -> Function { from = TypeI.id_ argId, to = toType })
            (TypeI.id_ resultId)


{-| Resolves an already-located global name to its type. Follows chains.
-}
resolveGlobalVar : Ctx -> PackageName -> ModuleId -> VarName -> StateM MonoType
resolveGlobalVar ctx package moduleId name =
    let
        ( aliasedPackage, aliasedModuleId, aliasedName ) =
            if package == "" then
                case
                    ModuleLookup.resolveOperatorFunction ctx.moduleMapping ctx.modules moduleId name
                        |> Result.withDefault Nothing
                of
                    Just ( m, n ) ->
                        ( "", m, n )

                    Nothing ->
                        ( package, moduleId, name )

            else
                ( package, moduleId, name )
    in
    State.lookupGlobalEnv ctx.moduleMapping aliasedPackage aliasedModuleId aliasedName


{-| Resolves a value or operator symbol to its type.
-}
lookupVarOrOperator : Ctx -> Maybe FullModuleName -> VarName -> StateM MonoType
lookupVarOrOperator ctx maybeModuleName name =
    State.do (ModuleLookup.findModuleOfVar ctx.moduleMapping ctx.index ctx.modules ctx.thisModule maybeModuleName name) <|
        \( package, moduleId ) ->
            resolveGlobalVar ctx package moduleId name


isKernelModule : FullModuleName -> Bool
isKernelModule full =
    case FullModuleName.toModuleName full of
        "Elm" :: "Kernel" :: _ ->
            True

        _ ->
            False


isKernelModuleId : Ctx -> ModuleId -> Bool
isKernelModuleId ctx moduleId =
    case ctx.moduleMapping |> ModuleIds.getName moduleId of
        Just full ->
            isKernelModule full

        Nothing ->
            False


isKernelQualifier : Ctx -> Maybe FullModuleName -> Bool
isKernelQualifier ctx maybeQualifier =
    case maybeQualifier of
        Nothing ->
            False

        Just qualifier ->
            if isKernelModule qualifier then
                True

            else
                case qualifier of
                    ( single, [] ) ->
                        ModuleIndex.modulesWithAlias ctx.thisModule single
                            |> List.any (\mod -> isKernelModuleId ctx mod)

                    _ ->
                        False


isExposedKernelValue : Ctx -> VarName -> Bool
isExposedKernelValue ctx varName =
    ctx.thisModule.imports
        |> List.any
            (\import_ ->
                isKernelModule import_.moduleName
                    && ModuleIndex.importCouldExposeValue import_ varName
            )


isKernelVar : Ctx -> Maybe FullModuleName -> VarName -> Bool
isKernelVar ctx maybeQualifier varName =
    if not ctx.allowKernel then
        False

    else
        case maybeQualifier of
            Nothing ->
                isExposedKernelValue ctx varName

            Just _ ->
                isKernelQualifier ctx maybeQualifier



-- BINDING GROUP MEMBERS


{-| The `FunctionImplementation` node (and the name node inside it) want the same
type as the declaration they belong to -- and in the case of the implementation,
elm-syntax even gives it the very same range when there's no documentation and no
signature.
-}
aliasImplementation :
    Id
    -> Node Expression.FunctionImplementation
    -> StateM Expression.FunctionImplementation
aliasImplementation declId implNode =
    let
        impl : Expression.FunctionImplementation
        impl =
            Node.value implNode
    in
    State.do (State.aliasNodeId (Node.range implNode) declId) <|
        \() ->
            State.do (State.aliasNodeId (Node.range impl.name) declId) <|
                \() ->
                    State.pure impl


aliasSignature : Id -> Maybe (Node Signature) -> StateM ()
aliasSignature declId maybeSigNode =
    case maybeSigNode of
        Nothing ->
            State.pureUnit

        Just sigNode ->
            State.do (State.aliasNodeId (Node.range sigNode) declId) <|
                \() ->
                    State.aliasNodeId (Node.range (Node.value sigNode).name) declId


inferFnImplementation : Ctx -> Id -> Expression.FunctionImplementation -> StateM Equations
inferFnImplementation ctx declId impl =
    State.withScopedEnv <|
        (State.do (inferMany (\arg -> inferPattern ctx arg) impl.arguments) <|
            \( argIds, argEqs ) ->
                State.do (inferExpr ctx impl.expression) <|
                    \( bodyId, bodyEqs ) ->
                        State.pure <|
                            TypeEquation.cons
                                ( TypeI.id_ declId
                                , functionType argIds bodyId
                                , "Binding: from its args to its body"
                                )
                                (TypeEquation.append argEqs bodyEqs)
        )


annotationType : Ctx -> Maybe (Node Signature) -> StateM (Maybe MonoType)
annotationType ctx maybeSigNode =
    case maybeSigNode of
        Nothing ->
            State.pure Nothing

        Just sigNode ->
            case
                Node.value sigNode
                    |> .typeAnnotation
                    |> Node.value
                    |> TypeI.fromTypeAnnotation (typeResolver ctx)
            of
                Err fromTypeAnnotationError ->
                    State.error (toError ctx (TypeI.fromTypeAnnotationError fromTypeAnnotationError))

                Ok t ->
                    State.pure (Just t)


{-| `declId ≡ annotationType`, if the function is annotated.

The annotation's type variables are freshly instantiated before use: two
unrelated declarations can both write eg. `a` in their signature, and those
`a`s must not be the same substitution-map key -- otherwise unifying one
declaration's body against its own annotation would leak a binding for `a`
that then corrupts every other same-named-variable signature processed
afterwards (see the `e2e/tests/histogram-force-comparable` regression fixture).

-}
signatureEquations : Id -> Maybe MonoType -> StateM Equations
signatureEquations declId maybeAnnotationType =
    case maybeAnnotationType of
        Nothing ->
            State.pure TypeEquation.empty

        Just annotationType_ ->
            State.do (State.instantiate (TypeI.closeOver annotationType_)) <|
                \freshAnnotationType ->
                    State.pure
                        (TypeEquation.single
                            ( TypeI.id_ declId
                            , freshAnnotationType
                            , "Binding must be consistent with its annotation"
                            )
                        )


{-| Shared body of `topLevelMember` and `letFunctionMember`.
Only the install step differs: `globalEnv` vs lexical env.
-}
functionMember :
    Ctx
    -> Node a
    -> Expression.Function
    -> (VarName -> Type -> StateM ())
    -> StateM BindingGroup.Member
functionMember ctx declNode fn installFor =
    State.do (State.idForNode declNode) <|
        \declId ->
            State.do (aliasImplementation declId fn.declaration) <|
                \impl ->
                    State.do (aliasSignature declId fn.signature) <|
                        \() ->
                            State.do (annotationType ctx fn.signature) <|
                                \maybeAnnotationType ->
                                    let
                                        varName : VarName
                                        varName =
                                            Node.value impl.name
                                    in
                                    State.pure
                                        { id = declId
                                        , annotation = Maybe.map TypeI.closeOver maybeAnnotationType
                                        , install = installFor varName
                                        , equations =
                                            State.map2
                                                (\sigEquations implEquations ->
                                                    TypeEquation.toList (TypeEquation.append sigEquations implEquations)
                                                )
                                                (signatureEquations declId maybeAnnotationType)
                                                (inferFnImplementation ctx declId impl)
                                        }


{-| Top-level function declaration. Adds a binding to `globalEnv`.
-}
topLevelMember : Ctx -> Node Declaration -> Expression.Function -> StateM BindingGroup.Member
topLevelMember ctx declNode fn =
    functionMember
        ctx
        declNode
        fn
        (\varName varType ->
            State.addGlobalBinding ( ctx.thisModule.moduleId, "", varName ) varType
        )


{-| A `let..in` function declaration. Adds a binding to lexical `lexicalEnv`
-}
letFunctionMember : Ctx -> Node LetDeclaration -> Expression.Function -> StateM BindingGroup.Member
letFunctionMember ctx declNode fn =
    functionMember
        ctx
        declNode
        fn
        State.addBinding



-- EXPRESSIONS


inferExpr : Ctx -> Node Expression -> StateM Inferred
inferExpr ctx exprNode =
    State.do (State.idForNode exprNode) <|
        \exprId ->
            let
                type_ : MonoType
                type_ =
                    TypeI.id_ exprId

                finish : List TypeEquation -> StateM Inferred
                finish eqs =
                    State.pure ( exprId, TypeEquation.batch eqs )

                finishEqns : Equations -> StateM Inferred
                finishEqns eqs =
                    State.pure ( exprId, eqs )
            in
            case Node.value exprNode of
                UnitExpr ->
                    finish [ ( type_, Unit, "Unit" ) ]

                Application application ->
                    case application of
                        [] ->
                            stateErrorImpossibleExpr ctx exprNode

                        fnNode :: argNodes ->
                            State.do State.getNextIdAndTick <|
                                \resultId ->
                                    State.do (inferExpr ctx fnNode) <|
                                        \( fnId, fnEqs ) ->
                                            State.do (inferMany (\arg -> inferExpr ctx arg) argNodes) <|
                                                \( argIds, argEqs ) ->
                                                    finishEqns <|
                                                        TypeEquation.append fnEqs
                                                            (TypeEquation.append argEqs
                                                                (TypeEquation.batch
                                                                    [ ( type_, TypeI.id_ resultId, "Application = its result" )
                                                                    , ( TypeI.id_ fnId
                                                                      , functionType argIds resultId
                                                                      , "Application: first is fn"
                                                                      )
                                                                    ]
                                                                )
                                                            )

                OperatorApplication operator _ e1 e2 ->
                    State.do State.getNextIdAndTick <|
                        \resultId ->
                            State.do (inferExpr ctx e1) <|
                                \( e1Id, e1Eqs ) ->
                                    State.do (inferExpr ctx e2) <|
                                        \( e2Id, e2Eqs ) ->
                                            State.do (lookupVarOrOperator ctx Nothing operator) <|
                                                \operatorType ->
                                                    finishEqns <|
                                                        TypeEquation.append e1Eqs
                                                            (TypeEquation.append e2Eqs
                                                                (TypeEquation.batch
                                                                    [ ( type_, TypeI.id_ resultId, "Op application = its result" )
                                                                    , ( operatorType, functionType [ e1Id, e2Id ] resultId, "Op application: is a fn" )
                                                                    ]
                                                                )
                                                            )

                FunctionOrValue moduleName varName ->
                    -- Lexically bound name wins over imported one
                    State.do
                        (if List.isEmpty moduleName then
                            State.existsInEnv varName

                         else
                            State.pure False
                        )
                    <|
                        \isLexical ->
                            if isLexical then
                                State.do (State.lookupEnv ctx.thisModule.moduleName varName) <|
                                    \varType ->
                                        finish [ ( type_, varType, "FunctionOrValue: var from env" ) ]

                            else
                                case
                                    ModuleLookup.moduleOfVar
                                        ctx.moduleMapping
                                        ctx.index
                                        ctx.modules
                                        ctx.thisModule
                                        (FullModuleName.fromModuleName moduleName)
                                        varName
                                of
                                    Ok (Just ( package, moduleId )) ->
                                        State.do (resolveGlobalVar ctx package moduleId varName) <|
                                            \varType ->
                                                finish [ ( type_, varType, "FunctionOrValue: global/top-level var" ) ]

                                    Ok Nothing ->
                                        if isKernelVar ctx (FullModuleName.fromModuleName moduleName) varName then
                                            -- Kernel functions are like Debug.todo: "trust me bro"
                                            finish []

                                        else
                                            State.do (State.lookupEnv ctx.thisModule.moduleName varName) <|
                                                \varType ->
                                                    finish [ ( type_, varType, "FunctionOrValue: var from env" ) ]

                                    Err details ->
                                        State.error (toError ctx details)

                IfBlock e1 e2 e3 ->
                    State.do (inferExpr ctx e1) <|
                        \( id1, eqs1 ) ->
                            State.do (inferExpr ctx e2) <|
                                \( id2, eqs2 ) ->
                                    State.do (inferExpr ctx e3) <|
                                        \( id3, eqs3 ) ->
                                            finishEqns <|
                                                TypeEquation.append eqs1
                                                    (TypeEquation.append eqs2
                                                        (TypeEquation.append eqs3
                                                            (TypeEquation.batch
                                                                [ ( TypeI.id_ id1, Bool, "If: condition = bool" )
                                                                , ( TypeI.id_ id2, TypeI.id_ id3, "If: then = else" )
                                                                , ( TypeI.id_ id2, type_, "If: then = result" )
                                                                ]
                                                            )
                                                        )
                                                    )

                PrefixOperator operator ->
                    State.do (lookupVarOrOperator ctx Nothing operator) <|
                        \operatorType ->
                            finish [ ( type_, operatorType, "Prefix operator: is a fn" ) ]

                Operator _ ->
                    stateErrorImpossibleExpr ctx exprNode

                Integer _ ->
                    State.do State.getNextIdAndTick <|
                        \numberId ->
                            finish [ ( type_, TypeI.number_ numberId, "Int" ) ]

                Hex _ ->
                    State.do State.getNextIdAndTick <|
                        \numberId ->
                            finish [ ( type_, TypeI.number_ numberId, "Hex" ) ]

                Floatable _ ->
                    finish [ ( type_, Float, "Float" ) ]

                Negation e1 ->
                    State.do State.getNextIdAndTick <|
                        \numberId ->
                            State.do (inferExpr ctx e1) <|
                                \( id1, eqs1 ) ->
                                    finishEqns <|
                                        TypeEquation.cons
                                            ( type_, TypeI.id_ id1, "Negation = inner" )
                                            (TypeEquation.cons
                                                ( type_, TypeI.number_ numberId, "Negation = number" )
                                                eqs1
                                            )

                Literal _ ->
                    finish [ ( type_, String, "String" ) ]

                CharLiteral _ ->
                    finish [ ( type_, Char, "Char" ) ]

                TupledExpression exprNodes ->
                    State.do (inferMany (\part -> inferExpr ctx part) exprNodes) <|
                        \( ids, eqs ) ->
                            case ids of
                                [ id1, id2 ] ->
                                    finishEqns <|
                                        TypeEquation.append eqs
                                            (TypeEquation.single
                                                ( type_
                                                , Tuple2 (TypeI.id_ id1) (TypeI.id_ id2)
                                                , "Tuple: top"
                                                )
                                            )

                                [ id1, id2, id3 ] ->
                                    finishEqns <|
                                        TypeEquation.append eqs
                                            (TypeEquation.single
                                                ( type_
                                                , Tuple3 (TypeI.id_ id1) (TypeI.id_ id2) (TypeI.id_ id3)
                                                , "Tuple3: top"
                                                )
                                            )

                                _ ->
                                    stateErrorImpossibleExpr ctx exprNode

                ParenthesizedExpression e1 ->
                    State.do (inferExpr ctx e1) <|
                        \( id1, eqs1 ) ->
                            finishEqns <| TypeEquation.cons ( type_, TypeI.id_ id1, "Parenthesized = inner" ) eqs1

                LetExpression { declarations, expression } ->
                    State.withScopedEnv <|
                        (State.do (solveLetDeclarations ctx declarations) <|
                            \() ->
                                State.do (inferExpr ctx expression) <|
                                    \( bodyId, bodyEqs ) ->
                                        finishEqns <|
                                            TypeEquation.cons
                                                ( type_, TypeI.id_ bodyId, "Let = its body" )
                                                bodyEqs
                        )

                CaseExpression { expression, cases } ->
                    State.do (inferExpr ctx expression) <|
                        \( scrutineeId, scrutineeEqs ) ->
                            State.do
                                (State.traverse
                                    (\( patternNode, bodyNode ) ->
                                        State.withScopedEnv <|
                                            (State.do (inferPattern ctx patternNode) <|
                                                \( patternId, patternEqs ) ->
                                                    State.do (inferExpr ctx bodyNode) <|
                                                        \( bodyId, bodyEqs ) ->
                                                            State.pure ( ( patternId, bodyId ), TypeEquation.append patternEqs bodyEqs )
                                            )
                                    )
                                    cases
                                )
                            <|
                                \caseInferreds ->
                                    let
                                        caseEqs : Equations
                                        caseEqs =
                                            List.foldr
                                                (\( _, branchEqs ) acc -> TypeEquation.append branchEqs acc)
                                                TypeEquation.empty
                                                caseInferreds

                                        ( scrutineeEquations, bodyEquations ) =
                                            List.foldr
                                                (\( ( patternId, bodyId ), _ ) ( scruts, bodies ) ->
                                                    ( ( TypeI.id_ scrutineeId
                                                      , TypeI.id_ patternId
                                                      , "Case: scrutinee = branch pattern"
                                                      )
                                                        :: scruts
                                                    , ( type_
                                                      , TypeI.id_ bodyId
                                                      , "Case: result = branch body"
                                                      )
                                                        :: bodies
                                                    )
                                                )
                                                ( [], [] )
                                                caseInferreds
                                    in
                                    finishEqns <|
                                        TypeEquation.append scrutineeEqs
                                            (TypeEquation.append caseEqs
                                                (TypeEquation.append
                                                    (TypeEquation.batch scrutineeEquations)
                                                    (TypeEquation.batch bodyEquations)
                                                )
                                            )

                LambdaExpression { args, expression } ->
                    State.withScopedEnv <|
                        (State.do (inferMany (\arg -> inferPattern ctx arg) args) <|
                            \( argIds, argEqs ) ->
                                State.do (inferExpr ctx expression) <|
                                    \( bodyId, bodyEqs ) ->
                                        finishEqns <|
                                            TypeEquation.append argEqs
                                                (TypeEquation.append bodyEqs
                                                    (TypeEquation.single
                                                        ( type_
                                                        , functionType argIds bodyId
                                                        , "Lambda: is a function"
                                                        )
                                                    )
                                                )
                        )

                RecordExpr fieldSetters ->
                    State.do (inferRecordSetters ctx fieldSetters) <|
                        \( fields, eqs ) ->
                            finishEqns <|
                                TypeEquation.append eqs
                                    (TypeEquation.single
                                        ( type_
                                        , Record { fields = fields }
                                        , "Record: is a record"
                                        )
                                    )

                ListExpr exprNodes ->
                    State.do (inferMany (\el -> inferExpr ctx el) exprNodes) <|
                        \( ids, eqs ) ->
                            State.do State.getNextIdAndTick <|
                                \listItemId ->
                                    finishEqns <|
                                        TypeEquation.append eqs
                                            (TypeEquation.append
                                                (TypeEquation.batch
                                                    (List.map
                                                        (\itemId ->
                                                            ( TypeI.id_ itemId
                                                            , TypeI.id_ listItemId
                                                            , "List: pin list type param to all inner"
                                                            )
                                                        )
                                                        ids
                                                    )
                                                )
                                                (TypeEquation.single
                                                    ( type_, List <| TypeI.id_ listItemId, "List: is a list" )
                                                )
                                            )

                RecordAccess recordNode fieldNameNode ->
                    State.do (inferExpr ctx recordNode) <|
                        \( recordNodeId, recordEqs ) ->
                            State.do State.getNextIdAndTick <|
                                \extensibleRecordId ->
                                    State.do State.getNextIdAndTick <|
                                        \resultId ->
                                            -- The field-name node has the field's type, which is the type of
                                            -- the whole access expression.
                                            State.do (State.aliasNodeId (Node.range fieldNameNode) resultId) <|
                                                \() ->
                                                    finishEqns <|
                                                        TypeEquation.append
                                                            (TypeEquation.batch
                                                                [ ( type_, TypeI.id_ resultId, "Record access = the field = the result" )
                                                                , ( TypeI.id_ recordNodeId
                                                                  , ExtensibleRecord
                                                                        { extensionTypevar = TypeI.id_ extensibleRecordId
                                                                        , fields =
                                                                            Dict.singleton
                                                                                (Node.value fieldNameNode)
                                                                                (TypeI.id_ resultId)
                                                                        }
                                                                  , "Record access: left is a record"
                                                                  )
                                                                ]
                                                            )
                                                            recordEqs

                RecordAccessFunction fieldName ->
                    State.do State.getNextIdAndTick <|
                        \recordId ->
                            State.do State.getNextIdAndTick <|
                                \resultId ->
                                    finish
                                        [ ( type_
                                          , Function
                                                { from =
                                                    ExtensibleRecord
                                                        { extensionTypevar = TypeI.id_ recordId
                                                        , fields =
                                                            -- the fieldName is ".a", not "a", so let's sanitize that
                                                            Dict.singleton (String.dropLeft 1 fieldName) (TypeI.id_ resultId)
                                                        }
                                                , to = TypeI.id_ resultId
                                                }
                                          , "Record access fn: is a function"
                                          )
                                        ]

                RecordUpdateExpression recordVarNode fieldSetters ->
                    let
                        recordVar : VarName
                        recordVar =
                            Node.value recordVarNode
                    in
                    State.do State.getNextIdAndTick <|
                        \recordId ->
                            State.do (State.idForNode recordVarNode) <|
                                \recordVarNodeId ->
                                    State.do (State.existsInEnv recordVar) <|
                                        \isLexical ->
                                            State.do
                                                (if isLexical then
                                                    State.lookupEnv ctx.thisModule.moduleName recordVar

                                                 else
                                                    lookupVarOrOperator ctx Nothing recordVar
                                                )
                                            <|
                                                \recordVarType ->
                                                    State.do (inferRecordSetters ctx fieldSetters) <|
                                                        \( fields, eqs ) ->
                                                            let
                                                                asExtensibleRecord : MonoType
                                                                asExtensibleRecord =
                                                                    ExtensibleRecord
                                                                        { extensionTypevar = TypeI.id_ recordId
                                                                        , fields = fields
                                                                        }
                                                            in
                                                            finishEqns <|
                                                                TypeEquation.append eqs
                                                                    (TypeEquation.batch
                                                                        [ ( recordVarType, asExtensibleRecord, "Record update: base record has at least that field" )
                                                                        , ( type_, asExtensibleRecord, "Record update: result has the same shape as the base record" )
                                                                        , ( TypeI.id_ recordVarNodeId, recordVarType, "Record update: base variable node" )
                                                                        ]
                                                                    )

                GLSLExpression code ->
                    let
                        declarations :
                            { uniforms : Dict VarName MonoType
                            , attributes : Dict VarName MonoType
                            , varyings : Dict VarName MonoType
                            }
                        declarations =
                            glslDeclarations code
                    in
                    State.do State.getNextIdAndTick <|
                        \attributesId ->
                            State.do State.getNextIdAndTick <|
                                \uniformsId ->
                                    State.do State.getNextIdAndTick <|
                                        \varyingsId ->
                                            finish
                                                [ ( type_
                                                  , WebGLShader
                                                        { attributesExtension = TypeI.id_ attributesId
                                                        , attributes = declarations.attributes
                                                        , uniformsExtension = TypeI.id_ uniformsId
                                                        , uniforms = declarations.uniforms
                                                        , varyingsExtension = TypeI.id_ varyingsId
                                                        , varyings = declarations.varyings
                                                        }
                                                  , "GLSLExpression: is a shader"
                                                  )
                                                ]


stateErrorImpossibleExpr : Ctx -> Node Expression -> StateM Inferred
stateErrorImpossibleExpr ctx exprNode =
    State.error (toError ctx (ImpossibleExpr exprNode))


inferRecordSetters :
    Ctx
    -> List (Node Expression.RecordSetter)
    -> StateM ( Dict VarName MonoType, Equations )
inferRecordSetters ctx fieldSetters =
    fieldSetters
        |> State.foldl
            (\fieldSetterNode ( fields, allEqs ) ->
                let
                    ( fieldNameNode, fieldExprNode ) =
                        Node.value fieldSetterNode
                in
                State.do (inferExpr ctx fieldExprNode) <|
                    \( fieldId, eqs ) ->
                        State.do (State.aliasNodeId (Node.range fieldNameNode) fieldId) <|
                            \() ->
                                State.pure
                                    ( Dict.insert (Node.value fieldNameNode) (TypeI.id_ fieldId) fields
                                    , TypeEquation.append allEqs eqs
                                    )
            )
            ( Dict.empty, TypeEquation.empty )



-- LET DECLARATIONS


{-| Solve decls in `let..in` in dependency order.
`let` functions and `let` destructurings are available at the same time.

Annotated functions are installed before the non-annotated ones' cycle.

-}
solveLetDeclarations : Ctx -> List (Node LetDeclaration) -> StateM ()
solveLetDeclarations ctx declarations =
    let
        indexed : List ( Int, Node LetDeclaration )
        indexed =
            List.indexedMap Tuple.pair declarations

        byIndex : Dict Int (Node LetDeclaration)
        byIndex =
            Dict.fromList indexed

        -- name -> index of the declaration binding it
        indexOfName : Dict VarName Int
        indexOfName =
            indexed
                |> List.foldl
                    (\( index, declNode ) accAcrossDecls ->
                        case Node.value declNode of
                            LetFunction fn ->
                                Dict.insert (functionName fn) index accAcrossDecls

                            LetDestructuring patternNode _ ->
                                Elm.Syntax.Pattern.Extra.varNames (Node.value patternNode)
                                    |> List.foldl (\name acc -> Dict.insert name index acc)
                                        accAcrossDecls
                    )
                    Dict.empty

        hasLetAnnotation : Node LetDeclaration -> Bool
        hasLetAnnotation declNode =
            case Node.value declNode of
                LetFunction fn ->
                    case fn.signature of
                        Just _ ->
                            True

                        Nothing ->
                            False

                LetDestructuring _ _ ->
                    False

        isAnnotatedIndex : Int -> Bool
        isAnnotatedIndex index =
            Dict.get index byIndex
                |> Maybe.map hasLetAnnotation
                |> Maybe.withDefault False

        bodyOf : Node LetDeclaration -> Expression
        bodyOf declNode =
            case Node.value declNode of
                LetFunction fn ->
                    Node.value (Node.value fn.declaration).expression

                LetDestructuring _ exprNode ->
                    Node.value exprNode

        edges : Int -> List Int
        edges index =
            case Dict.get index byIndex of
                Nothing ->
                    []

                Just declNode ->
                    referencedNames (bodyOf declNode)
                        |> List.filterMap
                            (\( maybeModuleName, refName ) ->
                                case maybeModuleName of
                                    Nothing ->
                                        case Dict.get refName indexOfName of
                                            Nothing ->
                                                Nothing

                                            Just target ->
                                                -- Annotated bindings have already been pre-installed.
                                                if isAnnotatedIndex target then
                                                    Nothing

                                                else
                                                    Just target

                                    Just _ ->
                                        Nothing
                            )

        sccs : List (List Int)
        sccs =
            SCC.stronglyConnectedComponents (List.map Tuple.first indexed) edges

        inferDestructuring : Node LetDeclaration -> Node Pattern -> Node Expression -> StateM ()
        inferDestructuring declNode patternNode exprNode =
            let
                boundVars : List VarName
                boundVars =
                    Elm.Syntax.Pattern.Extra.varNames (Node.value patternNode)

                inferAndUnify : StateM ()
                inferAndUnify =
                    State.do (State.idForNode declNode) <|
                        \declId ->
                            State.do (inferPattern ctx patternNode) <|
                                \( patternId, patternEqs ) ->
                                    State.do (inferExpr ctx exprNode) <|
                                        \( exprId, exprEqs ) ->
                                            let
                                                eqs : Equations
                                                eqs =
                                                    TypeEquation.cons
                                                        ( TypeI.id_ declId, TypeI.id_ patternId, "Let destructuring: alias" )
                                                        (TypeEquation.cons
                                                            ( TypeI.id_ patternId, TypeI.id_ exprId, "Let destructuring: pattern = expr" )
                                                            (TypeEquation.append patternEqs exprEqs)
                                                        )

                                                droppedEqs : List ( MonoType, MonoType )
                                                droppedEqs =
                                                    List.map (\( t1, t2, _ ) -> ( t1, t2 )) (TypeEquation.toList eqs)
                                            in
                                            Unify.unifyMany (unifyConfig ctx) droppedEqs
            in
            State.do (State.withDeeperLetRank inferAndUnify) <|
                \() ->
                    boundVars
                        |> State.traverseUnit State.generalizeBinding

        solveGroup : List Int -> StateM ()
        solveGroup groupIndices =
            let
                ( functions, destructurings ) =
                    List.foldr
                        (\declIndex ( fns, dests ) ->
                            case Dict.get declIndex byIndex of
                                Nothing ->
                                    ( fns, dests )

                                Just declNode ->
                                    case Node.value declNode of
                                        LetFunction fn ->
                                            ( ( declNode, fn ) :: fns, dests )

                                        LetDestructuring patternNode exprNode ->
                                            ( fns, ( declNode, patternNode, exprNode ) :: dests )
                        )
                        ( [], [] )
                        groupIndices
            in
            State.do
                (functions
                    |> State.traverse (\( declNode, fn ) -> letFunctionMember ctx declNode fn)
                    |> State.andThen (\members -> BindingGroup.solveGroup (unifyConfig ctx) members)
                )
            <|
                \() ->
                    destructurings
                        |> State.traverseUnit
                            (\( declNode, patternNode, exprNode ) ->
                                inferDestructuring declNode patternNode exprNode
                            )

        preinstallAnnotated : StateM ()
        preinstallAnnotated =
            declarations
                |> State.traverseUnit
                    (\declNode ->
                        case Node.value declNode of
                            LetFunction fn ->
                                case fn.signature of
                                    Nothing ->
                                        State.pureUnit

                                    Just _ ->
                                        State.do (annotationType ctx fn.signature) <|
                                            \maybeMono ->
                                                case maybeMono of
                                                    Nothing ->
                                                        State.pureUnit

                                                    Just mono ->
                                                        State.addBinding (functionName fn) (TypeI.closeOver mono)

                            LetDestructuring _ _ ->
                                State.pureUnit
                    )
    in
    State.do preinstallAnnotated <|
        \() ->
            sccs
                |> State.traverseUnit solveGroup



-- PATTERNS


inferPattern : Ctx -> Node Pattern -> StateM Inferred
inferPattern ctx patternNode =
    State.do (State.idForNode patternNode) <|
        \patternId ->
            let
                type_ : MonoType
                type_ =
                    TypeI.id_ patternId

                finish : List TypeEquation -> StateM Inferred
                finish eqs =
                    State.pure ( patternId, TypeEquation.batch eqs )

                finishEqns : Equations -> StateM Inferred
                finishEqns eqs =
                    State.pure ( patternId, eqs )
            in
            case Node.value patternNode of
                AllPattern ->
                    finish []

                UnitPattern ->
                    finish [ ( type_, Unit, "Unit pattern" ) ]

                CharPattern _ ->
                    finish [ ( type_, Char, "Char pattern" ) ]

                StringPattern _ ->
                    finish [ ( type_, String, "String pattern" ) ]

                IntPattern _ ->
                    State.do State.getNextIdAndTick <|
                        \numberId ->
                            finish [ ( type_, TypeI.number_ numberId, "Int pattern" ) ]

                HexPattern _ ->
                    State.do State.getNextIdAndTick <|
                        \numberId ->
                            finish [ ( type_, TypeI.number_ numberId, "Hex pattern" ) ]

                FloatPattern _ ->
                    finish [ ( type_, Float, "Float pattern" ) ]

                TuplePattern patterns ->
                    let
                        impossiblePattern : StateM Inferred
                        impossiblePattern =
                            State.error (toError ctx (ImpossiblePattern patternNode))
                    in
                    State.do (inferMany (\part -> inferPattern ctx part) patterns) <|
                        \( ids, eqs ) ->
                            case ids of
                                [ id1, id2 ] ->
                                    finishEqns <|
                                        TypeEquation.cons
                                            ( type_
                                            , Tuple2
                                                (TypeI.id_ id1)
                                                (TypeI.id_ id2)
                                            , "Tuple pattern: top"
                                            )
                                            eqs

                                [ id1, id2, id3 ] ->
                                    finishEqns <|
                                        TypeEquation.cons
                                            ( type_
                                            , Tuple3
                                                (TypeI.id_ id1)
                                                (TypeI.id_ id2)
                                                (TypeI.id_ id3)
                                            , "Tuple3 pattern: top"
                                            )
                                            eqs

                                _ ->
                                    impossiblePattern

                RecordPattern fields ->
                    {- If we're pattern matching some record fields, we're mandating that
                       the thing is a record that contains _at least_ these fields.

                       Which is what our ExtensibleRecord type does!
                    -}
                    State.do
                        (fields
                            |> State.foldl
                                (\fieldNode acc ->
                                    State.do (State.idForNode fieldNode) <|
                                        \fieldId ->
                                            State.do (State.addBinding (Node.value fieldNode) (TypeI.mono <| TypeI.id_ fieldId)) <|
                                                \() ->
                                                    State.pure (Dict.insert (Node.value fieldNode) (TypeI.id_ fieldId) acc)
                                )
                                Dict.empty
                        )
                    <|
                        \fields_ ->
                            State.do State.getNextIdAndTick <|
                                \recordId ->
                                    finish
                                        [ ( type_
                                          , ExtensibleRecord
                                                { extensionTypevar = TypeI.id_ recordId
                                                , fields = fields_
                                                }
                                          , "Record pattern"
                                          )
                                        ]

                UnConsPattern p1 p2 ->
                    State.do State.getNextIdAndTick <|
                        \listItemId ->
                            State.do (inferPattern ctx p1) <|
                                \( id1, eqs1 ) ->
                                    State.do (inferPattern ctx p2) <|
                                        \( id2, eqs2 ) ->
                                            finishEqns <|
                                                TypeEquation.append eqs1
                                                    (TypeEquation.append eqs2
                                                        (TypeEquation.batch
                                                            [ ( type_, List <| TypeI.id_ listItemId, "UnCons pattern: result" )
                                                            , ( type_, TypeI.id_ id2, "UnCons pattern: result same as tail" )
                                                            , ( TypeI.id_ id1, TypeI.id_ listItemId, "UnCons pattern: head pins list type param" )
                                                            ]
                                                        )
                                                    )

                ListPattern patterns ->
                    State.do State.getNextIdAndTick <|
                        \listItemId ->
                            State.do (inferMany (\el -> inferPattern ctx el) patterns) <|
                                \( ids, eqs ) ->
                                    finishEqns <|
                                        TypeEquation.append eqs
                                            (TypeEquation.append
                                                (TypeEquation.batch
                                                    (List.map
                                                        (\itemId ->
                                                            ( TypeI.id_ itemId
                                                            , TypeI.id_ listItemId
                                                            , "ListPattern: pin list type param to all items"
                                                            )
                                                        )
                                                        ids
                                                    )
                                                )
                                                (TypeEquation.single
                                                    ( type_, List <| TypeI.id_ listItemId, "ListPattern: result" )
                                                )
                                            )

                VarPattern var ->
                    State.do (State.addBinding var (TypeI.mono type_)) <|
                        \() ->
                            finish []

                NamedPattern customType args ->
                    case
                        ModuleLookup.moduleOfVar
                            ctx.moduleMapping
                            ctx.index
                            ctx.modules
                            ctx.thisModule
                            (FullModuleName.fromModuleName customType.moduleName)
                            customType.name
                    of
                        Ok (Just ( package, moduleId )) ->
                            State.do (State.lookupGlobalEnv ctx.moduleMapping package moduleId customType.name) <|
                                \ctorType ->
                                    State.do State.getNextIdAndTick <|
                                        \resultId ->
                                            State.do (inferMany (\arg -> inferPattern ctx arg) args) <|
                                                \( argIds, eqs ) ->
                                                    finishEqns <|
                                                        TypeEquation.cons
                                                            ( ctorType, functionType argIds resultId, "NamedPattern: constructor is a fn" )
                                                            (TypeEquation.cons
                                                                ( type_, TypeI.id_ resultId, "NamedPattern: result" )
                                                                eqs
                                                            )

                        Ok Nothing ->
                            if isKernelVar ctx (FullModuleName.fromModuleName customType.moduleName) customType.name then
                                State.do State.getNextIdAndTick <|
                                    \ctorId ->
                                        State.do State.getNextIdAndTick <|
                                            \resultId ->
                                                State.do (inferMany (\arg -> inferPattern ctx arg) args) <|
                                                    \( argIds, eqs ) ->
                                                        finishEqns <|
                                                            TypeEquation.cons
                                                                ( TypeI.id_ ctorId, functionType argIds resultId, "NamedPattern: kernel ctor is a fn" )
                                                                (TypeEquation.cons
                                                                    ( type_, TypeI.id_ resultId, "NamedPattern: result" )
                                                                    eqs
                                                                )

                            else
                                State.do
                                    (ModuleLookup.findModuleOfVar
                                        ctx.moduleMapping
                                        ctx.index
                                        ctx.modules
                                        ctx.thisModule
                                        (FullModuleName.fromModuleName customType.moduleName)
                                        customType.name
                                    )
                                <|
                                    \( package, moduleId ) ->
                                        State.do (State.lookupGlobalEnv ctx.moduleMapping package moduleId customType.name) <|
                                            \ctorType ->
                                                State.do State.getNextIdAndTick <|
                                                    \resultId ->
                                                        State.do (inferMany (\arg -> inferPattern ctx arg) args) <|
                                                            \( argIds, eqs ) ->
                                                                finishEqns <|
                                                                    TypeEquation.cons
                                                                        ( ctorType, functionType argIds resultId, "NamedPattern: constructor is a fn" )
                                                                        (TypeEquation.cons
                                                                            ( type_, TypeI.id_ resultId, "NamedPattern: result" )
                                                                            eqs
                                                                        )

                        Err details ->
                            State.error (toError ctx details)

                AsPattern p1 varNameNode ->
                    State.do (State.addBinding (Node.value varNameNode) (TypeI.mono type_)) <|
                        \() ->
                            State.do (State.aliasNodeId (Node.range varNameNode) patternId) <|
                                \() ->
                                    State.do (inferPattern ctx p1) <|
                                        \( id1, eqs1 ) ->
                                            finishEqns <| TypeEquation.cons ( type_, TypeI.id_ id1, "AsPattern = inner" ) eqs1

                ParenthesizedPattern p1 ->
                    State.do (inferPattern ctx p1) <|
                        \( id1, eqs1 ) ->
                            finishEqns <| TypeEquation.cons ( type_, TypeI.id_ id1, "Parenthesized pattern = inner" ) eqs1



-- GLSL


{-| Extracts the `uniform` / `attribute` / `varying` declarations of a shader.

Approximation, not a full GLSL parser.

       uniform /* hello */ mat4 u_x, u_y, u_z;
       attribute
         vec4 a_position
            ;
       uniform lowp float u_alpha;

TODO preprocessor directives (`#ifdef`, `#define`d types)
TODO `struct` declarations

Prior art:
\* <https://github.com/noteed/language-glsl/blob/master/Language/GLSL/Parser.hs>
\* what elm/compiler uses under the hood
\* <https://github.com/shuhei/elm-compiler/blob/glsl-parser/compiler/src/Parse/Shader.hs>
\* this one might be doing the least work
\* <https://github.com/w0rm/elm-glsl/blob/main/Language/GLSL/NewParser.hs>
\* written using elm-parser-like primitives

-}
glslDeclarations :
    String
    ->
        { uniforms : Dict VarName MonoType
        , attributes : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }
glslDeclarations code =
    code
        |> Regex.replace glslCommentRegex (\_ -> " ")
        |> String.split ";"
        |> List.foldl
            (\chunk acc -> List.foldl insertGlslDeclaration acc (glslDeclaration chunk))
            { uniforms = Dict.empty
            , attributes = Dict.empty
            , varyings = Dict.empty
            }


insertGlslDeclaration :
    ( String, VarName, MonoType )
    ->
        { uniforms : Dict VarName MonoType
        , attributes : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }
    ->
        { uniforms : Dict VarName MonoType
        , attributes : Dict VarName MonoType
        , varyings : Dict VarName MonoType
        }
insertGlslDeclaration ( storageQualifier, varName, varType ) acc =
    case storageQualifier of
        "attribute" ->
            { uniforms = acc.uniforms
            , attributes = Dict.insert varName varType acc.attributes
            , varyings = acc.varyings
            }

        "varying" ->
            { uniforms = acc.uniforms
            , attributes = acc.attributes
            , varyings = Dict.insert varName varType acc.varyings
            }

        "uniform" ->
            { uniforms = Dict.insert varName varType acc.uniforms
            , attributes = acc.attributes
            , varyings = acc.varyings
            }

        _ ->
            acc


{-|

     "uniform mediump mat4 u_x, u_y"
     -->
     [ ( "uniform", "u_x", ExternalTypes.mat4 )
     , ( "uniform", "u_y", ExternalTypes.mat4 )
     ]

-}
glslDeclaration : String -> List ( String, VarName, MonoType )
glslDeclaration chunk =
    case Regex.findAtMost 1 glslDeclarationRegex chunk of
        [ { submatches } ] ->
            case submatches of
                [ Just storageQualifier, _, Just varType, Just declarators ] ->
                    case parseGlslVarType varType of
                        Nothing ->
                            []

                        Just varType_ ->
                            declarators
                                |> String.split ","
                                |> List.filterMap
                                    (\declarator ->
                                        declarator
                                            |> glslDeclaratorName
                                            |> Maybe.map (\varName -> ( storageQualifier, varName, varType_ ))
                                    )

                _ ->
                    []

        _ ->
            []


glslDeclaratorName : String -> Maybe VarName
glslDeclaratorName declarator =
    let
        name : String
        name =
            declarator
                |> String.split "="
                |> List.head
                |> Maybe.withDefault ""
                |> String.trim
    in
    if Regex.contains glslVarNameRegex name then
        Just name

    else
        Nothing


parseGlslVarType : String -> Maybe MonoType
parseGlslVarType type_ =
    case type_ of
        "vec2" ->
            Just ExternalTypes.vec2

        "vec3" ->
            Just ExternalTypes.vec3

        "vec4" ->
            Just ExternalTypes.vec4

        "mat4" ->
            Just ExternalTypes.mat4

        "sampler2D" ->
            Just ExternalTypes.texture

        "int" ->
            Just Int

        "float" ->
            Just Float

        _ ->
            Nothing


glslDeclarationRegex : Regex
glslDeclarationRegex =
    Regex.fromString "^\\s*(uniform|attribute|varying)\\s+(highp\\s+|mediump\\s+|lowp\\s+)?([A-Za-z_][A-Za-z0-9_]*)\\s+([\\s\\S]+)"
        |> Maybe.withDefault Regex.never


glslVarNameRegex : Regex
glslVarNameRegex =
    Regex.fromString "^[A-Za-z_][A-Za-z0-9_]*$"
        |> Maybe.withDefault Regex.never


glslCommentRegex : Regex
glslCommentRegex =
    Regex.fromString "//[^\\n]*|/\\*[\\s\\S]*?\\*/"
        |> Maybe.withDefault Regex.never
