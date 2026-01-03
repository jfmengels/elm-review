module NoUnused.Parameters.ParameterPath exposing
    ( Nesting
    , Path
    , fixCall
    , fixDeclaration
    , inAlias
    , inNamedPattern
    , inRecord
    , inTuple
    , new
    , nextArgument
    )

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import List.Extra


type alias Path =
    { functionName : String
    , index : Int
    , nesting : List Nesting
    , isUnderAlias : Bool
    , nameLocation : Location
    , arguments : List (Node Pattern)
    , typeAnnotation : Maybe (Node TypeAnnotation)
    , hasOnlyOneArgument : Bool
    }


type Nesting
    = RecordField String
    | TupleField Int
    | NamedPattern
    | AliasPattern


new : String -> Location -> List (Node Pattern) -> Maybe (Node Signature) -> Path
new functionName nameLocation arguments signature =
    { functionName = functionName
    , index = 0
    , nesting = []
    , isUnderAlias = False
    , nameLocation = nameLocation
    , arguments = arguments
    , typeAnnotation = Maybe.map (\(Node _ { typeAnnotation }) -> typeAnnotation) signature
    , hasOnlyOneArgument =
        case arguments of
            [ _ ] ->
                True

            _ ->
                False
    }


nextArgument : Path -> Path
nextArgument path =
    { functionName = path.functionName
    , index = path.index + 1
    , nesting = []
    , isUnderAlias = False
    , nameLocation = path.nameLocation
    , arguments = path.arguments
    , typeAnnotation = path.typeAnnotation
    , hasOnlyOneArgument = path.hasOnlyOneArgument
    }


argumentInTypeAnnotation : Int -> Node TypeAnnotation -> Maybe ( Node TypeAnnotation, Location )
argumentInTypeAnnotation index (Node _ type_) =
    case type_ of
        TypeAnnotation.FunctionTypeAnnotation typeAnnotation ((Node nextRange _) as rest) ->
            if index == 0 then
                Just ( typeAnnotation, nextRange.start )

            else
                argumentInTypeAnnotation (index - 1) rest

        _ ->
            Nothing


inRecord : String -> Path -> Path
inRecord fieldName path =
    { functionName = path.functionName
    , index = path.index
    , nesting = RecordField fieldName :: path.nesting
    , isUnderAlias = path.isUnderAlias
    , nameLocation = path.nameLocation
    , arguments = path.arguments
    , typeAnnotation = path.typeAnnotation
    , hasOnlyOneArgument = path.hasOnlyOneArgument
    }


findFieldInTypeAnnotation : String -> Range -> List (Node TypeAnnotation.RecordField) -> Maybe ( Node TypeAnnotation, Range )
findFieldInTypeAnnotation fieldName fullArgRange list =
    findNeighboring (\_ (Node _ ( Node _ name, _ )) -> name == fieldName) fullArgRange.end list
        |> Maybe.map (\( Node _ ( _, fieldType ), range ) -> ( fieldType, range ))


findTupleInTypeAnnotation : Int -> Range -> Maybe Range -> List (Node TypeAnnotation) -> Maybe ( Node TypeAnnotation, Range )
findTupleInTypeAnnotation index fullArgRange previousRange list =
    case list of
        [] ->
            Nothing

        ((Node range _) as node) :: rest ->
            if index == 0 then
                Just
                    ( node
                    , case previousRange of
                        Just previous ->
                            -- Not the first element
                            { start = previous.end, end = range.end }

                        Nothing ->
                            case rest of
                                (Node next _) :: _ ->
                                    -- first element
                                    { start = range.start, end = next.start }

                                [] ->
                                    -- only element
                                    fullArgRange
                    )

            else
                findTupleInTypeAnnotation (index - 1) fullArgRange (Just range) rest


inTuple : Int -> Path -> Path
inTuple index path =
    { functionName = path.functionName
    , index = path.index
    , nesting = TupleField index :: path.nesting
    , isUnderAlias = path.isUnderAlias
    , nameLocation = path.nameLocation
    , arguments = path.arguments
    , typeAnnotation = path.typeAnnotation
    , hasOnlyOneArgument = path.hasOnlyOneArgument
    }


inNamedPattern : Path -> Path
inNamedPattern path =
    { functionName = path.functionName
    , index = path.index
    , nesting = NamedPattern :: path.nesting
    , isUnderAlias = path.isUnderAlias
    , nameLocation = path.nameLocation
    , arguments = path.arguments
    , typeAnnotation = path.typeAnnotation
    , hasOnlyOneArgument = path.hasOnlyOneArgument
    }


inAlias : Path -> Path
inAlias path =
    { functionName = path.functionName
    , index = path.index
    , nesting = AliasPattern :: path.nesting
    , isUnderAlias = True
    , nameLocation = path.nameLocation
    , arguments = path.arguments
    , typeAnnotation = path.typeAnnotation
    , hasOnlyOneArgument = path.hasOnlyOneArgument
    }


fixCall : Range -> Node Expression -> List Nesting -> List Range -> Maybe (List Range)
fixCall initialRange initialNode nesting edits =
    case fixCallHelp nesting initialNode initialRange of
        Just range ->
            Just (range :: edits)

        Nothing ->
            Nothing


fixCallHelp : List Nesting -> Node Expression -> Range -> Maybe Range
fixCallHelp nesting parentNode previousRange =
    case nesting of
        [] ->
            Just previousRange

        AliasPattern :: rest ->
            fixCallHelp rest parentNode previousRange

        (RecordField name) :: rest ->
            case removeParensFromExpression parentNode of
                Expression.RecordExpr fields ->
                    case findFieldInRecordExpression previousRange name fields of
                        Just ( node, range ) ->
                            fixCallHelp rest node range

                        Nothing ->
                            Nothing

                Expression.RecordUpdateExpression _ _ ->
                    -- TODO
                    Nothing

                _ ->
                    Nothing

        (TupleField index) :: rest ->
            case removeParensFromExpression parentNode of
                Expression.TupledExpression elements ->
                    case getNeighboring index previousRange elements of
                        Just ( node, range ) ->
                            fixCallHelp rest node range

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


fixDeclaration : Path -> Maybe (List Range)
fixDeclaration path =
    let
        nesting : List Nesting
        nesting =
            List.reverse path.nesting
    in
    case
        findNeighboring (\index _ -> path.index == index) path.nameLocation path.arguments
            |> Maybe.andThen
                (\( node, argRange ) ->
                    fixDeclarationPattern nesting node argRange
                )
    of
        Just patternFixRange ->
            case path.typeAnnotation of
                Nothing ->
                    Just [ patternFixRange ]

                Just typeAnnotation ->
                    case fixDeclarationSignature path.index nesting typeAnnotation of
                        Just signatureFixRange ->
                            Just [ signatureFixRange, patternFixRange ]

                        Nothing ->
                            Nothing

        Nothing ->
            Nothing


fixDeclarationPattern : List Nesting -> Node Pattern -> Range -> Maybe Range
fixDeclarationPattern nesting parentNode previousRange =
    case nesting of
        [] ->
            Just previousRange

        AliasPattern :: rest ->
            case removeParensFromPattern parentNode of
                Pattern.AsPattern pattern _ ->
                    -- TODO previousRange?
                    fixDeclarationPattern rest pattern previousRange

                _ ->
                    Nothing

        (RecordField name) :: [] ->
            case removeParensFromPattern parentNode of
                Pattern.RecordPattern fields ->
                    findFieldRangeInPattern previousRange name fields

                _ ->
                    Nothing

        (TupleField index) :: rest ->
            case removeParensFromPattern parentNode of
                Pattern.TuplePattern elements ->
                    case getNeighboring index previousRange elements of
                        Just ( node, range ) ->
                            fixDeclarationPattern rest node range

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


fixDeclarationSignature : Int -> List Nesting -> Node TypeAnnotation -> Maybe Range
fixDeclarationSignature index nesting typeAnnotation =
    argumentInTypeAnnotation index typeAnnotation
        |> Maybe.andThen
            (\( node, end ) ->
                removeArgumentInTypeAnnotation
                    nesting
                    node
                    { start = (Node.range node).start, end = end }
            )


removeArgumentInTypeAnnotation : List Nesting -> Node TypeAnnotation -> Range -> Maybe Range
removeArgumentInTypeAnnotation nesting node parentRange =
    case nesting of
        [] ->
            Just parentRange

        AliasPattern :: rest ->
            removeArgumentInTypeAnnotation rest node parentRange

        (RecordField name) :: [] ->
            case removeParensFromTypeAnnotation (Node.value node) of
                TypeAnnotation.Record fields ->
                    findFieldInTypeAnnotation name (Node.range node) fields
                        |> Maybe.map Tuple.second

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    findFieldInTypeAnnotation name (Node.range node) fields
                        |> Maybe.map Tuple.second

                _ ->
                    Nothing

        (TupleField index) :: rest ->
            case removeParensFromTypeAnnotation (Node.value node) of
                TypeAnnotation.Tupled elements ->
                    case findTupleInTypeAnnotation index (Node.range node) Nothing elements of
                        Just ( subNode, elementRange ) ->
                            removeArgumentInTypeAnnotation rest subNode elementRange

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


findFieldInRecordExpression : Range -> String -> List (Node Expression.RecordSetter) -> Maybe ( Node Expression, Range )
findFieldInRecordExpression fullRange fieldName list =
    findNeighboring (\_ (Node _ ( Node _ name, _ )) -> name == fieldName) fullRange.end list
        |> Maybe.map (\( Node _ ( _, value ), range ) -> ( value, range ))


findFieldRangeInPattern : Range -> String -> List (Node String) -> Maybe Range
findFieldRangeInPattern previousRange fieldName list =
    findNeighboring (\_ (Node _ name) -> name == fieldName) previousRange.end list
        |> Maybe.map Tuple.second


removeParensFromTypeAnnotation : TypeAnnotation -> TypeAnnotation
removeParensFromTypeAnnotation node =
    case node of
        TypeAnnotation.Tupled [ Node _ single ] ->
            removeParensFromTypeAnnotation single

        _ ->
            node


removeParensFromExpression : Node Expression -> Expression
removeParensFromExpression (Node _ expr) =
    case expr of
        Expression.ParenthesizedExpression e ->
            removeParensFromExpression e

        _ ->
            expr


removeParensFromPattern : Node Pattern -> Pattern
removeParensFromPattern (Node _ expr) =
    case expr of
        Pattern.ParenthesizedPattern e ->
            removeParensFromPattern e

        _ ->
            expr


findNeighboring : (Int -> Node a -> Bool) -> Location -> List (Node a) -> Maybe ( Node a, Range )
findNeighboring predicate previousEnd list =
    List.Extra.findNeighboring predicate list
        |> Maybe.map
            (\( (Node range _) as node, { before, after } ) ->
                ( node
                , case before of
                    Just (Node before_ _) ->
                        -- Not the first element
                        { start = before_.end, end = range.end }

                    Nothing ->
                        case after of
                            Just (Node after_ _) ->
                                -- first element
                                { start = range.start, end = after_.start }

                            Nothing ->
                                -- only element
                                { start = previousEnd, end = range.end }
                )
            )


getNeighboring : Int -> Range -> List (Node a) -> Maybe ( Node a, Range )
getNeighboring index fullRange list =
    List.Extra.getNeighboring index list
        |> Maybe.map
            (\( (Node range _) as node, { before, after } ) ->
                ( node
                , case before of
                    Just (Node before_ _) ->
                        -- Not the first element
                        { start = before_.end, end = range.end }

                    Nothing ->
                        case after of
                            Just (Node after_ _) ->
                                -- first element
                                { start = range.start, end = after_.start }

                            Nothing ->
                                -- only element
                                fullRange
                )
            )
