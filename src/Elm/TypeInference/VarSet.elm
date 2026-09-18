module Elm.TypeInference.VarSet exposing
    ( VarKey
    , VarSet
    , diff
    , empty
    , fromList
    , insert
    , toList
    , union
    , varKey
    )

{-| An ordered set of `TypeVar`s, and the `TypeVar` identity it's keyed on.
-}

import Elm.TypeInference.TypeVar
    exposing
        ( SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )
import Set exposing (Set)


{-| `comparable` encoding of `TypeVar` to be able to use Dict: (id, superTypeTag, name)
-}
type alias VarKey =
    ( Int, Int, String )


{-|

    varKey (Generated 5, Number)
    --> (5, 1 {- Number -}, "")

    varKey (Named "hello", Comparable)
    --> (-1, 2 {- Comparable -}, "hello")

-}
varKey : TypeVar -> VarKey
varKey ( style, superType ) =
    case style of
        Generated theId ->
            ( theId, superTypeTag superType, "" )

        Named name ->
            ( -1, superTypeTag superType, name )


superTypeTag : SuperType -> Int
superTypeTag superType =
    case superType of
        Normal ->
            0

        Number ->
            1

        Comparable ->
            2

        Appendable ->
            3

        CompAppend ->
            4


{-| An ordered set of `TypeVar`s (`normalize` needs them in order of first appearance).
-}
type alias VarSet =
    { order : List TypeVar
    , members : Set VarKey
    }


empty : VarSet
empty =
    { order = [], members = Set.empty }


insert : TypeVar -> VarSet -> VarSet
insert var s =
    { order = var :: s.order
    , members = Set.insert (varKey var) s.members
    }


toList : VarSet -> List TypeVar
toList s =
    let
        go : Set VarKey -> List TypeVar -> List TypeVar -> List TypeVar
        go seen remaining acc =
            case remaining of
                [] ->
                    List.reverse acc

                var :: rest ->
                    let
                        k : VarKey
                        k =
                            varKey var
                    in
                    if Set.member k seen then
                        go seen rest acc

                    else
                        go (Set.insert k seen) rest (var :: acc)
    in
    go Set.empty s.order []


union : VarSet -> VarSet -> VarSet
union l r =
    { order = l.order ++ r.order
    , members = Set.union l.members r.members
    }


diff : VarSet -> VarSet -> VarSet
diff l r =
    { order = List.filter (\var -> not (Set.member (varKey var) r.members)) l.order
    , members = Set.diff l.members r.members
    }


fromList : List TypeVar -> VarSet
fromList vars =
    { order = List.reverse vars
    , members = Set.fromList (List.map varKey vars)
    }
