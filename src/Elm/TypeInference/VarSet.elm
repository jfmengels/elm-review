module Elm.TypeInference.VarSet exposing
    ( GenKey
    , NamedKey
    , VarSet
    , diff
    , empty
    , fromList
    , genKeyFrom
    , insert
    , namedKeyFrom
    , superTypeTag
    , toList
    , union
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


{-| `comparable` encoding of a generated `TypeVar`: `id * 5 + superTypeTag`.
-}
type alias GenKey =
    Int


{-| `comparable` encoding of a named `TypeVar`: `(superTypeTag, name)`.
-}
type alias NamedKey =
    ( Int, String )


{-|

    genKeyFrom 5 Number --> 5 * 5 + 1 == 26

-}
genKeyFrom : Int -> SuperType -> GenKey
genKeyFrom theId superType =
    theId * 5 + superTypeTag superType


{-|

    namedKeyFrom "hello" Comparable --> (2, "hello")

-}
namedKeyFrom : String -> SuperType -> NamedKey
namedKeyFrom name superType =
    ( superTypeTag superType, name )


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
    , membersGen : Set GenKey
    , membersNamed : Set NamedKey
    }


empty : VarSet
empty =
    { order = []
    , membersGen = Set.empty
    , membersNamed = Set.empty
    }


insert : TypeVar -> VarSet -> VarSet
insert (( style, super ) as var) s =
    case style of
        Generated theId ->
            { order = var :: s.order
            , membersGen = Set.insert (genKeyFrom theId super) s.membersGen
            , membersNamed = s.membersNamed
            }

        Named name ->
            { order = var :: s.order
            , membersGen = s.membersGen
            , membersNamed = Set.insert (namedKeyFrom name super) s.membersNamed
            }


toList : VarSet -> List TypeVar
toList s =
    let
        go : Set GenKey -> Set NamedKey -> List TypeVar -> List TypeVar -> List TypeVar
        go seenGen seenNamed remaining acc =
            case remaining of
                [] ->
                    List.reverse acc

                (( style, super ) as var) :: rest ->
                    case style of
                        Generated theId ->
                            let
                                k : GenKey
                                k =
                                    genKeyFrom theId super
                            in
                            if Set.member k seenGen then
                                go seenGen seenNamed rest acc

                            else
                                go (Set.insert k seenGen) seenNamed rest (var :: acc)

                        Named name ->
                            let
                                k : NamedKey
                                k =
                                    namedKeyFrom name super
                            in
                            if Set.member k seenNamed then
                                go seenGen seenNamed rest acc

                            else
                                go seenGen (Set.insert k seenNamed) rest (var :: acc)
    in
    go Set.empty Set.empty s.order []


union : VarSet -> VarSet -> VarSet
union l r =
    { order = l.order ++ r.order
    , membersGen = Set.union r.membersGen l.membersGen
    , membersNamed = Set.union r.membersNamed l.membersNamed
    }


diff : VarSet -> VarSet -> VarSet
diff l r =
    { order =
        List.filter
            (\( style, super ) ->
                case style of
                    Generated theId ->
                        not (Set.member (genKeyFrom theId super) r.membersGen)

                    Named name ->
                        not (Set.member (namedKeyFrom name super) r.membersNamed)
            )
            l.order
    , membersGen = Set.diff l.membersGen r.membersGen
    , membersNamed = Set.diff l.membersNamed r.membersNamed
    }


fromList : List TypeVar -> VarSet
fromList vars =
    let
        ( genKeys, namedKeys ) =
            List.foldl
                (\( style, super ) ( genAcc, namedAcc ) ->
                    case style of
                        Generated theId ->
                            ( Set.insert (genKeyFrom theId super) genAcc, namedAcc )

                        Named name ->
                            ( genAcc, Set.insert (namedKeyFrom name super) namedAcc )
                )
                ( Set.empty, Set.empty )
                vars
    in
    { order = List.reverse vars
    , membersGen = genKeys
    , membersNamed = namedKeys
    }
