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


toList : List TypeVar -> List TypeVar
toList order =
    toListHelp Set.empty Set.empty order []


toListHelp : Set GenKey -> Set NamedKey -> List TypeVar -> List TypeVar -> List TypeVar
toListHelp seenGen seenNamed remaining acc =
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
                        toListHelp seenGen seenNamed rest acc

                    else
                        toListHelp (Set.insert k seenGen) seenNamed rest (var :: acc)

                Named name ->
                    let
                        k : NamedKey
                        k =
                            namedKeyFrom name super
                    in
                    if Set.member k seenNamed then
                        toListHelp seenGen seenNamed rest acc

                    else
                        toListHelp seenGen (Set.insert k seenNamed) rest (var :: acc)


diff : List TypeVar -> VarSet -> List TypeVar
diff order r =
    List.filter
        (\( style, super ) ->
            case style of
                Generated theId ->
                    not (Set.member (genKeyFrom theId super) r.membersGen)

                Named name ->
                    not (Set.member (namedKeyFrom name super) r.membersNamed)
        )
        order


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
