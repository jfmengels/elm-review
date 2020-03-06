module Vendor.Graph.Tree exposing
    ( Tree, Forest
    , empty, leaf, inner, unfoldTree, unfoldForest
    , map
    , isEmpty, root, size, height
    , levelOrder, levelOrderList, preOrder, preOrderList, postOrder, postOrderList
    )

{-| This module provides a simple tree data type of arbitrary arity (a rose tree).
There are primitives for building and traversing such a tree.


# Data

@docs Tree, Forest


# Building

@docs empty, leaf, inner, unfoldTree, unfoldForest


# Transforming

@docs map


# Query

@docs isEmpty, root, size, height


# Traversal

It is instructory to read the article on [tree traversals on Wikipedia](https://en.wikipedia.org/wiki/Tree_traversal) first if you are not familiar with the concept.

@docs levelOrder, levelOrderList, preOrder, preOrderList, postOrder, postOrderList

-}

import Vendor.Fifo as Fifo exposing (Fifo)


{-| Data type representing an n-ary tree with node labels of type `a`
Building such a tree is done with the `empty`, `leaf` and `inner` smart
constructors. An example for a tree with three leafs and a root node:

    tree =
        inner 1 [ leaf 2, leaf 3, leaf 4 ]

-}
type Tree label
    = MkTree Int (Maybe ( label, List (Tree label) ))


{-| This is just an alias for a list of trees, called a forest in the
literature.
-}
type alias Forest label =
    List (Tree label)



{- BUILDING -}


{-| Construct an empty tree with no nodes.
-}
empty : Tree label
empty =
    MkTree 0 Nothing


{-| Construct a tree with a single node from a value for the node's label.

    tree : Tree Int
    tree =
        leaf 42

-}
leaf : label -> Tree label
leaf val =
    inner val []


{-| Construct a new tree by `inner label children`, combining a number of
subtrees `children` with a `label` for the new inner node which will be
the root of the tree. Empty subtrees are filtered out. An example:

    tree1 = inner 1 [leaf 2, leaf 3, leaf 4]
    tree2 = inner 1 [leaf 2, leaf 3, leaf 4, empty]
    tree1 == tree2

-}
inner : label -> List (Tree label) -> Tree label
inner label children =
    let
        nonEmptyChildren =
            List.filter (not << isEmpty) children

        totalSize =
            List.foldl ((+) << size) 1 nonEmptyChildren
    in
    MkTree totalSize (Just ( label, nonEmptyChildren ))


{-| Construct a new tree with `unfoldTree next seed`, top to bottom. `next` will be
called repeatedly with seeds, from which it should construct a label for
the current tree node but also a list of seeds from which to unfold
child nodes. This sort of works top to bottom compared to creating a
tree bottom up by using the other primitives.

    tree1 = inner 1 [leaf 2, leaf 3, leaf 4]
    next seed = (seed, if seed == 1 then [2, 3, 4] else [])
    tree2 = unfoldTree next 1
    tree1 == tree2

-}
unfoldTree : (seed -> ( label, List seed )) -> seed -> Tree label
unfoldTree next seed =
    let
        ( label, seeds ) =
            next seed
    in
    inner label (List.map (unfoldTree next) seeds)


{-| Construct a new forest with `unfoldForest next seeds` by `unfoldTree next seed`
for each `seed` in `seeds`. A simple specification would be

    unfoldForest next seeds =
        List.map (unfoldTree next) seeds

-}
unfoldForest : (seed -> ( label, List seed )) -> List seed -> Forest label
unfoldForest next seeds =
    List.map (unfoldTree next) seeds



{- TRANSFORMING -}


{-| `map f tree` applies supplied function f to every label in a tree, without changing the structure of the tree

    map (\x -> x + 1) empty == empty

    map (\x -> x * 10) (inner 1 [ leaf 2, leaf 3 ]) == inner 10 [ leaf 20, leaf 30 ]

-}
map : (a -> b) -> Tree a -> Tree b
map f (MkTree totalSize maybeLabelAndChildren) =
    MkTree totalSize <|
        Maybe.map
            (\( label, children ) -> ( f label, List.map (map f) children ))
            maybeLabelAndChildren



{- QUERY -}


{-| `isEmpty tree` returns true if and only if `tree` is `empty`.

    isEmpty empty == True

    isEmpty (leaf 42) == False

-}
isEmpty : Tree label -> Bool
isEmpty tree =
    tree == empty


{-| `root tree` returns `Nothing` if `tree` is `empty`, otherwise
it returns `Just (label, childForest)` of the root node.

    tree = inner 1 [leaf 2, leaf 3, leaf 4]
    root tree == Just (1, [leaf 2, leaf 3, leaf 4])
    root empty == Nothing

-}
root : Tree label -> Maybe ( label, Forest label )
root tree =
    case tree of
        MkTree _ maybe ->
            maybe


{-| The size of the tree, e.g. the number of nodes.

    tree = inner 0 [inner 1 [leaf 2, leaf 3], inner 4 [leaf 5, leaf 6]]
    size tree == 7

-}
size : Tree label -> Int
size tree =
    case tree of
        MkTree n _ ->
            n


{-| Computes the height of the tree in O(n) time.

    tree = inner 0 [inner 1 [leaf 2, leaf 3], inner 4 [leaf 5, leaf 6]]
    height tree == 3

-}
height : Tree label -> Int
height tree =
    let
        go h t =
            case root t of
                Just ( _, children ) ->
                    children
                        |> List.foldl (go (h + 1) >> max) (h + 1)

                Nothing ->
                    h
    in
    go 0 tree



{- TRAVERSAL -}
-- This is private. No type annotation for this, traversal is quite daunting.


listForTraversal traversal tree =
    -- we will compute a DList that we then can turn into a List.
    let
        f label children rest =
            (::) label >> rest

        acc =
            identity

        -- the call to postOrder returns a DList ([a] -> [a]), so [] turns it into a list
    in
    traversal f acc tree []



-- This is also not exported.


pushMany : List a -> Fifo a -> Fifo a
pushMany vals queue =
    List.foldl Fifo.insert queue vals


{-| `levelOrder visit acc tree` is a breadth-first fold over `tree`,
visiting each node and accumulating results with `visit`. Nodes are visited
in level-order, e.g. for a tree like

    tree =
        inner 0 [ inner 1 [ leaf 2, leaf 3 ], inner 4 [ leaf 5, leaf 6 ] ]

nodes would be visited in order `[0, 1, 4, 2, 3, 5, 6]`. This is in fact the
list produced by `levelOrderList`, but through `levelOrder` you also get access
to the children of the current node via the second parameter of visit.

-}
levelOrder : (label -> Forest label -> acc -> acc) -> acc -> Tree label -> acc
levelOrder visit acc tree =
    let
        go acc_ toVisit =
            case Fifo.remove toVisit of
                ( Nothing, _ ) ->
                    acc_

                ( Just tree_, othersToVisit ) ->
                    case root tree_ of
                        Nothing ->
                            go acc_ othersToVisit

                        Just ( label, children ) ->
                            go (visit label children acc_) (pushMany children othersToVisit)
    in
    go acc (Fifo.empty |> Fifo.insert tree)


{-| See the documentation on `levelOrder`. `levelOrderList tree` produces
a list of the nodes of the tree visited in level-order, e.g. breadth-first.
So:

    tree = inner 0 [inner 1 [leaf 2, leaf 3], inner 4 [leaf 5, leaf 6]]
    levelOrderList tree == [0, 1, 4, 2, 3, 5, 6]

If you also need information on child trees instead of just the node labels,
use `levelOrder`.

-}
levelOrderList : Tree label -> List label
levelOrderList =
    listForTraversal levelOrder


{-| `postOrder visit acc tree` is a (depth-first) post-order traversal (fold)
over `tree` where `visit` is called with the label and the child sub-forest of
the current node in addition to a supplied accumulator value.

When `visit` is called for some node, `acc` already contains the value of all
sub-trees, so post-order traversal is a kind of bottom-up traversal, where
all children are visited prior to their parent.
See `postOrderList` for an example on the order in which nodes are visited.

-}
postOrder : (label -> Forest label -> acc -> acc) -> acc -> Tree label -> acc
postOrder visit acc tree =
    let
        folder =
            \b a -> postOrder visit a b
    in
    case root tree of
        Nothing ->
            acc

        Just ( label, children ) ->
            visit label children (List.foldl folder acc children)


{-| See `postOrder` for an explanation of how post-order traversals work.
Here is an example on visit order:

    tree = inner 0 [inner 1 [leaf 2, leaf 3], inner 4 [leaf 5, leaf 6]]
    postOrderList tree == [2, 3, 1, 5, 6, 4, 0]

If you also need information on child trees instead of just the node labels,
use `postOrder`.

-}
postOrderList : Tree label -> List label
postOrderList =
    listForTraversal postOrder


{-| `preOrder visit acc tree` is a (depth-first) pre-order traversal (fold)
over `tree` where `visit` is called with the label and the child sub-forest of
the current node in addition to a supplied accumulator value.

Post-order traversals work top-down: When `visit` is called for some node,
`acc` already contains the value of all ancestral nodes.
See `preOrderList` for an example on the order in which nodes are visited.

-}
preOrder : (label -> Forest label -> acc -> acc) -> acc -> Tree label -> acc
preOrder visit acc tree =
    let
        folder =
            \b a -> preOrder visit a b
    in
    case root tree of
        Nothing ->
            acc

        Just ( label, children ) ->
            List.foldl folder (visit label children acc) children


{-| See `preOrder` for an explanation of how post-order traversals work.
Here is an example on visit order:

    tree = inner 0 [inner 1 [leaf 2, leaf 3], inner 4 [leaf 5, leaf 6]]
    preOrderList tree == [0, 1, 2, 3, 4, 5, 6]

If you also need information on child trees instead of just the node labels,
use `preOrder`.

-}
preOrderList : Tree label -> List label
preOrderList =
    listForTraversal preOrder
