module NoUnused.NonemptyList exposing
    ( Nonempty(..)
    , fromElement
    , head
    , cons, pop
    , mapHead
    )

{-| Copied contents of mgold/elm-nonempty-list, and trimmed down unused functions.

This is to avoid dependency conflicts when mgold/elm-nonempty-list would release a new major version.

A list that cannot be empty. The head and tail can be accessed without Maybes. Most other list functions are
available.


# Definition

@docs Nonempty


# Create

@docs fromElement


# Access

@docs head, sample


# Convert

@docs cons, pop


# Map

@docs mapHead


# Original copyright notice

Copyright (c) 2015, Max Goldstein

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Max Goldstein nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}


{-| The Nonempty type. If you have both a head and tail, you can construct a
nonempty list directly. Otherwise use the helpers below instead.
-}
type Nonempty a
    = Nonempty a (List a)


{-| Create a singleton list with the given element.
-}
fromElement : a -> Nonempty a
fromElement x =
    Nonempty x []


{-| Return the head of the list.
-}
head : Nonempty a -> a
head (Nonempty x _) =
    x


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
cons : a -> Nonempty a -> Nonempty a
cons y (Nonempty x xs) =
    Nonempty y (x :: xs)


{-| Pop and discard the head, or do nothing for a singleton list. Useful if you
want to exhaust a list but hang on to the last item indefinitely.
pop (Nonempty 3 [ 2, 1 ]) --> Nonempty 2 [1]
pop (Nonempty 1 []) --> Nonempty 1 []
-}
pop : Nonempty a -> Nonempty a
pop (Nonempty x xs) =
    case xs of
        [] ->
            Nonempty x xs

        y :: ys ->
            Nonempty y ys


{-| Map the head to a value of the same type
-}
mapHead : (a -> a) -> Nonempty a -> Nonempty a
mapHead fn (Nonempty x xs) =
    Nonempty (fn x) xs
