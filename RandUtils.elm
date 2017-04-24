module RandUtils exposing (
    randSubset
    , combine
    )
import Html exposing (text)
import Random exposing (..)

shiftInsert : Int -> List Int -> List Int
shiftInsert x ys =
  let
      f y =
        if y >= x
           then y + 1
           else y
  in
  x :: List.map f ys

-- Given a list of k Floats (presumably sampled uniformly at random from
-- [0,1)), produce a duplicate-free list of random elements of {0,...,n-1}
randSubset : Int -> List Float -> List Int
randSubset n fs = 
  if n <= 0 
     then []
     else case fs of
       [] -> []
       f :: gs -> 
         let
             x1 = floor (f * toFloat n)
         in
            randSubset (n-1) gs |> shiftInsert x1

-- For some reason the unit to the Random.Generator monad is not implemented.  
-- This is a workaround
unit : a -> Random.Generator a
unit x = Random.map (always x) (Random.int 0 0)

-- Combine a List of Random.Generator values into a Random.Generator with List value
combine : List (Random.Generator a) -> Random.Generator (List a)
combine xs = 
    List.foldl (Random.map2 (::)) (unit []) (List.reverse xs)


main = randSubset 20 [0.5,0.2,0.1,0.4,0.5,0.8,0.1,0.9,0.61,0.05,0.33] |> toString >> text

