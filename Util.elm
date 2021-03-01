module Util exposing (..)

import Array exposing (Array)
import Bitwise as Bit
import List.Extra as List
import Tuple3



-- Bitwise


lsb : Int -> Int
lsb v =
  Bit.and v (negate v)


deBruijnConstant : Int
deBruijnConstant =
  0x077CB531


deBruijnBitPosition : Array Int
deBruijnBitPosition =
  [ 0
  , 1
  , 28
  , 2
  , 29
  , 14
  , 24
  , 3
  , 30
  , 22
  , 20
  , 15
  , 25
  , 17
  , 4
  , 8
  , 31
  , 27
  , 13
  , 23
  , 21
  , 19
  , 16
  , 7
  , 26
  , 12
  , 18
  , 6
  , 11
  , 5
  , 10
  , 9
  ]
    |> Array.fromList


orShiftRightBy : Int -> Int -> Int
orShiftRightBy i v =
  Bit.or v (Bit.shiftRightBy i v)



-- https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2


nextUIntPow2 : Int -> Int
nextUIntPow2 v =
  if v == 0 then
    1

  else
    orShiftRightBy 1 (v - 1)
      |> orShiftRightBy 2
      |> orShiftRightBy 4
      |> orShiftRightBy 8
      |> orShiftRightBy 16
      |> (+) 1


trailingZRight : Int -> Int
trailingZRight v =
  let
    idx =
      Bit.shiftRightBy 27 (lsb v * deBruijnConstant)
  in
  Array.get idx deBruijnBitPosition
    |> Maybe.withDefault 0



-- Tuples


tuple2Via : (a -> b) -> a -> a -> ( b, b )
tuple2Via f x y =
  ( f x, f y )


tuple3Via : (a -> b) -> a -> a -> a -> ( b, b, b )
tuple3Via f x y z =
  ( f x, f y, f z )


tuple2Map : (a -> b) -> ( a, a ) -> ( b, b )
tuple2Map f x =
  Tuple.mapBoth f f x


tuple3Map : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tuple3Map f x =
  Tuple3.mapAllThree f f f x


tuple2Functor : ( a -> b, a -> c ) -> a -> ( b, c )
tuple2Functor ( f, g ) x =
  ( f x, g x )


tuple3Functor : ( a -> b, a -> c, a -> d ) -> a -> ( b, c, d )
tuple3Functor ( f, g, h ) x =
  ( f x, g x, h x )


tuple3Find : (a -> Bool) -> ( a, a, a ) -> Maybe a
tuple3Find f ( x1, x2, x3 ) =
  if f x1 then
    Just x1

  else if f x2 then
    Just x2

  else if f x3 then
    Just x3

  else
    Nothing


tuple3DropFirst : ( a, b, c ) -> ( b, c )
tuple3DropFirst ( x, y, z ) =
  ( y, z )


tuple3DropSecond : ( a, b, c ) -> ( a, c )
tuple3DropSecond ( x, y, z ) =
  ( x, z )


tuple3DropThird : ( a, b, c ) -> ( a, b )
tuple3DropThird ( x, y, z ) =
  ( x, y )


tuple3MaybeMap : (a -> Maybe b) -> ( a, a, a ) -> Maybe ( b, b, b )
tuple3MaybeMap f t =
  tuple3Map f t
    |> Tuple3.uncurry (Maybe.map3 Tuple3.join)



-- List


listFindWithIdx : (a -> Bool) -> List a -> Maybe ( Int, a )
listFindWithIdx f li =
  case List.break f li of
    ( _, [] ) ->
      Nothing

    ( prefix, first :: rest ) ->
      Just ( List.length prefix, first )
