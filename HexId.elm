module HexId exposing
    ( HexId
    , compare
    , decoder
    , eq
    , eqHi
    , eqLo
    , eqMid
    , fromString
    , toInt96
    , toString
    )

import Bitwise as Bit
import Hex as Hex
import Json.Decode as Decoder exposing (Decoder)
import Tuple3
import Util



-- Types & typeclass intantiation
--------


type alias Int96 =
    ( Int, Int, Int )


type HexId
    = HexId String Int96


compare : HexId -> HexId -> Order
compare (HexId _ ( lo1, mid1, hi1 )) (HexId _ ( lo2, mid2, hi2 )) =
    if hi1 < hi2 then
        LT

    else if hi1 > hi2 then
        GT

    else if mid1 < mid2 then
        LT

    else if mid1 > mid2 then
        GT

    else if lo1 < lo2 then
        LT

    else if lo1 > lo2 then
        GT

    else
        EQ


eq : HexId -> HexId -> Bool
eq a b =
    if not (eqHi a b) then
        False

    else if not (eqMid a b) then
        False

    else if not (eqLo a b) then
        False

    else
        True


eqHi : HexId -> HexId -> Bool
eqHi (HexId _ ( hi1, _, _ )) (HexId _ ( hi2, _, _ )) =
    Bit.xor hi1 hi2 == 0


eqMid : HexId -> HexId -> Bool
eqMid (HexId _ ( _, mid1, _ )) (HexId _ ( _, mid2, _ )) =
    Bit.xor mid1 mid2 == 0


eqLo : HexId -> HexId -> Bool
eqLo (HexId _ ( _, _, lo1 )) (HexId _ ( _, _, lo2 )) =
    Bit.xor lo1 lo2 == 0


toInt96 : HexId -> ( Int, Int, Int )
toInt96 (HexId _ int96) =
    int96


toString : HexId -> String
toString (HexId id _) =
    id


fromString : String -> Maybe HexId
fromString inputStr =
    let
        res =
            String.padLeft 24 '0' inputStr
                |> Tuple3.triple
                |> Tuple3.mapAllThree (String.right 8) (String.slice 8 16) (String.left 8)
                |> Util.tuple3MaybeMap (Hex.fromString >> Result.toMaybe)
    in
    case res of
        Just int96value ->
            Just (HexId inputStr int96value)

        Nothing ->
            Nothing


decoder : Decoder HexId
decoder =
    Decoder.string
        |> Decoder.andThen
            (\str ->
                case fromString <| String.toLower str of
                    Nothing ->
                        Decoder.fail ("The string:" ++ str ++ " is not a valid HexId.")

                    Just hexId ->
                        Decoder.succeed hexId
            )
