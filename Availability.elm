module Availability exposing
    ( Availability
    , Stock(..)
    , decoder
    , del
    , empty
    , get
    , set
    , toBoolAndString
    , updateAll
    )

import Debug
import HexId exposing (HexId)
import IntDict exposing (IntDict)
import Json.Decode as Decoder exposing (Decoder)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Tuple2 as Tuple
import Tuple3
import Util


type Stock
    = InStock
    | LessThan10
    | OutOfStock
    | Unknown


type alias IdToStock =
    { key : HexId
    , val : Stock
    }


type alias DictVal =
    { keys : ( Int, Int )
    , stock : Stock
    }


type Availability
    = Availability (IntDict (List DictVal))


lookup : HexId -> Availability -> ( Int, List DictVal, List DictVal )
lookup hexId (Availability inv) =
    let
        ( hi, mid, lo ) =
            HexId.toInt96 hexId

        res =
            IntDict.get lo inv
                |> Maybe.withDefault []
    in
    case res of
        [] ->
            ( lo, [], [] )

        _ ->
            ( lo
            , res
                |> List.break
                    (\{ keys } ->
                        Tuple.mapBoth ((==) hi) ((==) mid) keys
                            |> Tuple.uncurry (&&)
                    )
            )
                |> Tuple3.splitSecond


del : HexId -> Availability -> Availability
del hexId (Availability inv) =
    case lookup hexId (Availability inv) of
        ( _, _, [] ) ->
            Availability inv

        ( key, prefix, _ :: rest ) ->
            Availability (IntDict.insert key (prefix ++ rest) inv)


get : HexId -> Availability -> Stock
get hexId inventory =
    case lookup hexId inventory |> Tuple3.third of
        [] ->
            Unknown

        { stock } :: _ ->
            stock


empty : Availability
empty =
    Availability IntDict.empty


fromList : List ( HexId, Stock ) -> Availability
fromList kvList =
    List.foldl
        (\( key, val ) acc ->
            set key val acc
        )
        empty
        kvList


set : HexId -> Stock -> Availability -> Availability
set hexId stock_ (Availability inv) =
    case stock_ of
        Unknown ->
            del hexId (Availability inv)

        _ ->
            let
                ( hi, mid, lo ) =
                    HexId.toInt96 hexId

                modifiedList =
                    case lookup hexId (Availability inv) of
                        ( key, prefix, [] ) ->
                            { keys = ( hi, mid ), stock = stock_ } :: prefix

                        ( key, prefix, first :: rest ) ->
                            { first | stock = stock_ } :: (prefix ++ rest)
            in
            Availability (IntDict.insert lo modifiedList inv)


toBoolAndString : Stock -> ( Bool, String )
toBoolAndString stock =
    case stock of
        InStock ->
            ( True, "In stock" )

        LessThan10 ->
            ( True, "Less than 10 left" )

        OutOfStock ->
            ( False, "Out of stock" )

        Unknown ->
            ( False, "Stock unknown – please contact us about this product!" )



-- keeps a key-value pair when it appears in both, preferring the value in new.


updateAll : Availability -> Availability -> Availability
updateAll (Availability old) (Availability new) =
    Availability (IntDict.intersect old new)


decoder : Decoder Availability
decoder =
    Decoder.map fromList (Decoder.field "response" (Decoder.list idToStockDecoder))


idToStockDecoder : Decoder ( HexId, Stock )
idToStockDecoder =
    Decoder.map2 Tuple.pair
        (Decoder.field "id" HexId.decoder)
        (Decoder.field "DATAPAYLOAD" payloadDecoder)


payloadDecoder : Decoder Stock
payloadDecoder =
    Decoder.string
        |> Decoder.andThen
            (\str ->
                let
                    parsed =
                        Parser.run inStockValueParser str
                in
                case parsed of
                    Err _ ->
                        Decoder.fail ("Error parsing: " ++ str)

                    Ok res ->
                        let
                            stockValue =
                                if String.isEmpty res then
                                    Unknown

                                else if res == "outofstock" then
                                    OutOfStock

                                else if res == "lessthan10" then
                                    LessThan10

                                else
                                    InStock
                        in
                        Decoder.succeed stockValue
            )


inStockValueParser : Parser String
inStockValueParser =
    let
        parser =
            Parser.map String.toLower <|
                Parser.succeed String.slice
                    |. Parser.chompUntil "<INSTOCKVALUE>"
                    |. Parser.token "<INSTOCKVALUE>"
                    |= Parser.getOffset
                    |. Parser.chompWhile (\c -> c /= '<')
                    |= Parser.getOffset
                    |= Parser.getSource
    in
    parser
