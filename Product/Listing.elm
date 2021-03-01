module Product.Listing exposing
  ( Description
  , Listing
  , ListingView
  , create
  , decoder
  , description
  , idString
  , idValue
  , view
  , viewKeyed
  )

import HexId exposing (HexId)
import Html exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Palette.X11 exposing (..)
import Product.Category as Category exposing (Category)
import Product.ColorSpec as ColorSpec exposing (ColorSpec)
import SolidColor exposing (SolidColor)



-- TYPES


type alias Description =
  { category : Category
  , name : String
  , manufacturer : String
  , price : Int
  , color : List ColorSpec
  }


type alias ListingView =
  ( Listing, { visibility : Bool, stock : String } )



{--Main type for a single product, having an id and an associated description
  record. We export the type but keep the constructor opaque.
--}


type Listing
  = Listing HexId Description



-- FUNCTIONS
-- Exported constructor for `Listing`


create : HexId -> Description -> Listing
create hexId { category, name, manufacturer, price, color } =
  Listing hexId (Description category name manufacturer price color)



-- Getters for encapsulated/stringified ID and product description record


idString : Listing -> String
idString (Listing id _) =
  HexId.toString id


idValue : Listing -> HexId
idValue (Listing id _) =
  id


description : Listing -> Description
description (Listing _ desc) =
  desc



-- JSON decoders


descriptionDecoder : Decode.Decoder Description
descriptionDecoder =
  Decode.map5 Description
    (Decode.field "type" Category.decoder)
    (Decode.field "name" Decode.string)
    (Decode.field "manufacturer" Decode.string)
    (Decode.field "price" Decode.int)
    (Decode.field "color" (Decode.list ColorSpec.decoder))


decoder : Decode.Decoder Listing
decoder =
  Decode.map2 create
    (Decode.field "id" HexId.decoder)
    descriptionDecoder



-- HTML viewers


view : ListingView -> Html msg
view ( prod, { stock } ) =
  let
    desc =
      description prod
  in
  li []
    [ span [] [ text (desc.name ++ " ") ]
    , span [] [ text ("by " ++ desc.manufacturer ++ " ") ]
    , span [] [ text ("/ " ++ String.fromInt desc.price ++ " EUR") ]
    , span [] [ text (" / " ++ stock) ]
    ]


viewKeyed : ListingView -> ( String, Html msg )
viewKeyed lview =
  ( idString (Tuple.first lview), lazy view lview )
