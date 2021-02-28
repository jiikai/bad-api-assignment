module Product.ColorSpec exposing
  ( ColorSpec
  , decoder
  , fromString, toHex, toString
  )

import Dict exposing (Dict)
import Json.Decode as Decode

import SolidColor exposing (SolidColor)
import Palette.X11 as X11



type ColorSpec
  = X11Color String SolidColor
  | UnknownColor String


-- Dict from X11 color name strings to their SolidColor representations.

colorNameToSolidColor = Dict.fromList
  [ ( "pink", X11.pink )
  , ( "lightPink", X11.lightPink )
  , ( "hotPink", X11.hotPink )
  , ( "deepPink", X11.deepPink )
  , ( "paleVioletRed", X11.paleVioletRed )
  , ( "mediumVioletRed", X11.mediumVioletRed )      
  , ( "lightSalmon", X11.lightSalmon )
  , ( "salmon", X11.salmon )
  , ( "darkSalmon", X11.darkSalmon )
  , ( "lightCoral", X11.lightCoral )
  , ( "indianRed", X11.indianRed )
  , ( "crimson", X11.crimson )
  , ( "firebrick", X11.firebrick )
  , ( "darkRed", X11.darkRed )
  , ( "red", X11.red )
  , ( "orangeRed", X11.orangeRed )
  , ( "tomato", X11.tomato )
  , ( "coral", X11.coral )
  , ( "darkOrange", X11.darkOrange )
  , ( "orange", X11.orange )      
  , ( "yellow", X11.yellow )
  , ( "lightYellow", X11.lightYellow )
  , ( "lemonChiffon", X11.lemonChiffon )
  , ( "lightGoldenrodYellow", X11.lightGoldenrodYellow )
  , ( "papayaWhip", X11.papayaWhip )
  , ( "moccasin", X11.moccasin )
  , ( "peachPuff", X11.peachPuff )
  , ( "paleGoldenrod", X11.paleGoldenrod )
  , ( "khaki", X11.khaki )
  , ( "darkKhaki", X11.darkKhaki )
  , ( "gold", X11.gold )      
  , ( "cornsilk", X11.cornsilk )
  , ( "blanchedAlmond", X11.blanchedAlmond )
  , ( "bisque", X11.bisque )
  , ( "navajoWhite", X11.navajoWhite )
  , ( "wheat", X11.wheat )
  , ( "burlywood", X11.burlywood )
  , ( "tan", X11.tan )
  , ( "rosyBrown", X11.rosyBrown )
  , ( "sandyBrown", X11.sandyBrown )
  , ( "goldenrod", X11.goldenrod )
  , ( "darkGoldenrod", X11.darkGoldenrod )
  , ( "peru", X11.peru )
  , ( "chocolate", X11.chocolate )
  , ( "saddleBrown", X11.saddleBrown )
  , ( "sienna", X11.sienna )
  , ( "brown", X11.brown )
  , ( "maroon", X11.maroon )      
  , ( "darkOliveGreen", X11.darkOliveGreen )
  , ( "olive", X11.olive )
  , ( "oliveDrab", X11.oliveDrab )
  , ( "yellowGreen", X11.yellowGreen )
  , ( "limeGreen", X11.limeGreen )
  , ( "lime", X11.lime )
  , ( "lawnGreen", X11.lawnGreen )
  , ( "chartreuse", X11.chartreuse )
  , ( "greenYellow", X11.greenYellow )
  , ( "springGreen", X11.springGreen )
  , ( "mediumSpringGreen", X11.mediumSpringGreen )
  , ( "lightGreen", X11.lightGreen )
  , ( "paleGreen", X11.paleGreen )
  , ( "darkSeaGreen", X11.darkSeaGreen )
  , ( "mediumAquamarine", X11.mediumAquamarine )
  , ( "mediumSeaGreen", X11.mediumSeaGreen )
  , ( "seaGreen", X11.seaGreen )
  , ( "forestGreen", X11.forestGreen )
  , ( "green", X11.green )
  , ( "darkGreen", X11.darkGreen )      
  , ( "aqua", X11.aqua )
  , ( "cyan", X11.cyan )
  , ( "lightCyan", X11.lightCyan )
  , ( "paleTurquoise", X11.paleTurquoise )
  , ( "aquamarine", X11.aquamarine )
  , ( "turquoise", X11.turquoise )
  , ( "mediumTurquoise", X11.mediumTurquoise )
  , ( "darkTurquoise", X11.darkTurquoise )
  , ( "lightSeaGreen", X11.lightSeaGreen )
  , ( "cadetBlue", X11.cadetBlue )
  , ( "darkCyan", X11.darkCyan )
  , ( "teal", X11.teal )      
  , ( "lightSteelBlue", X11.lightSteelBlue )
  , ( "powderBlue", X11.powderBlue )
  , ( "lightBlue", X11.lightBlue )
  , ( "skyBlue", X11.skyBlue )
  , ( "lightSkyBlue", X11.lightSkyBlue )
  , ( "deepSkyBlue", X11.deepSkyBlue )
  , ( "dodgerBlue", X11.dodgerBlue )
  , ( "cornflowerBlue", X11.cornflowerBlue )
  , ( "steelBlue", X11.steelBlue )
  , ( "royalBlue", X11.royalBlue )
  , ( "blue", X11.blue )
  , ( "mediumBlue", X11.mediumBlue )
  , ( "darkBlue", X11.darkBlue )
  , ( "navy", X11.navy )
  , ( "midnightBlue", X11.midnightBlue )      
  , ( "lavender", X11.lavender )
  , ( "thistle", X11.thistle )
  , ( "plum", X11.plum )
  , ( "violet", X11.violet )
  , ( "orchid", X11.orchid )
  , ( "fuchsia", X11.fuchsia )
  , ( "magenta", X11.magenta )
  , ( "mediumOrchid", X11.mediumOrchid )
  , ( "mediumPurple", X11.mediumPurple )
  , ( "blueViolet", X11.blueViolet )
  , ( "darkViolet", X11.darkViolet )
  , ( "darkOrchid", X11.darkOrchid )
  , ( "darkMagenta", X11.darkMagenta )
  , ( "purple", X11.purple )
  , ( "indigo", X11.indigo )
  , ( "darkSlateBlue", X11.darkSlateBlue )
  , ( "slateBlue", X11.slateBlue )
  , ( "mediumSlateBlue", X11.mediumSlateBlue )     
  , ( "white", X11.white )
  , ( "snow", X11.snow )
  , ( "honeydew", X11.honeydew )
  , ( "mintCream", X11.mintCream )
  , ( "azure", X11.azure )
  , ( "aliceBlue", X11.aliceBlue )
  , ( "ghostWhite", X11.ghostWhite )
  , ( "whiteSmoke", X11.whiteSmoke )
  , ( "seashell", X11.seashell )
  , ( "beige", X11.beige )
  , ( "oldLace", X11.oldLace )
  , ( "floralWhite", X11.floralWhite )
  , ( "ivory", X11.ivory )
  , ( "antiqueWhite", X11.antiqueWhite )
  , ( "linen", X11.linen )
  , ( "lavenderBlush", X11.lavenderBlush )
  , ( "mistyRose", X11.mistyRose )      
  , ( "gainsboro", X11.gainsboro )
  , ( "lightGray", X11.lightGray )
  , ( "silver", X11.silver )
  , ( "darkGray", X11.darkGray )
  , ( "gray", X11.gray )
  , ( "dimGray", X11.dimGray )
  , ( "lightSlateGray", X11.lightSlateGray )
  , ( "slateGray", X11.slateGray )
  , ( "darkSlateGray", X11.darkSlateGray )
  , ( "black", X11.black )
  ]


decoder : Decode.Decoder ColorSpec
decoder =
  Decode.string 
    |> Decode.andThen (\color -> Decode.succeed (fromString color))


fromString : String -> ColorSpec 
fromString str =
  case (Dict.get str colorNameToSolidColor) of
    Just solidColor ->
      X11Color str solidColor 
    Nothing ->
      UnknownColor str


toHex : ColorSpec -> Maybe String
toHex colorSpec =
  case colorSpec of
    X11Color _ solidColor ->
      Just (SolidColor.toHex solidColor)
    UnknownColor _ ->
      Nothing


toString : ColorSpec -> String
toString colorSpec =
  case colorSpec of
    X11Color str _ ->
      str
    UnknownColor str ->
      str
