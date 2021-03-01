module Route exposing (Route(..), fromString, fromUrl)

import HexId
import Product.Category as Category
import Url exposing (Url)
import Url.Parser
  exposing
    ( (</>)
    , Parser
    , map
    , oneOf
    , parse
    , s
    , top
    )


type Route
  = Products Category.Category
  | Home
  | About
  | InvalidUrl
  | NotFound


parser : Parser (Route -> a) a
parser =
  oneOf
    [ map Products <|
        (s "products" </> Category.parser)
    , map About (s "about")
    , map Home (oneOf [ s "home", s "", s "Main.elm" ])
    ]


fromUrl : Url -> Route
fromUrl url =
  case parse parser url of
    Nothing ->
      NotFound

    Just route ->
      case route of
        Products category ->
          if category == Category.other then
            NotFound

          else
            route

        _ ->
          route


fromString : String -> Route
fromString str =
  case Url.fromString str of
    Nothing ->
      InvalidUrl

    Just url ->
      fromUrl url
