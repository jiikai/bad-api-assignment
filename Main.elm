module Main exposing (..)

import Array.NonEmpty as Nea exposing (NonEmptyArray)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
--import Process
--import Task
import Time
import Tuple2 as Tuple
import Url
import Url.Builder as Builder


import SolidColor
import Palette.X11 exposing (..)

import HexId exposing (HexId)
import Route
import Product.Category as Category exposing (Category)
import Product.ColorSpec as ColorSpec exposing (ColorSpec)
import Product.Listing as Listing exposing (Listing)
import Availability exposing (Availability)



-- MAIN


main : Program Encode.Value Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Nea a = NonEmptyArray a


type ETag = 
  ETag String


etagEmpty : ETag
etagEmpty =
  ETag ""


etagFromString : String -> ETag
etagFromString str =
  ETag str


etagFromLoadState : LoadState -> ETag
etagFromLoadState loadState =
  case loadState of
    Success etag ->
      etag
    Updating etag ->
      etag
    FailedUpdating etag _ ->
      etag
    _ ->
      etagEmpty

etagIfNoneMatch : ETag -> Http.Header
etagIfNoneMatch (ETag str) =
  Http.header "If-None-Match" str


type ContentChanged
  = NoChange
  | Changed ETag
 

type alias ListingViewData =
  { visibility : Bool
  , stock : String
  }

type alias NavState =
  { key : Nav.Key
  , url : Url.Url
  , route : Route.Route
  }


type PeriodicUpdate
  = ForProducts
  | ForStock


type alias StoreSection =
  ( LoadState, List Listing )


type alias StockSection =
  ( LoadState, Availability )


type LoadState
  = Initializing
  | FailedInitializing String
  | Success ETag
  | Updating ETag
  | FailedUpdating ETag String


type alias Flags =
  { protocol : String
  , rootUrl : String
  , availabilityUrl : String
  , productsUrl : String
  }

type alias Model =
  { navi : NavState
  , store : Nea StoreSection
  , stock : Dict String StockSection
  , flags : Flags
  }


type alias GotProductsResult =
  Result ( Category, Http.Error ) ( Category, ContentChanged, List Listing )


type alias GotStockResult = 
  Result ( String, Http.Error ) ( String, ContentChanged, Availability )


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotProducts GotProductsResult
  | GotStock GotStockResult
  | DoUpdate PeriodicUpdate 


-- Extract manufacturer names out of product listings, returning a list of new names
newManufacturers : Dict String StockSection -> StoreSection -> ( Dict String StockSection, List String )
newManufacturers stock_ ( _, sect ) =
  List.foldl (\listing ( stock, names ) ->
    let
      manufacturer = Listing.description listing |> .manufacturer
    in
      if (Dict.member manufacturer stock) then
        ( stock, names )
      else
        ( Dict.insert manufacturer ( Initializing, Availability.empty ) stock
        , manufacturer :: names 
        )
  ) ( stock_, [ ] ) sect


getStockSection : String -> Dict String StockSection -> Maybe StockSection
getStockSection manufacturer stock =
  Dict.get manufacturer stock


getStockSectionLoadState : String -> Dict String StockSection -> Maybe LoadState
getStockSectionLoadState manufacturer stock =
  Dict.get manufacturer stock |> Maybe.map Tuple.first


setStockSection : String -> StockSection -> Dict String StockSection -> Dict String StockSection
setStockSection manufacturer sect stock =
  Dict.insert manufacturer sect stock


getStoreSection : Category -> Nea StoreSection -> StoreSection
getStoreSection category store =
  Nea.setSelectedIndex (Category.toInt category) store
    |> Nea.getSelected


getStoreSectionLoadState : Category -> Nea StoreSection -> LoadState
getStoreSectionLoadState category store =
  getStoreSection category store |> Tuple.first


setStoreSection : Category -> StoreSection -> Nea StoreSection -> Nea StoreSection
setStoreSection category updatedSection store =
  Nea.set (Category.toInt category) updatedSection store


loadStateUpdateSuccess : LoadState -> LoadState
loadStateUpdateSuccess state =
  case state of
    Updating etag ->
      Success etag
    _ ->
      state


initStore : Nea StoreSection
initStore =
  Nea.repeat Category.count ( Initializing, [ ] )
    |> Nea.set 0 ( Success etagEmpty, [ ] )


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init encodedFlags url key =
  let
    flags = Decode.decodeValue flagsDecoder encodedFlags
      |> Result.toMaybe |> Maybe.withDefault 
        { protocol = "https://"
        , rootUrl = "bad-api-assignment.reaktor.com/v2/"
        , availabilityUrl = "availability/"
        , productsUrl = "products/"
        }
  in
    ( (Model (NavState key url (Route.fromUrl url)) initStore Dict.empty flags)
    , Category.listOf
      |> List.drop 1
      |> List.map (\category -> getProductsJson flags Nothing category)
      |> Cmd.batch
    )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        {-  We are staying within our application.
            Results in an `onUrlChange`, triggering `UrlChanged`,
            handled in the branch below. 
        -}
        Browser.Internal url ->
          ( model, Nav.pushUrl model.navi.key (Url.toString url) )

        {-  User is going someplace else. -}
        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      let 
        updatedModel = 
          { model | navi = (NavState model.navi.key url (Route.fromUrl url)) }
      in
        case updatedModel.navi.route of 
          Route.Products category ->
            let 
              ( loadState, products) = updatedModel.store
                |> getStoreSection category
            in
              case loadState of
                Initializing ->
                  ( updatedModel, Cmd.none)
                Updating _ ->
                  ( updatedModel, Cmd.none )
                FailedInitializing _ ->
                  ( updatedModel, getProductsJson model.flags Nothing category )
                FailedUpdating etag _ ->
                  ( updatedModel, getProductsJson model.flags (Just etag) category )
                Success _ -> 
                  ( updatedModel, Cmd.none )
          _ ->
            ( updatedModel, Cmd.none )
    
    {-  Incoming json. -}
    GotProducts res ->
      case res of
        Ok ( category, change, newProducts ) ->
          let
            sect = getStoreSection category model.store
          in
            case change of
              NoChange ->
                ( { model 
                  | store = model.store 
                    |> (setStoreSection category 
                      <| Tuple.mapFirst loadStateUpdateSuccess sect)
                  }
                , Cmd.none
                )
              Changed etag ->
                let
                  newSect = ( Success etag, newProducts )
                  ( newStock, fetchTheseManufacturers )
                    = newManufacturers model.stock newSect
                in
                  ( { model 
                    | stock = newStock
                    , store = model.store
                      |> setStoreSection category newSect
                    }
                  , fetchTheseManufacturers
                    |> List.map (\manufacturer -> 
                      getStockJson model.flags Nothing manufacturer)
                    |> Cmd.batch
                )
        Err ( category, err ) ->
          let
            ( loadState, oldProducts ) = getStoreSection category model.store
            newLoadState =
              case loadState of
                Initializing ->
                  FailedInitializing (httpErrorToString err)
                Updating etag ->
                  FailedUpdating etag (httpErrorToString err)
                _ ->
                  loadState
          in
            ( { model | store = model.store
                |> setStoreSection category ( loadState, oldProducts ) } 
            , Cmd.none
            )
    GotStock res ->
      case res of
        Ok ( manufacturer, change, newStock ) ->
          let
            sect = getStockSection manufacturer model.stock
              |> Maybe.withDefault ( Initializing, Availability.empty )
          in
            case change of
              NoChange ->
                ( { model | stock = model.stock
                    |> setStockSection manufacturer (Tuple.mapFirst loadStateUpdateSuccess sect)
                  }
                , Cmd.none
                )
              Changed etag ->
                ( { model | stock = model.stock
                    |> setStockSection manufacturer 
                      ( Success etag
                      , newStock
                      )
                  }
                , Cmd.none
                )
        Err ( manufacturer, err ) ->
          case err of
            (Http.BadBody "availability-empty") ->
              let
                etag = getStockSectionLoadState manufacturer model.stock
                  |> Maybe.map etagFromLoadState 
                  |> Maybe.withDefault etagEmpty
              in
                ( model
                , getStockJson model.flags (Just etag) manufacturer
                )
            _ ->
              let
                ( loadState, oldStock ) = getStockSection manufacturer model.stock
                  |> Maybe.withDefault ( Initializing, Availability.empty )
                newLoadState =
                  case loadState of
                    Initializing ->
                      FailedInitializing (httpErrorToString err)
                    Updating etag ->
                      FailedUpdating etag (httpErrorToString err)
                    _ ->
                      loadState
              in
                ( { model | stock = model.stock
                    |> setStockSection manufacturer ( loadState, oldStock ) } 
                , Cmd.none
                )


    DoUpdate for ->
      case for of
        ForProducts ->
          ( model
          , Category.listOf |> List.drop 1 |> List.filterMap (\category ->
              case (getStoreSectionLoadState category model.store) of
                Success etag ->
                  Just (getProductsJson model.flags (Just etag) category)
                FailedUpdating etag _ ->
                  Just (getProductsJson model.flags (Just etag) category)
                _ ->
                  Nothing)
              |> Cmd.batch
          )
        ForStock ->
          ( model
          , (Dict.foldr (\manufacturer ( loadState, sect ) acc ->
            case loadState of
              Success etag ->
                (getStockJson model.flags (Just etag) manufacturer) :: acc
              FailedUpdating etag _ ->
                (getStockJson model.flags (Just etag) manufacturer) :: acc
              _ ->
                acc) [ ] model.stock)
              |> Cmd.batch
          )


-- SUB

subscriptions : Model -> Sub Msg
subscriptions model =
  -- prime intervals of a little over five and seven minutes respectively
    [ Time.every 300589 (\_ -> DoUpdate ForProducts)
    , Time.every 420613 (\_ -> DoUpdate ForStock)
    ] |> Sub.batch



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    viewContent =
      case model.navi.route of
        Route.Products category ->
          let 
            ( loadState, products ) = getStoreSection category model.store
          in
            case loadState of
              Initializing ->
                viewInfoMsg "Initializing application ..."
              FailedInitializing err ->
                viewErrorMsg err
              Success _ ->
                viewProducts model.stock products
              Updating _ ->
                viewInfoMsg "Updating catalogue ..."
              FailedUpdating _ err ->
                viewErrorMsg err
        Route.Home ->
          text "CLOTHING STORE – Browse the store by clicking the above links!"
        Route.About ->
          text "This is a clothing store!"
        Route.InvalidUrl ->
          viewErrorMsg "Invalid Url!"
        Route.NotFound ->
          viewErrorMsg "Not Found!"
  in
    { title = "Clothing Store"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.navi.url) ]
        , ul []
          <| (++)
            [ viewLink "/home" "Home"
            , viewLink "/about" "About"
            ]
            [ viewLink "/products/beanies" "Beanies"
            , viewLink "/products/facemasks" "Facemasks"
            , viewLink "/products/gloves" "Gloves"
            ]
        , (viewContent)
        --, text (Debug.toString model.stock)
        ]
    }


viewInfoMsg : String -> Html msg
viewInfoMsg msg =
  pre [] [ text (msg) ]


viewErrorMsg : String -> Html msg
viewErrorMsg msg =
  pre [] [ text (msg) ]


viewLink : String -> String -> Html msg
viewLink path display =
  li [] [ a [ href path ] [ text display ] ]


viewProducts : Dict String StockSection -> List Listing -> Html msg
viewProducts stock products =
  List.map (\listing ->
    let
      manufacturer = Listing.description listing |> .manufacturer
      viewData = getStockSection manufacturer stock
        |> Maybe.map Tuple.second
        |> Maybe.map (Availability.get (Listing.idValue listing))
        |> Maybe.withDefault Availability.Unknown
        |> Availability.toBoolAndString
        |> Tuple.uncurry ListingViewData
    in
      Listing.viewKeyed ( listing, viewData )) products |> Keyed.ul [ ]



-- HTTP


expectProductsJson : Category -> (GotProductsResult -> msg) -> Http.Expect msg
expectProductsJson category toMsg =
  Http.expectStringResponse toMsg <|
    \res ->
      case res of
        Http.BadUrl_ url ->
          Err ( category, (Http.BadUrl url) )

        Http.Timeout_ ->
          Err ( category, Http.Timeout )

        Http.NetworkError_ ->
          Err ( category, Http.NetworkError )

        Http.BadStatus_ metadata body ->
          case metadata.statusCode of
            304 ->
              Ok ( category, NoChange, [ ] )
            _ ->
              Err ( category, (Http.BadStatus metadata.statusCode) )

        Http.GoodStatus_ metadata body ->
          case (Decode.decodeString (Decode.list Listing.decoder) body) of
            Ok value ->
              let
                etag = Dict.get "etag" metadata.headers
                  |> Maybe.withDefault ""
              in
                Ok ( category, Changed (etagFromString etag), value )
            Err err ->
               Err ( category, Http.BadBody (Decode.errorToString err) )


expectStockJson : String -> (GotStockResult -> msg) -> Http.Expect msg
expectStockJson manufacturer toMsg =
  Http.expectStringResponse toMsg <|
    \res ->
      case res of
        Http.BadUrl_ url ->
          Err ( manufacturer, (Http.BadUrl url) )

        Http.Timeout_ ->
          Err ( manufacturer, Http.Timeout )

        Http.NetworkError_ ->
          Err ( manufacturer, Http.NetworkError )

        Http.BadStatus_ metadata body ->
          case metadata.statusCode of
            304 ->
              Ok ( manufacturer, NoChange, Availability.empty )
            _ ->
              Err ( manufacturer, (Http.BadStatus metadata.statusCode) )

        Http.GoodStatus_ metadata body ->
          -- Deal with the "availability-empty" error
          
          let
            errorModesActive = Dict.get "x-error-modes-active" metadata.headers
              |> Maybe.withDefault ""
          in
              if errorModesActive == "availability-empty" then
                Err ( manufacturer, Http.BadBody ("availability-empty") )
              else
                case (Decode.decodeString Availability.decoder body) of
                  Ok value ->
                    let
                      etag = Dict.get "etag" metadata.headers
                        |> Maybe.withDefault ""
                    in
                      Ok ( manufacturer, Changed (etagFromString etag), value )
                  Err err ->
                    let
                      strError = Decode.errorToString err
                    in
                     Err ( manufacturer, Http.BadBody strError )


ifNoneMatchReq : Flags -> Maybe ETag -> String -> Http.Expect Msg -> Cmd Msg
ifNoneMatchReq { protocol, rootUrl } etag urlString expectFunction =
  let
    ifNoneMatch = etagIfNoneMatch (Maybe.withDefault etagEmpty etag)
    -- we use a dirty proxy hack to get around the remote API'S lack of CORS headers
    corsProxy = "https://afternoon-mountain-60459.herokuapp.com/"
  in
    Http.request
      { method = "GET"
      , headers = [ ifNoneMatch ]
      , url = corsProxy ++ protocol ++ rootUrl ++ urlString
      , body = Http.emptyBody
      , expect = expectFunction
      , timeout = Nothing
      , tracker = Nothing
      }


getProductsJson : Flags -> Maybe ETag -> Category -> Cmd Msg
getProductsJson flags etag category =
  let
    urlStr = 
      flags.productsUrl ++ (Category.toString category)
  in
    ifNoneMatchReq flags etag urlStr (expectProductsJson category GotProducts)


getStockJson : Flags -> Maybe ETag -> String -> Cmd Msg
getStockJson flags etag manufacturer =
  let
    urlStr =
      flags.availabilityUrl ++ manufacturer
  in
    ifNoneMatchReq flags etag urlStr (expectStockJson manufacturer GotStock)



httpErrorToString : Http.Error -> String
httpErrorToString err =
  case err of
    Http.BadUrl str ->
      """Bad URL:
      """ ++ str
    Http.Timeout ->
      -- TODO implement a logic for trying again after some time
      """It took to long to get a response from the server. 
      Try coming back after a while!
      """
    Http.NetworkError ->
      """There would seem to be a problem with your internet connection.
      Could you be behind a firewall? Or spelunking?
      """
    Http.BadStatus code ->
      -- TODO implement logic for responding to various codes
      "Response status " ++ (String.fromInt code) ++ " indicates an error."
    Http.BadBody str ->
      """Failed parsing the message body:
      """ ++ str

flagsDecoder : Decoder Flags
flagsDecoder =
  Decode.map4 Flags
    (Decode.field "protocol" Decode.string)
    (Decode.field "rootUrl" Decode.string)
    (Decode.field "productsUrl" Decode.string)
    (Decode.field "availabilityUrl" Decode.string)
