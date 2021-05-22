module Shared exposing
    ( Flags
    , Model(..)
    , Msg
    , init
    , subscriptions
    , update
    )

import API.Cards
import Card
import Dict exposing (Dict)
import Http
import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    Json.Value


type Model
    = Loading
    | Loaded { cards : Dict String Card.Card, user : Maybe String }
    | Error


type Msg
    = FetchedCards (Result Http.Error (Dict String Card.Card))


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( Loading, API.Cards.fetchCards FetchedCards )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case ( msg, model ) of
        ( FetchedCards result, Loading ) ->
            case result of
                Ok cards ->
                    ( Loaded { cards = cards, user = Nothing }, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
