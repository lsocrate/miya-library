module Pages.Deck.Id_ exposing (Model, Msg, page)

import API.Deck
import Card
import Dict
import Gen.Params.Deck.Id_ exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe
import Page
import Request
import Shared
import UI.Decklist
import UI.Page
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params
        , update = update
        , view = view shared req.route
        , subscriptions = subscriptions
        }



-- INIT


type Model
    = Loading
    | Error
    | Decklist (List ( String, Int ))


init : Params -> ( Model, Cmd Msg )
init params =
    ( Loading, API.Deck.fetchDeck FetchedDecklist params.id )



-- UPDATE


type Msg
    = FetchedDecklist API.Deck.FetchResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FetchedDecklist result, Loading ) ->
            case result of
                Ok decklist ->
                    ( Decklist decklist, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Route -> Model -> View Msg
view shared route model =
    let
        content =
            case ( shared, model ) of
                ( _, Loading ) ->
                    List.singleton viewLoading

                ( Shared.Loading, _ ) ->
                    List.singleton viewLoading

                ( _, Error ) ->
                    List.singleton viewError

                ( Shared.Error, _ ) ->
                    List.singleton viewError

                ( Shared.Loaded { cards }, Decklist decklist ) ->
                    [ div [ class "main-column--1_of_2" ] [ UI.Decklist.view <| decklistToDeck cards decklist ]
                    , div [ class "main-column--1_of_2" ] []
                    ]
    in
    UI.Page.view route content


viewError : Html Msg
viewError =
    div [] [ text "Error!" ]


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading" ]


decklistToDeck : Dict.Dict String Card.Card -> List ( String, Int ) -> UI.Decklist.Model
decklistToDeck cardCollection decklist =
    let
        realCards =
            List.filterMap (toCardTuple cardCollection) decklist
    in
    { cards = realCards, name = Just "Kisada's last Stand", author = "HidaAmoro" }


toCardTuple : Dict.Dict String Card.Card -> ( String, Int ) -> Maybe ( Card.Card, Int )
toCardTuple cards ( cardId, n ) =
    Dict.get cardId cards |> Maybe.map (\card -> ( card, n ))
