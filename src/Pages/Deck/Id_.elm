module Pages.Deck.Id_ exposing (Model, Msg, page)

import API.Deck
import Deck
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
                    [ viewLoading ]

                ( Shared.Loading, _ ) ->
                    [ viewLoading ]

                ( _, Error ) ->
                    [ viewError ]

                ( Shared.Error, _ ) ->
                    [ viewError ]

                ( Shared.Loaded { cards }, Decklist decklist ) ->
                    [ main_ [ class "deckview-decklist" ]
                        [ decklistToDeck cards decklist |> Maybe.map (UI.Decklist.view Nothing) |> Maybe.withDefault (div [] []) ]
                    , aside [ class "deckview-side" ]
                        []
                    ]
    in
    UI.Page.view route content


viewError : Html Msg
viewError =
    div [] [ text "Error!" ]


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading" ]


decklistToDeck : Shared.CardCollection -> Deck.Decklist -> Maybe UI.Decklist.Model
decklistToDeck cardCollection decklist =
    Deck.fromDecklist cardCollection decklist
        |> Maybe.map
            (\deck ->
                { name = Just "Kisada's last Stand"
                , author = "HidaAmoro"
                , deck = deck
                , editingName = False
                }
            )
