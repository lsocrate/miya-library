module Pages.Deckbuilder exposing (Model, Msg, page)

import API.Cards
import Card
import Clan exposing (Clan(..))
import Components.Header
import EverySet
import Gen.Params.Deckbuilder exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error)
import Page
import Request
import Shared
import String
import UI.ClanFilterSelector
import Url exposing (Protocol(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Deck =
    { stronghold : Card.Stronghold
    , name : Maybe String
    , role : Maybe Card.Role
    }


type alias Filters =
    { byClan : UI.ClanFilterSelector.Model }


type Model
    = Loading
    | Error
    | ChoosingStronghold (List Card.Card)
    | Deckbuilding (List Card.Card) Deck Filters


init : ( Model, Cmd Msg )
init =
    ( Loading, API.Cards.fetchCards FetchedCards )


type Msg
    = FetchedCards (Result Http.Error (List Card.Card))
    | StrongholdSelected Card.Stronghold
    | StrongholdReset
    | ClanFilterChanged UI.ClanFilterSelector.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading, FetchedCards result ) ->
            case result of
                Ok cards ->
                    ( ChoosingStronghold cards, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )

        ( ChoosingStronghold cards, StrongholdSelected stronghold ) ->
            ( Deckbuilding cards { stronghold = stronghold, name = Nothing, role = Nothing } { byClan = UI.ClanFilterSelector.init }, Cmd.none )

        ( Deckbuilding cards _ _, StrongholdReset ) ->
            ( ChoosingStronghold cards, Cmd.none )

        ( Deckbuilding cards deck filters, ClanFilterChanged clanFilters ) ->
            ( Deckbuilding cards deck { filters | byClan = clanFilters }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> View Msg
view model =
    let
        onlyStronghold card =
            case card of
                Card.StrongholdType stronghold ->
                    Just stronghold

                _ ->
                    Nothing

        viewForStep =
            case model of
                Loading ->
                    viewLoading

                Error ->
                    viewError

                ChoosingStronghold allCards ->
                    viewStrongholdSelector <| List.filterMap onlyStronghold allCards

                Deckbuilding cards deck filters ->
                    viewDeckbuilder cards deck filters
    in
    { title = "Deckbuilder"
    , body =
        [ Components.Header.view
        , viewForStep
        ]
    }


viewError : Html Msg
viewError =
    div [] [ text "Error!" ]


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading" ]


viewStrongholdSelector : List Card.Stronghold -> Html Msg
viewStrongholdSelector strongholds =
    let
        strongholdOption sh =
            case sh of
                Card.Stronghold props ->
                    li []
                        [ div [ onClick (StrongholdSelected sh) ] [ text props.title ]
                        ]
    in
    div []
        [ h2 [] [ text "Choose a stronghold" ]
        , ul []
            (List.map strongholdOption strongholds)
        ]


viewDeckbuilder : List Card.Card -> Deck -> Filters -> Html Msg
viewDeckbuilder cards deck filters =
    div []
        [ viewDeck deck
        , aside []
            [ viewFilters filters
            , viewCardsOptions cards deck filters
            ]
        ]


viewDeck : Deck -> Html Msg
viewDeck deck =
    let
        dynastyDeck =
            []

        conflictDeck =
            []
    in
    main_ [ class "decklist", id "decklist" ]
        [ div [ class "decklist-deck_name" ] (viewDeckName deck)
        , div [ class "decklist-header" ] (viewDeckHeader deck)
        , div [ class "decklist-decks" ]
            [ div [ class "decklist-deck" ] (viewDeckSide dynastyDeck)
            , div [ class "decklist-deck" ] (viewDeckSide conflictDeck)
            ]
        ]


viewDeckName : Deck -> List (Html Msg)
viewDeckName deck =
    case deck.name of
        Just deckName ->
            [ h1 [ class "decklist-deck_name" ] [ text deckName ] ]

        Nothing ->
            [ h1 [ class "decklist-deck_name" ] [ text "Unnamed" ] ]


viewDeckHeader : Deck -> List (Html Msg)
viewDeckHeader deck =
    let
        strongholdName =
            case deck.stronghold of
                Card.Stronghold props ->
                    props.title
    in
    [ div [ class "decklist-header_stronghold" ]
        [ text <| Maybe.withDefault "" deck.name ]
    , div [ class "decklist-header_details" ]
        [ h2 []
            [ text strongholdName
            , button [ onClick StrongholdReset ] [ text "X" ]
            ]
        , h3 [] (viewRole deck)

        -- , ul [] (viewProvinces model)
        ]
    ]


viewDeckSide : List ( Card.Card, Int ) -> List (Html Msg)
viewDeckSide cards =
    let
        cardItem ( card, qty ) =
            li [] [ text (String.fromInt qty), text (Card.title card) ]
    in
    [ ul [] (List.map cardItem cards) ]


viewFilters : Filters -> Html Msg
viewFilters filters =
    div [ class "filters" ]
        [ p [] [ text "Filters" ]
        , UI.ClanFilterSelector.view filters.byClan ClanFilterChanged
        ]


viewCardsOptions : List Card.Card -> Deck -> Filters -> Html Msg
viewCardsOptions cards _ filters =
    let
        filteredCards =
            List.filter (isClanAllowed filters.byClan) cards
    in
    div [ class "cards" ]
        [ p [] [ text "Cards" ]
        , ul [] (List.map viewCardRow filteredCards)
        ]


isClanAllowed : EverySet.EverySet Clan.Clan -> Card.Card -> Bool
isClanAllowed filter _ =
    let
        clan =
            Clan.Crab
    in
    UI.ClanFilterSelector.isClanAllowed filter clan


viewCardRow : Card.Card -> Html Msg
viewCardRow card =
    li []
        [ p []
            [ text (Card.title card)
            ]
        ]


viewRole : Deck -> List (Html Msg)
viewRole deck =
    case deck.role of
        Just role ->
            case role of
                Card.Role props ->
                    [ text props.title ]

        Nothing ->
            [ text "No Role" ]
