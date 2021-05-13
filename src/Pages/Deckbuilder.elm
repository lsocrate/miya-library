module Pages.Deckbuilder exposing (Model, Msg, page)

import API.Cards
import Card
import Clan exposing (Clan(..), clanName)
import Components.Header
import Debug exposing (toString)
import EverySet
import Gen.Params.Deckbuilder exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error)
import Json.Decode exposing (errorToString)
import Page
import Request
import Shared
import String
import UI.ClanFilterSelector
import Url exposing (Protocol(..))
import View exposing (View)



-- MODEL


type alias Deck =
    { stronghold : Card.Stronghold
    , name : Maybe String
    }


type alias Filters =
    { byClan : UI.ClanFilterSelector.Model }


type Model
    = Loading
    | ChoosingStronghold (List Card.Card)
    | Deckbuilding (List Card.Card) Filters Deck


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Loading, API.Cards.fetchCards FetchedCards )



-- UPDATE


type Msg
    = FetchedCards (Result Http.Error (List Card.Card))
    | StrongholdChosen Card.Stronghold
    | ClanFilterChanged UI.ClanFilterSelector.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading, FetchedCards result ) ->
            case result of
                Ok cards ->
                    ( ChoosingStronghold cards, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ChoosingStronghold cards, StrongholdChosen stronghold ) ->
            ( Deckbuilding cards { byClan = UI.ClanFilterSelector.init } { stronghold = stronghold, name = Nothing }, Cmd.none )

        -- ClanFilterChanged clanFilters ->
        --     ( { model | filterClan = clanFilters }, Cmd.none )
        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


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

                ChoosingStronghold allCards ->
                    viewStrongholdSelector <| List.filterMap onlyStronghold allCards

                Deckbuilding cards filters deck ->
                    viewDeckbuilder cards filters deck
    in
    { title = "Deckbuilder"
    , body =
        [ Components.Header.view
        , viewForStep
        ]
    }


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
                        [ div [ onClick (StrongholdChosen sh) ] [ text props.title ]
                        ]
    in
    div []
        [ h2 [] [ text "Choose a stronghold" ]
        , ul []
            (List.map strongholdOption strongholds)
        ]


viewDeckbuilder : List Card.Card -> Filters -> Deck -> Html Msg
viewDeckbuilder cards filters deck =
    div []
        [ viewDeck deck

        -- , aside []
        --     [ viewFilters model
        --     , viewCardOptions model
        --     ]
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
        [ h2 [] [ text strongholdName ]

        -- , h3 [] (viewRole model)
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



-- viewFilters : Model -> Html Msg
-- viewFilters model =
--     div [ class "filters" ]
--         [ p [] [ text "Filters" ]
--         , UI.ClanFilterSelector.view model.filterClan ClanFilterChanged
--         ]
-- viewCardsOptions : Model -> Html Msg
-- viewCardsOptions model =
--     case model.allCardsData of
--         Nothing ->
--             text "loading"
--         Just allCardsData ->
--             let
--                 filteredCards =
--                     List.filter (isClanAllowed model.filterClan) allCardData
--             in
--             div [ class "cards" ]
--                 [ p [] [ text "Cards" ]
--                 , ul [] (List.map viewCardRow filteredCards)
--                 ]
-- viewCardRow : API.Cards.Card -> Html Msg
-- viewCardRow card =
--     li [] [ p [] [ text card.id, span [] (List.map (text << clanName) card.allowedClans) ] ]
-- viewRole : Model -> List (Html Msg)
-- viewRole model =
--     case model.role of
--         Just role ->
--             [ text role.title ]
--         Nothing ->
--             [ text "" ]
-- viewProvinces : Model -> List (Html Msg)
-- viewProvinces model =
--     let
--         viewProvince province =
--             li [] [ text province.title ]
--     in
--     List.map viewProvince model.provinces
-- isClanAllowed : EverySet.EverySet Rules.Clans.Clan -> API.Cards.Card -> Bool
-- isClanAllowed filter card =
--     case card.clan of
--         Nothing ->
--             False
--         Just clan ->
--             UI.ClanFilterSelector.isClanAllowed filter clan
