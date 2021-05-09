module Pages.Deckbuilder exposing (Model, Msg, page)

import API.Cards
import Components.Header
import EverySet
import Gen.Params.Deckbuilder exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page
import Request
import Rules.Cards as Cards
import Rules.Clans exposing (Clan(..), clanName)
import Rules.Formats as Formats
import Shared
import String
import UI.ClanFilterSelector
import Url exposing (Protocol(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { format : Formats.Format
    , stronghold : Cards.StrongholdData
    , role : Maybe Cards.RoleData
    , provinces : List Cards.ProvinceData
    , deckCards : List ( Cards.Card, Int )
    , deckName : Maybe String
    , allCardsData : Maybe (List API.Cards.Card)
    , filterClan : UI.ClanFilterSelector.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { format = Cards.sample.format
      , stronghold = Cards.sample.stronghold
      , role = Just Cards.sample.role
      , provinces = Cards.sample.provinces
      , deckCards = Cards.sample.deckCards
      , deckName = Nothing
      , allCardsData = Nothing
      , filterClan = UI.ClanFilterSelector.init
      }
    , API.Cards.fetchCards GotCards
    )



-- UPDATE


type Msg
    = GotCards (Result Http.Error (List API.Cards.Card))
    | ClanFilterChanged UI.ClanFilterSelector.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClanFilterChanged clanFilters ->
            ( { model | filterClan = clanFilters }, Cmd.none )

        GotCards result ->
            case result of
                Ok allCardsData ->
                    ( { model | allCardsData = Just allCardsData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Deckbuilder"
    , body =
        [ Components.Header.view
        , viewDeck model
        , aside []
            [ viewFilters model
            , viewCardsOptions model
            ]
        ]
    }


viewFilters : Model -> Html Msg
viewFilters model =
    div [ class "filters" ]
        [ p [] [ text "Filters" ]
        , UI.ClanFilterSelector.view model.filterClan ClanFilterChanged
        ]


viewCardsOptions : Model -> Html Msg
viewCardsOptions model =
    case model.allCardsData of
        Nothing ->
            text "loading"

        Just allCardsData ->
            let
                filteredCards =
                    List.filter (isClanAllowed model.filterClan) allCardsData
            in
            div [ class "cards" ]
                [ p [] [ text "Cards" ]
                , ul [] (List.map viewCardRow filteredCards)
                ]


viewCardRow : API.Cards.Card -> Html Msg
viewCardRow card =
    li [] [ p [] [ text card.id, span [] (List.map (text << clanName) card.allowedClans) ] ]


viewDeck : Model -> Html Msg
viewDeck model =
    let
        ( dynastyDeck, other ) =
            List.partition (Tuple.first >> Cards.hasBack Cards.Dynasty) model.deckCards

        ( conflictDeck, _ ) =
            List.partition (Tuple.first >> Cards.hasBack Cards.Conflict) other
    in
    main_ [ class "decklist", id "decklist" ]
        [ div [ class "decklist-deck_name" ] (viewDeckName model)
        , div [ class "decklist-header" ] (viewDeckHeader model)
        , div [ class "decklist-decks" ]
            [ div [ class "decklist-deck" ] (viewDeckSide dynastyDeck)
            , div [ class "decklist-deck" ] (viewDeckSide conflictDeck)
            ]
        ]


viewDeckSide : List ( Cards.Card, Int ) -> List (Html Msg)
viewDeckSide cards =
    let
        line qty card =
            Just (li [] [ text (String.fromInt qty), text card.title ])

        viewCard ( card, qty ) =
            case card of
                Cards.CardCharacter _ character ->
                    line qty character

                Cards.CardEvent _ event ->
                    line qty event

                Cards.CardHolding _ holding ->
                    line qty holding

                Cards.CardAttachment _ attachment ->
                    line qty attachment

                _ ->
                    Nothing
    in
    [ ul [] (List.filterMap viewCard cards) ]


viewDeckName : Model -> List (Html Msg)
viewDeckName model =
    case model.deckName of
        Just deckName ->
            [ h1 [ class "decklist-deck_name" ] [ text deckName ] ]

        Nothing ->
            [ h1 [ class "decklist-deck_name" ] [ text "Unnamed" ] ]


viewDeckHeader : Model -> List (Html Msg)
viewDeckHeader model =
    [ div [ class "decklist-header_stronghold" ]
        [ text model.stronghold.title ]
    , div [ class "decklist-header_details" ]
        [ h2 [] [ text model.stronghold.title ]
        , h3 [] (viewRole model)
        , ul [] (viewProvinces model)
        ]
    ]


viewRole : Model -> List (Html Msg)
viewRole model =
    case model.role of
        Just role ->
            [ text role.title ]

        Nothing ->
            [ text "" ]


viewProvinces : Model -> List (Html Msg)
viewProvinces model =
    let
        viewProvince province =
            li [] [ text province.title ]
    in
    List.map viewProvince model.provinces


isClanAllowed : EverySet.EverySet Rules.Clans.Clan -> API.Cards.Card -> Bool
isClanAllowed filter card =
    case card.clan of
        Nothing ->
            False

        Just clan ->
            UI.ClanFilterSelector.isClanAllowed filter clan
