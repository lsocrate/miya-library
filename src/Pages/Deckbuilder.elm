module Pages.Deckbuilder exposing (Model, Msg, page)

import API.Cards
import Cards as Cards exposing (Clan(..), clanName)
import Components.Header
import EverySet
import Gen.Params.Deckbuilder exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
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


type alias Deck =
    { stronghold : Cards.Card

    -- , format : Formats.Format
    -- , role : Maybe Cards.RoleData
    -- , provinces : List Cards.ProvinceData
    -- , deckCards : List ( Cards.Card, Int )
    -- , deckName : Maybe String
    }


type alias Filters =
    { byClan : UI.ClanFilterSelector.Model }


type Model
    = Loading
    | ChoosingStronghold CardsData
    | Deckbuilding CardsData Filters Deck


init : ( Model, Cmd Msg )
init =
    ( Loading, API.Cards.fetchCards FetchedCards )



-- UPDATE


type Msg
    = FetchedCards (Result Http.Error (List API.Cards.Card))
    | StrongholdChosen API.Cards.Card
    | ClanFilterChanged UI.ClanFilterSelector.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading, FetchedCards result ) ->
            case result of
                Ok allCardsData ->
                    ( ChoosingStronghold allCardsData, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ChoosingStronghold allCards, StrongholdChosen sh ) ->
            ( Deckbuilding allCards { byClan = UI.ClanFilterSelector.init } { stronghold = sh }, Cmd.none )

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
        viewForStep =
            case model of
                Loading ->
                    viewLoading

                ChoosingStronghold allCards ->
                    viewStrongholdSelector <| List.filter isStronghold allCards
    in
    { title = "Deckbuilder"
    , body =
        [ Components.Header.view
        , viewForStep
        ]
    }


isStronghold : API.Cards.Card -> Bool
isStronghold card =
    case card.cardType of
        Just cardType ->
            cardType == Cards.Stronghold

        _ ->
            False


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading" ]


viewStrongholdSelector : CardsData -> Html Msg
viewStrongholdSelector strongholds =
    let
        viewStrongholdOption sh =
            li [] [ div [ onClick (StrongholdChosen sh) ] [ text sh.name ] ]
    in
    div []
        [ h2 [] [ text "Choose a stronghold" ]
        , ul [] (List.map viewStrongholdOption strongholds)
        ]


viewDeckbuild : Model -> Html Msg
viewDeckbuild model =
    div []
        [ viewDeck model
        , aside []
            [ viewFilters model
            , viewCardsOptions model
            ]
        ]


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
