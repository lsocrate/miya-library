module Pages.Deckbuilder exposing (Model, Msg, page)

import API.Cards
import Card
import Clan exposing (Clan(..))
import EverySet
import Gen.Params.Deckbuilder exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Http exposing (Error)
import Page
import Request
import Result exposing (fromMaybe)
import Shared
import UI.ClanFilterSelector
import UI.Page
import Url exposing (Protocol(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ req =
    Page.element
        { init = init
        , update = update
        , view = view req.route
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Deck =
    { stronghold : Card.Stronghold
    , name : Maybe String
    , role : Maybe Card.Role
    , cards : List ( Card.Card, Int )
    }


type alias Filters =
    { byClan : UI.ClanFilterSelector.Model }


type Model
    = Loading
    | Error
    | ChoosingStronghold { allCards : List Card.Card, oldDeck : Maybe Deck }
    | Deckbuilding { allCards : List Card.Card, deck : Deck, filters : Filters }


init : ( Model, Cmd Msg )
init =
    ( Loading, API.Cards.fetchCards FetchedCards )


type Msg
    = FetchedCards (Result Http.Error (List Card.Card))
    | StrongholdSelected Card.Stronghold
    | StrongholdReset
    | ClanFilterChanged UI.ClanFilterSelector.Model
    | CardCountInDeckChange Card.Card Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FetchedCards result, Loading ) ->
            case result of
                Ok cards ->
                    ( ChoosingStronghold { allCards = cards, oldDeck = Nothing }, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )

        ( StrongholdSelected stronghold, ChoosingStronghold { allCards, oldDeck } ) ->
            ( Deckbuilding
                { allCards = allCards
                , filters = { byClan = UI.ClanFilterSelector.init }
                , deck =
                    Maybe.withDefault
                        { stronghold = stronghold
                        , name = Nothing
                        , role = Nothing
                        , cards = []
                        }
                        oldDeck
                }
            , Cmd.none
            )

        ( StrongholdReset, Deckbuilding { allCards, deck } ) ->
            ( ChoosingStronghold { allCards = allCards, oldDeck = Just deck }, Cmd.none )

        ( ClanFilterChanged clanFilters, Deckbuilding props ) ->
            let
                { filters } =
                    props

                newFilters =
                    { filters | byClan = clanFilters }
            in
            ( Deckbuilding { props | filters = newFilters }, Cmd.none )

        ( CardCountInDeckChange card n, Deckbuilding props ) ->
            let
                { deck } =
                    props

                { cards } =
                    deck
            in
            ( Deckbuilding
                { props
                    | deck =
                        { deck
                            | cards =
                                List.filter (Tuple.first >> (/=) card) cards
                                    |> (if n > 0 then
                                            List.append [ ( card, n ) ]

                                        else
                                            identity
                                       )
                        }
                }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Route -> Model -> View Msg
view route model =
    let
        isStronghold card =
            case card of
                Card.StrongholdType stronghold ->
                    Just stronghold

                _ ->
                    Nothing

        viewsForStep =
            case model of
                Loading ->
                    [ viewLoading ]

                Error ->
                    [ viewError ]

                ChoosingStronghold { allCards } ->
                    List.singleton <| viewStrongholdSelector <| List.filterMap isStronghold allCards

                Deckbuilding { allCards, deck, filters } ->
                    List.singleton <| viewDeckbuilder allCards deck filters
    in
    UI.Page.view route viewsForStep


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
    div [ class "deckbuilder" ]
        [ viewDeck deck
        , aside []
            [ viewFilters filters
            , viewCardsOptions cards deck filters
            ]
        ]


viewDeck : Deck -> Html Msg
viewDeck deck =
    let
        { setup, dynasty, conflict } =
            List.foldl
                (\card groups ->
                    case card of
                        ( Card.StrongholdType _, _ ) ->
                            { groups | setup = groups.setup ++ [ card ] }

                        ( Card.RoleType _, _ ) ->
                            { groups | setup = groups.setup ++ [ card ] }

                        ( Card.ProvinceType _, _ ) ->
                            { groups | setup = groups.setup ++ [ card ] }

                        ( Card.HoldingType _, _ ) ->
                            { groups | dynasty = groups.dynasty ++ [ card ] }

                        ( Card.CharacterType (Card.DynastyCharacter _), _ ) ->
                            { groups | dynasty = groups.dynasty ++ [ card ] }

                        ( Card.EventType (Card.DynastyEvent _), _ ) ->
                            { groups | dynasty = groups.dynasty ++ [ card ] }

                        ( Card.CharacterType (Card.ConflictCharacter _), _ ) ->
                            { groups | conflict = groups.conflict ++ [ card ] }

                        ( Card.EventType (Card.ConflictEvent _), _ ) ->
                            { groups | conflict = groups.conflict ++ [ card ] }

                        ( Card.AttachmentType _, _ ) ->
                            { groups | conflict = groups.conflict ++ [ card ] }
                )
                { setup = [], dynasty = [], conflict = [] }
                deck.cards
    in
    main_ [ class "decklist", id "decklist" ]
        [ div [ class "decklist-deck_name" ] (viewDeckName deck)
        , div [ class "decklist-header" ] (viewDeckHeader deck)
        , div [ class "decklist-decks" ]
            [ div [ class "decklist-deck" ] (viewDeckSide dynasty)
            , div [ class "decklist-deck" ] (viewDeckSide conflict)
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
        ]
    ]


viewDeckSide : List ( Card.Card, Int ) -> List (Html Msg)
viewDeckSide cards =
    let
        cardItem ( card, qty ) =
            li [] [ text (String.fromInt qty ++ "x " ++ Card.title card) ]
    in
    [ ul [] (List.map cardItem cards) ]


viewFilters : Filters -> Html Msg
viewFilters filters =
    div [ class "filters" ]
        [ p [] [ text "Filters" ]
        , UI.ClanFilterSelector.view filters.byClan ClanFilterChanged
        ]


viewCardsOptions : List Card.Card -> Deck -> Filters -> Html Msg
viewCardsOptions cards deck filters =
    let
        filteredCards =
            List.filter (Card.clan >> UI.ClanFilterSelector.isClanAllowed filters.byClan) cards

        cardRow : Card.Card -> Html Msg
        cardRow card =
            let
                copiesInDeck =
                    List.filter (Tuple.first >> (==) card) deck.cards |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault 0
            in
            li [ class "buildercards-row" ]
                [ div [ class "buildercards-quantity_picker" ]
                    (List.range 0 3
                        |> List.map
                            (\qty ->
                                label []
                                    [ text <| String.fromInt qty
                                    , input
                                        [ type_ "radio"
                                        , name <| Card.title card
                                        , onClick <| CardCountInDeckChange card qty
                                        , classList
                                            [ ( "buildercards-quantity_option", True )
                                            , ( "buildercards-quantity_option--active", qty == copiesInDeck )
                                            ]
                                        ]
                                        []
                                    ]
                            )
                    )
                , div [ class "buildercards-card" ]
                    [ text (Card.title card)
                    ]
                ]
    in
    div [ class "cards" ]
        [ p [] [ text "Cards" ]
        , ul [ class "buildercards-card_rows" ] (List.map cardRow filteredCards)
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
