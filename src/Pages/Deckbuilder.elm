module Pages.Deckbuilder exposing (Model, Msg, page)

import Card
import Clan exposing (Clan(..))
import Dict exposing (filter)
import Gen.Params.Deckbuilder exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page
import Request
import Shared
import UI.ClanFilterSelector
import UI.Decklist
import UI.Page
import Url exposing (Protocol(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update shared
        , view = view shared req.route
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Deck =
    { stronghold : String
    , name : Maybe String
    , role : Maybe String
    , otherCards : Dict.Dict String Int
    }


type alias Filters =
    { byClan : UI.ClanFilterSelector.Model, byType : List String }


type Model
    = ChoosingStronghold (Maybe Deck)
    | Deckbuilding Deck Filters


init : ( Model, Cmd Msg )
init =
    ( ChoosingStronghold Nothing, Cmd.none )


type Msg
    = StrongholdSelected String
    | ClanFilterChanged UI.ClanFilterSelector.Model
    | CardCountInDeckChange Card.Card Int


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case ( msg, model ) of
        ( StrongholdSelected stronghold, ChoosingStronghold oldDeck ) ->
            let
                filters =
                    { byClan = UI.ClanFilterSelector.init, byType = [] }

                newDeck =
                    Maybe.map (\deck -> { deck | stronghold = stronghold }) oldDeck
                        |> Maybe.withDefault
                            { stronghold = stronghold
                            , name = Nothing
                            , role = Nothing
                            , otherCards = Dict.empty
                            }
            in
            ( Deckbuilding newDeck filters, Cmd.none )

        ( ClanFilterChanged clanFilters, Deckbuilding deck filters ) ->
            ( Deckbuilding deck { filters | byClan = clanFilters }, Cmd.none )

        ( CardCountInDeckChange card n, Deckbuilding deck filters ) ->
            let
                newDeck =
                    { deck | otherCards = Dict.insert (Card.id card) n deck.otherCards }
            in
            ( Deckbuilding newDeck filters, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Shared.Model -> Route -> Model -> View Msg
view shared route model =
    let
        isStronghold card =
            case card of
                Card.StrongholdType (Card.Stronghold props) ->
                    Just props

                _ ->
                    Nothing

        viewsForStep =
            case ( shared, model ) of
                ( Shared.Loading, _ ) ->
                    [ viewLoading ]

                ( Shared.Error, _ ) ->
                    [ viewError ]

                ( Shared.Loaded { cards }, ChoosingStronghold _ ) ->
                    [ viewStrongholdSelector <| List.filterMap isStronghold <| Dict.values cards ]

                ( Shared.Loaded { cards }, Deckbuilding deck filters ) ->
                    viewDeckbuilder cards deck filters
    in
    UI.Page.view route viewsForStep


viewError : Html Msg
viewError =
    div [] [ text "Error!" ]


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading" ]


viewStrongholdSelector : List Card.StrongholdProps -> Html Msg
viewStrongholdSelector strongholds =
    let
        sortedStrongholds =
            List.sortBy (.clan >> Clan.comparable) strongholds

        strongholdOption strongholdProps =
            li [ class "strongholdpicker-item", onClick (StrongholdSelected strongholdProps.id) ]
                [ img
                    [ src <| Maybe.withDefault "http://placekitten.com/300/419" strongholdProps.image
                    , attribute "loading" "lazy"
                    ]
                    []
                ]
    in
    div [ class "strongholdpicker" ]
        [ h2 [] [ text "Choose a stronghold" ]
        , ul [ class "strongholdpicker-options" ]
            (List.map strongholdOption sortedStrongholds)
        ]


viewDeckbuilder : Dict.Dict String Card.Card -> Deck -> Filters -> List (Html Msg)
viewDeckbuilder cards deck filters =
    let
        roleList =
            Maybe.withDefault [] <|
                Maybe.map (\roleId -> [ ( roleId, 1 ) ]) deck.role

        cardIdEntryToCardEntry ( cardId, n ) =
            Dict.get cardId cards |> Maybe.map (\card -> ( card, n ))

        decklist =
            List.filterMap cardIdEntryToCardEntry <|
                ( deck.stronghold, 1 )
                    :: roleList
                    ++ Dict.toList deck.otherCards
    in
    [ main_ [ class "deckbuilder-decklist" ]
        [ UI.Decklist.view <| { name = Nothing, author = "dude", cards = decklist }
        ]
    , aside [ class "deckbuilder-builder" ]
        [ viewFilters filters
        , viewCardsOptions cards deck filters
        ]
    ]


viewFilters : Filters -> Html Msg
viewFilters filters =
    div [ class "filters" ]
        [ p [] [ text "Filters" ]
        , UI.ClanFilterSelector.view filters.byClan ClanFilterChanged
        ]


viewCardsOptions : Dict.Dict String Card.Card -> Deck -> Filters -> Html Msg
viewCardsOptions cards deck filters =
    let
        filteredCards =
            Dict.values cards
                |> List.filter (Card.clan >> UI.ClanFilterSelector.isClanAllowed filters.byClan)

        cardRow : Card.Card -> Html Msg
        cardRow card =
            let
                copiesInDeck =
                    Maybe.withDefault 0 <| Dict.get (Card.id card) deck.otherCards
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
