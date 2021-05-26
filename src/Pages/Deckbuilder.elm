module Pages.Deckbuilder exposing (Model, Msg, page)

import Card
import Clan exposing (Clan(..))
import Dict
import Gen.Params.Deckbuilder exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Numerical
import Page
import Request
import Shared
import UI.Decklist
import UI.Filter.CardBack
import UI.Filter.Clan
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
    { byClan : UI.Filter.Clan.Model, byCardBack : UI.Filter.CardBack.Model }


type Model
    = ChoosingStronghold (Maybe Deck)
    | Deckbuilding Deck Filters


init : ( Model, Cmd Msg )
init =
    -- ( ChoosingStronghold Nothing, Cmd.none )
    ( Deckbuilding
        { stronghold = "shiro-nishiyama"
        , name = Nothing
        , role = Nothing
        , otherCards = Dict.empty
        }
        { byClan = UI.Filter.Clan.init, byCardBack = UI.Filter.CardBack.init }
    , Cmd.none
    )


type Msg
    = StrongholdSelected String
    | FilterChangedClan UI.Filter.Clan.Model
    | FilterChangedCardBack UI.Filter.CardBack.Model
    | DeckChanged Card.Card Int


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case ( msg, model ) of
        ( StrongholdSelected stronghold, ChoosingStronghold oldDeck ) ->
            let
                filters =
                    { byClan = UI.Filter.Clan.init, byCardBack = UI.Filter.CardBack.init }

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

        ( FilterChangedClan newFilter, Deckbuilding deck oldFilters ) ->
            ( Deckbuilding deck { oldFilters | byClan = newFilter }, Cmd.none )

        ( FilterChangedCardBack newFilter, Deckbuilding deck oldFilters ) ->
            ( Deckbuilding deck { oldFilters | byCardBack = newFilter }, Cmd.none )

        ( DeckChanged card n, Deckbuilding deck filters ) ->
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
        , div [ class "filters-row" ]
            [ UI.Filter.Clan.view filters.byClan FilterChangedClan
            , UI.Filter.CardBack.view filters.byCardBack FilterChangedCardBack
            ]
        ]


viewCardsOptions : Dict.Dict String Card.Card -> Deck -> Filters -> Html Msg
viewCardsOptions cards deck filters =
    let
        filteredCards =
            Dict.values cards
                |> List.filter (compositeFilter filters)
                |> List.sortWith (compositeSort deck)

        cardTypeIcon card =
            case card of
                Card.StrongholdType _ ->
                    "🏯"

                Card.ProvinceType _ ->
                    "⛰️"

                Card.HoldingType _ ->
                    "🏨"

                Card.CharacterType _ ->
                    "🧑"

                Card.AttachmentType _ ->
                    "🗡️"

                Card.EventType _ ->
                    "⚡"

                Card.RoleType _ ->
                    "📇"

        cardRow card =
            let
                copiesInDeck =
                    Maybe.withDefault 0 <| Dict.get (Card.id card) deck.otherCards
            in
            tr [ class "cardlist-row" ]
                [ td [ class "cardlist-quantity" ]
                    [ div [ class "cardlist-picker" ]
                        (List.range 0 3
                            |> List.map
                                (\qty ->
                                    label
                                        [ classList
                                            [ ( "cardlist-option", True )
                                            , ( "cardlist-option--active", qty == copiesInDeck )
                                            ]
                                        ]
                                        [ text <| String.fromInt qty
                                        , input
                                            [ type_ "radio"
                                            , name <| Card.title card
                                            , onClick <| DeckChanged card qty
                                            ]
                                            []
                                        ]
                                )
                        )
                    ]
                , td [ class "cardlist-clan" ] [ text (Clan.icon <| Card.clan card) ]
                , td [ class "cardlist-type" ] [ text (cardTypeIcon card) ]
                , td [ class "cardlist-title" ]
                    [ text (Card.title card)
                    , text <|
                        if Card.isUnique card then
                            " ⁕"

                        else
                            ""
                    ]
                , td [ class "cardlist-influence" ]
                    [ text <| Maybe.withDefault "" <| Maybe.map (\n -> String.repeat n "/") <| Card.influence card ]
                , td [ class "cardlist-cost" ]
                    [ text <| Maybe.withDefault "•" <| Maybe.map Numerical.toString <| Card.cost card ]
                , td [ class "cardlist-military" ]
                    [ text <| Maybe.withDefault "•" <| Maybe.map Numerical.toString <| Card.military card ]
                , td [ class "cardlist-political" ]
                    [ text <| Maybe.withDefault "•" <| Maybe.map Numerical.toString <| Card.political card ]
                , td [ class "cardlist-glory" ]
                    [ text <| Maybe.withDefault "•" <| Maybe.map String.fromInt <| Card.glory card ]
                , td [ class "cardlist-strength" ]
                    [ text <| Maybe.withDefault "•" <| Maybe.map Numerical.toString <| Card.strength card ]
                ]
    in
    div [ class "cards" ]
        [ p [] [ text "Cards" ]
        , table [ class "cardlist" ]
            [ thead []
                [ tr []
                    [ th [ class "cardlist-quantity" ] [ text "Quantity" ]
                    , th [ class "cardlist-clan" ] []
                    , th [ class "cardlist-type" ] []
                    , th [ class "cardlist-title" ] [ text "Title" ]
                    , th [ class "cardlist-influence" ] []
                    , th [ class "cardlist-cost" ] [ text "C" ]
                    , th [ class "cardlist-military" ] [ text "M" ]
                    , th [ class "cardlist-political" ] [ text "P" ]
                    , th [ class "cardlist-glory" ] [ text "G" ]
                    , th [ class "cardlist-strength" ] [ text "S" ]
                    ]
                ]
            , tbody [] (List.map cardRow filteredCards)
            ]
        ]


compositeSort : Deck -> Card.Card -> Card.Card -> Order
compositeSort deck a b =
    let
        selected card =
            Maybe.withDefault 0 <| Dict.get (Card.id card) deck.otherCards

        cardType card =
            case card of
                Card.AttachmentType _ ->
                    0

                Card.CharacterType _ ->
                    1

                Card.EventType _ ->
                    2

                Card.HoldingType _ ->
                    3

                Card.ProvinceType _ ->
                    4

                Card.RoleType _ ->
                    5

                Card.StrongholdType _ ->
                    6

        cost card =
            case Card.cost card of
                Nothing ->
                    0

                Just Numerical.Dash ->
                    1

                Just Numerical.VariableValue ->
                    2

                Just Numerical.VariableModifier ->
                    3

                Just (Numerical.FixedValue n) ->
                    100 + n

                Just (Numerical.FixedModifier n) ->
                    100 + n
    in
    case compare (selected a) (selected b) of
        GT ->
            LT

        LT ->
            GT

        EQ ->
            case compare (cardType a) (cardType b) of
                GT ->
                    GT

                LT ->
                    LT

                EQ ->
                    case compare (cost a) (cost b) of
                        GT ->
                            GT

                        LT ->
                            LT

                        EQ ->
                            EQ


compositeFilter : Filters -> Card.Card -> Bool
compositeFilter filters card =
    (Card.clan card |> UI.Filter.Clan.isClanAllowed filters.byClan) && UI.Filter.CardBack.isBackAllowed filters.byCardBack card
