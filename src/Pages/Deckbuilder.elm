module Pages.Deckbuilder exposing (Model, Msg, page)

import Card
import Clan exposing (Clan(..))
import Dict
import Gen.Params.Deckbuilder exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import Numerical
import Page
import Request
import Shared
import UI.Decklist
import UI.Filters
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


type Model
    = ChoosingStronghold (Maybe Deck)
    | Deckbuilding Deck UI.Filters.Model


init : ( Model, Cmd Msg )
init =
    -- ( ChoosingStronghold Nothing, Cmd.none )
    ( Deckbuilding
        { stronghold = "shiro-nishiyama"
        , name = Nothing
        , role = Nothing
        , otherCards = Dict.empty
        }
        UI.Filters.init
    , Cmd.none
    )


type Msg
    = StrongholdSelected String
    | Changed UI.Filters.Model
    | DeckChanged Card.Card Int


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case ( msg, model ) of
        ( StrongholdSelected stronghold, ChoosingStronghold oldDeck ) ->
            let
                newDeck =
                    Maybe.map (\deck -> { deck | stronghold = stronghold }) oldDeck
                        |> Maybe.withDefault
                            { stronghold = stronghold
                            , name = Nothing
                            , role = Nothing
                            , otherCards = Dict.empty
                            }
            in
            ( Deckbuilding newDeck UI.Filters.init, Cmd.none )

        ( Changed newFilters, Deckbuilding deck _ ) ->
            ( Deckbuilding deck newFilters, Cmd.none )

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


viewDeckbuilder : Dict.Dict String Card.Card -> Deck -> UI.Filters.Model -> List (Html Msg)
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


viewFilters : UI.Filters.Model -> Html Msg
viewFilters filters =
    div [ class "filters" ]
        [ p [] [ text "Filters" ]
        , div [ class "filters-row" ] (UI.Filters.view filters Changed)
        ]


viewCardsOptions : Dict.Dict String Card.Card -> Deck -> UI.Filters.Model -> Html Msg
viewCardsOptions cards deck filters =
    let
        cardRow card =
            let
                copiesInDeck =
                    Maybe.withDefault 0 <| Dict.get (Card.id card) deck.otherCards

                picker =
                    div [ class "cardlist-picker" ]
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
            in
            ( Card.id card
            , tr
                [ classList
                    [ ( "cardlist-row", True )
                    , ( "cardlist-row--crab", Crab == Card.clan card )
                    , ( "cardlist-row--crane", Crane == Card.clan card )
                    , ( "cardlist-row--dragon", Dragon == Card.clan card )
                    , ( "cardlist-row--lion", Lion == Card.clan card )
                    , ( "cardlist-row--phoenix", Phoenix == Card.clan card )
                    , ( "cardlist-row--scorpion", Scorpion == Card.clan card )
                    , ( "cardlist-row--unicorn", Unicorn == Card.clan card )
                    , ( "cardlist-row--neutral", Neutral == Card.clan card )
                    , ( "cardlist-row--shadowlands", Shadowlands == Card.clan card )
                    , ( "cardlist-row--conflict", Card.isConflict card )
                    , ( "cardlist-row--dynasty", Card.isDynasty card )
                    ]
                ]
                [ td [ class "cardlist-quantity" ] [ picker ]
                , td [ class "cardlist-clan" ] [ text <| Clan.icon <| Card.clan card ]
                , td [ class "cardlist-type" ] [ text <| Card.typeIcon card ]
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
            )
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
            , Html.Keyed.node "tbody"
                [ classList
                    [ ( "cardlist-filtered--crab", UI.Filters.isClanFilteredOut filters Crab )
                    , ( "cardlist-filtered--crane", UI.Filters.isClanFilteredOut filters Crane )
                    , ( "cardlist-filtered--dragon", UI.Filters.isClanFilteredOut filters Dragon )
                    , ( "cardlist-filtered--lion", UI.Filters.isClanFilteredOut filters Lion )
                    , ( "cardlist-filtered--phoenix", UI.Filters.isClanFilteredOut filters Phoenix )
                    , ( "cardlist-filtered--scorpion", UI.Filters.isClanFilteredOut filters Scorpion )
                    , ( "cardlist-filtered--unicorn", UI.Filters.isClanFilteredOut filters Unicorn )
                    , ( "cardlist-filtered--neutral", UI.Filters.isClanFilteredOut filters Neutral )
                    , ( "cardlist-filtered--shadowlands", UI.Filters.isClanFilteredOut filters Shadowlands )
                    , ( "cardlist-filtered--conflict", UI.Filters.isConflictFilteredOut filters )
                    , ( "cardlist-filtered--dynasty", UI.Filters.isDynastyFilteredOut filters )
                    ]
                ]
                (Dict.values cards
                    |> List.sortWith (compositeSort deck)
                    |> List.map cardRow
                )
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
