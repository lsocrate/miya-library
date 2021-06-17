module Pages.Deckbuilder exposing (Model, Msg, page)

import Card
import Clan exposing (Clan(..))
import Deck
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
import Task
import UI.Decklist
import UI.Error
import UI.Filters
import UI.Icon
import UI.Loading
import UI.Page
import Url exposing (Protocol(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update shared
        , view = view shared req.route
        , subscriptions = always Sub.none
        }


type Model
    = ChoosingStronghold (Maybe DeckCards)
    | Deckbuilding DeckCards UI.Filters.Model


init : ( Model, Cmd Msg )
init =
    -- ( ChoosingStronghold Nothing, Cmd.none )
    ( ChoosingStronghold Nothing, Task.perform (always <| SelectedStronghold Card.shiroNishiyama) (Task.succeed ()) )


type alias DeckCards =
    { stronghold : Card.StrongholdProps
    , name : DeckName
    , role : Maybe String
    , otherCards : Dict.Dict String Int
    }


type DeckName
    = Unnamed
    | Named String
    | EditingName String (Maybe String)


type Msg
    = SelectedStronghold Card.StrongholdProps
    | ChangedFilters UI.Filters.Model
    | ChangedDecklist ( Card.Card, Int )
    | StartUpdateName
    | UpdateName String
    | DoneUpdateName String


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case ( model, msg ) of
        ( ChoosingStronghold oldDeck, SelectedStronghold newStronghold ) ->
            let
                newDeck =
                    Maybe.map (\deck -> { deck | stronghold = newStronghold }) oldDeck
                        |> Maybe.withDefault
                            { stronghold = newStronghold
                            , name = Unnamed
                            , role = Nothing
                            , otherCards = Dict.empty
                            }
            in
            ( Deckbuilding newDeck UI.Filters.init, Cmd.none )

        ( Deckbuilding deck filters, StartUpdateName ) ->
            let
                oldName =
                    case deck.name of
                        Named name ->
                            Just name

                        _ ->
                            Nothing
            in
            ( Deckbuilding { deck | name = EditingName "" oldName } filters, Cmd.none )

        ( Deckbuilding deck filters, UpdateName newName ) ->
            let
                oldName =
                    case deck.name of
                        Named name ->
                            Just name

                        _ ->
                            Nothing
            in
            ( Deckbuilding { deck | name = EditingName newName oldName } filters, Cmd.none )

        ( Deckbuilding oldDeck filters, DoneUpdateName newName ) ->
            let
                newDeck =
                    { oldDeck
                        | name =
                            if String.isEmpty newName then
                                Unnamed

                            else
                                Named newName
                    }
            in
            ( Deckbuilding newDeck filters, Cmd.none )

        ( Deckbuilding deck _, ChangedFilters newFilters ) ->
            ( Deckbuilding deck newFilters, Cmd.none )

        ( Deckbuilding oldDeck filters, ChangedDecklist ( card, qty ) ) ->
            let
                newDeck =
                    if qty == 0 then
                        { oldDeck | otherCards = Dict.remove (Card.id card) oldDeck.otherCards }

                    else
                        { oldDeck | otherCards = Dict.insert (Card.id card) qty oldDeck.otherCards }
            in
            ( Deckbuilding newDeck filters, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Shared.Model -> Route -> Model -> View Msg
view shared route model =
    let
        isStronghold card =
            case card of
                Card.StrongholdType (Card.Stronghold sh) ->
                    Just sh

                _ ->
                    Nothing

        viewsForStep =
            case ( shared, model ) of
                ( Shared.Loading, _ ) ->
                    [ UI.Loading.view ]

                ( Shared.Error, _ ) ->
                    [ UI.Error.view ]

                ( Shared.Loaded { cards }, ChoosingStronghold _ ) ->
                    [ List.filterMap isStronghold (Dict.values cards) |> viewStrongholdSelector ]

                ( Shared.Loaded { cards }, Deckbuilding deckCards filters ) ->
                    [ main_ [ class "deckbuilder-decklist" ]
                        [ intoDeck cards deckCards
                            |> Maybe.map (decklistModel deckCards >> UI.Decklist.view decklistActions)
                            |> Maybe.withDefault UI.Error.view
                        ]
                    , aside [ class "deckbuilder-builder" ]
                        [ viewFilters filters
                        , viewCardsOptions cards deckCards filters
                        ]
                    ]
    in
    UI.Page.view route viewsForStep


decklistActions : Maybe { startUpdateName : Msg, updateName : String -> Msg, doneUpdateName : String -> Msg }
decklistActions =
    Just
        { startUpdateName = StartUpdateName
        , updateName = UpdateName
        , doneUpdateName = DoneUpdateName
        }


decklistModel : DeckCards -> Deck.Deck -> UI.Decklist.Model
decklistModel deckCards deck =
    { name =
        case deckCards.name of
            Named name ->
                Just name

            EditingName name _ ->
                Just name

            _ ->
                Nothing
    , author = "dude"
    , deck = deck
    , editingName =
        case deckCards.name of
            EditingName _ _ ->
                True

            _ ->
                False
    }


viewStrongholdSelector : List Card.StrongholdProps -> Html Msg
viewStrongholdSelector strongholds =
    div [ class "strongholdpicker" ]
        [ h2 [] [ text "Choose a stronghold" ]
        , ul [ class "strongholdpicker-options" ]
            (List.sortBy (.clan >> Clan.comparable) strongholds
                |> List.map
                    (\sh ->
                        li
                            [ class "strongholdpicker-item"
                            , onClick <| SelectedStronghold sh
                            ]
                            [ img
                                [ src <| Card.image sh.id
                                , attribute "loading" "lazy"
                                ]
                                []
                            ]
                    )
            )
        ]


intoDeck : Dict.Dict String Card.Card -> DeckCards -> Maybe Deck.Deck
intoDeck cards deckCards =
    [ Dict.toList deckCards.otherCards
    , [ ( deckCards.stronghold.id, 1 ) ]
    , Maybe.map (\roleId -> [ ( roleId, 1 ) ]) deckCards.role
        |> Maybe.withDefault []
    ]
        |> List.concat
        |> List.filter (\( cardId, _ ) -> Dict.member cardId cards)
        |> Deck.fromDecklist cards


viewFilters : UI.Filters.Model -> Html Msg
viewFilters filters =
    div [ class "filters" ]
        [ p [] [ text "Filters" ]
        , div [ class "filters-row" ] (UI.Filters.view filters ChangedFilters)
        ]


viewCardsOptions : Dict.Dict String Card.Card -> DeckCards -> UI.Filters.Model -> Html Msg
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
                                (\n ->
                                    label
                                        [ classList
                                            [ ( "cardlist-option", True )
                                            , ( "cardlist-option--active", n == copiesInDeck )
                                            ]
                                        ]
                                        [ text <| String.fromInt n
                                        , input
                                            [ type_ "radio"
                                            , name <| Card.title card
                                            , onClick <| ChangedDecklist ( card, n )
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
                    , ( "cardlist-row--character", Card.isCharacter card )
                    , ( "cardlist-row--attachment", Card.isAttachment card )
                    , ( "cardlist-row--event", Card.isEvent card )
                    , ( "cardlist-row--holding", Card.isHolding card )
                    ]
                ]
                [ td [ class "cardlist-quantity" ] [ picker ]
                , td [ class "cardlist-clan" ] [ UI.Icon.small <| UI.Icon.clan <| Card.clan card ]
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
                    (case Card.influence card of
                        Just 1 ->
                            [ UI.Icon.small UI.Icon.Influence1 ]

                        Just 2 ->
                            [ UI.Icon.small UI.Icon.Influence2 ]

                        Just 3 ->
                            [ UI.Icon.small UI.Icon.Influence3 ]

                        Just 4 ->
                            [ UI.Icon.small UI.Icon.Influence4 ]

                        _ ->
                            []
                    )
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
        [ table [ class "cardlist" ]
            [ thead [ class "cardlist-headers" ]
                [ tr []
                    [ th [ class "cardlist-quantity" ] [ text "Quantity" ]
                    , th [ class "cardlist-clan" ] []
                    , th [ class "cardlist-type" ] []
                    , th [ class "cardlist-title" ] [ text "Title" ]
                    , th [ class "cardlist-influence" ] [ UI.Icon.small UI.Icon.Influence1 ]
                    , th [ class "cardlist-cost" ] [ UI.Icon.small UI.Icon.Fate ]
                    , th [ class "cardlist-military" ] [ UI.Icon.small UI.Icon.Military ]
                    , th [ class "cardlist-political" ] [ UI.Icon.small UI.Icon.Political ]
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
                    , ( "cardlist-filtered--character", UI.Filters.isCharacterFilteredOut filters )
                    , ( "cardlist-filtered--attachment", UI.Filters.isAttachmentFilteredOut filters )
                    , ( "cardlist-filtered--event", UI.Filters.isEventFilteredOut filters )
                    , ( "cardlist-filtered--holding", UI.Filters.isHoldingFilteredOut filters )
                    ]
                ]
                (Dict.values cards
                    |> List.sortWith (compositeSort deck)
                    |> List.map cardRow
                )
            ]
        ]


compositeSort : DeckCards -> Card.Card -> Card.Card -> Order
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
