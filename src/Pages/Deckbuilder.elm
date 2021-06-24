module Pages.Deckbuilder exposing (Model, Msg, page)

import Card
import Clan exposing (Clan(..))
import Deck
import Dict
import Element
import Gen.Params.Deckbuilder exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Numerical
import Page
import Request
import Shared exposing (CardCollection)
import Task
import UI.Card
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
    | Deckbuilding DeckCards UI.Filters.Model ProvinceSelector


init : ( Model, Cmd Msg )
init =
    -- ( ChoosingStronghold Nothing, Cmd.none )
    ( ChoosingStronghold Nothing, Task.perform (always <| SelectedStronghold Card.shiroNishiyama) (Task.succeed ()) )


type alias DeckCards =
    { stronghold : Card.StrongholdProps
    , name : DeckName
    , role : Maybe Card.RoleProps
    , otherCards : Dict.Dict String Int
    }


type DeckName
    = Unnamed
    | Named String
    | EditingName String (Maybe String)


type ProvinceSelector
    = ProvinceSelectorClosed
    | ProvinceSelectorOpen Element.Element


type Msg
    = SelectedStronghold Card.StrongholdProps
    | FilterChanged UI.Filters.Msg
    | ChangedDecklist ( Card.Card, Int )
    | StartUpdateName
    | UpdateName String
    | DoneUpdateName String
    | OpenProvinceSelector Element.Element
    | CloseProvinceSelector
    | ToggleProvince Card.ProvinceProps


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
            ( Deckbuilding newDeck UI.Filters.init ProvinceSelectorClosed, Cmd.none )

        ( Deckbuilding deck filters ps, StartUpdateName ) ->
            let
                oldName =
                    case deck.name of
                        Named name ->
                            Just name

                        _ ->
                            Nothing
            in
            ( Deckbuilding { deck | name = EditingName "" oldName } filters ps, Cmd.none )

        ( Deckbuilding deck filters ps, UpdateName newName ) ->
            let
                oldName =
                    case deck.name of
                        Named name ->
                            Just name

                        _ ->
                            Nothing
            in
            ( Deckbuilding { deck | name = EditingName newName oldName } filters ps, Cmd.none )

        ( Deckbuilding oldDeck filters ps, DoneUpdateName newName ) ->
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
            ( Deckbuilding newDeck filters ps, Cmd.none )

        ( Deckbuilding deck oldFilters ps, FilterChanged subMsg ) ->
            ( Deckbuilding deck (UI.Filters.update subMsg oldFilters) ps, Cmd.none )

        ( Deckbuilding oldDeck filters ps, ChangedDecklist ( card, qty ) ) ->
            let
                newDeck =
                    if qty == 0 then
                        { oldDeck | otherCards = Dict.remove (Card.id card) oldDeck.otherCards }

                    else
                        { oldDeck | otherCards = Dict.insert (Card.id card) qty oldDeck.otherCards }
            in
            ( Deckbuilding newDeck filters ps, Cmd.none )

        ( Deckbuilding dc fs (ProvinceSelectorOpen _), OpenProvinceSelector newElement ) ->
            ( Deckbuilding dc fs (ProvinceSelectorOpen newElement), Cmd.none )

        ( Deckbuilding dc fs _, CloseProvinceSelector ) ->
            ( Deckbuilding dc fs ProvinceSelectorClosed, Cmd.none )

        ( Deckbuilding dc fs ps, ToggleProvince { id } ) ->
            ( Deckbuilding
                { dc
                    | otherCards =
                        if Dict.member id dc.otherCards then
                            Dict.remove id dc.otherCards

                        else
                            Dict.insert id 1 dc.otherCards
                }
                fs
                ps
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Shared.Model -> Route -> Model -> View Msg
view shared route model =
    let
        toStronghold card =
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
                    [ List.filterMap toStronghold (Dict.values cards) |> viewStrongholdSelector ]

                ( Shared.Loaded { cards }, Deckbuilding deckCards filters ps ) ->
                    case intoDeck cards deckCards of
                        Nothing ->
                            [ UI.Error.view ]

                        Just deck ->
                            [ main_ [ class "deckbuilder-decklist" ]
                                [ decklistModel deckCards deck |> UI.Decklist.view decklistActions
                                ]
                            , aside [ class "deckbuilder-builder" ]
                                (List.filterMap identity
                                    [ provinceSelector cards deck ps
                                    , viewFilters filters
                                    , viewCardsOptions cards deckCards filters
                                    ]
                                )
                            ]
    in
    UI.Page.view route viewsForStep


provinceSelector : CardCollection -> Deck.Deck -> ProvinceSelector -> Maybe (Html Msg)
provinceSelector collection deck ps =
    case ps of
        ProvinceSelectorClosed ->
            Nothing

        ProvinceSelectorOpen displayEl ->
            Just
                (div [ class "provinceselector" ]
                    [ div [ class "provinceselector-dismiss" ]
                        [ button [ onClick CloseProvinceSelector ]
                            [ text "Close province selector" ]
                        ]
                    , div [ class "provinceselector-tabs" ]
                        (Element.list
                            |> List.map
                                (\el ->
                                    label
                                        [ classList
                                            [ ( "provinceselector-tab", True )
                                            , ( "provinceselector-tab--active", el == displayEl )
                                            ]
                                        ]
                                        [ text <| Element.name el
                                        , text " "
                                        , UI.Icon.small <| UI.Icon.element el
                                        , input
                                            [ type_ "radio"
                                            , name "provinceSelectorTab"
                                            , checked <| el == displayEl
                                            , onClick <| OpenProvinceSelector el
                                            ]
                                            []
                                        ]
                                )
                        )
                    , div [ class "provinceselector-provinces" ] <|
                        List.filterMap (viewProvinceLine deck displayEl) (Dict.values collection)
                    ]
                )


viewProvinceLine : Deck.Deck -> Element.Element -> Card.Card -> Maybe (Html Msg)
viewProvinceLine deck displayEl card =
    Just card
        |> Maybe.andThen
            (\someCard ->
                case someCard of
                    Card.ProvinceType (Card.Province props) ->
                        Just props

                    _ ->
                        Nothing
            )
        |> Maybe.andThen
            (\provProps ->
                case provProps.element of
                    Card.Tomoe ->
                        Just provProps

                    Card.Single el ->
                        if el == displayEl then
                            Just provProps

                        else
                            Nothing

                    Card.Double elA elB ->
                        if elA == displayEl || elB == displayEl then
                            Just provProps

                        else
                            Nothing
            )
        |> Maybe.andThen
            (\provProps ->
                if provProps.clan == deck.stronghold.clan || provProps.clan == Neutral then
                    Just provProps

                else
                    Nothing
            )
        |> Maybe.andThen
            (\prov ->
                Just <|
                    button
                        [ class "provinceselector-province"
                        , onClick <| ToggleProvince prov
                        ]
                        [ UI.Card.lazy prov ]
            )


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
                            [ UI.Card.lazy sh
                            ]
                    )
            )
        ]


intoDeck : CardCollection -> DeckCards -> Maybe Deck.Deck
intoDeck cards deckCards =
    [ Dict.toList deckCards.otherCards
    , [ ( deckCards.stronghold.id, 1 ) ]
    , Maybe.map (\role -> [ ( role.id, 1 ) ]) deckCards.role
        |> Maybe.withDefault []
    ]
        |> List.concat
        |> List.filter (\( cardId, _ ) -> Dict.member cardId cards)
        |> Deck.fromDecklist cards


viewFilters : UI.Filters.Model -> Maybe (Html Msg)
viewFilters filters =
    Just
        (div [ class "filters" ]
            [ p [] [ text "Filters" ]
            , UI.Filters.view [ class "filters-row" ] FilterChanged filters
            ]
        )


viewCardsOptions : Dict.Dict String Card.Card -> DeckCards -> UI.Filters.Model -> Maybe (Html Msg)
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
                                            , name <| "count-" ++ Card.title card
                                            , checked <| n == copiesInDeck
                                            , onClick <| ChangedDecklist ( card, n )
                                            ]
                                            []
                                        ]
                                )
                        )
            in
            tr
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
                , td [ class "cardlist-clan" ] [ UI.Icon.large <| UI.Icon.clan <| Card.clan card ]
                , td [ class "cardlist-type" ]
                    (case card of
                        Card.AttachmentType _ ->
                            [ UI.Icon.medium UI.Icon.Attachment ]

                        Card.CharacterType _ ->
                            [ UI.Icon.medium UI.Icon.Character ]

                        Card.EventType _ ->
                            [ UI.Icon.medium UI.Icon.Event ]

                        Card.HoldingType _ ->
                            [ UI.Icon.medium UI.Icon.Holding ]

                        _ ->
                            []
                    )
                , td [ class "cardlist-title" ] [ text <| Card.title card ]
                , td [ class "cardlist-influence" ]
                    (if deck.stronghold.clan == Card.clan card then
                        []

                     else
                        case Card.influence card of
                            Just 1 ->
                                [ UI.Icon.medium UI.Icon.Influence1 ]

                            Just 2 ->
                                [ UI.Icon.medium UI.Icon.Influence2 ]

                            Just 3 ->
                                [ UI.Icon.medium UI.Icon.Influence3 ]

                            Just 4 ->
                                [ UI.Icon.medium UI.Icon.Influence4 ]

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
    in
    Just
        (div [ class "cards" ]
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
                , tbody
                    [ classList
                        [ ( "cardlist-filtered--crab", UI.Filters.isClanOut filters Crab )
                        , ( "cardlist-filtered--crane", UI.Filters.isClanOut filters Crane )
                        , ( "cardlist-filtered--dragon", UI.Filters.isClanOut filters Dragon )
                        , ( "cardlist-filtered--lion", UI.Filters.isClanOut filters Lion )
                        , ( "cardlist-filtered--phoenix", UI.Filters.isClanOut filters Phoenix )
                        , ( "cardlist-filtered--scorpion", UI.Filters.isClanOut filters Scorpion )
                        , ( "cardlist-filtered--unicorn", UI.Filters.isClanOut filters Unicorn )
                        , ( "cardlist-filtered--neutral", UI.Filters.isClanOut filters Neutral )
                        , ( "cardlist-filtered--shadowlands", UI.Filters.isClanOut filters Shadowlands )
                        , ( "cardlist-filtered--conflict", UI.Filters.isCardBackOut filters UI.Filters.Conflict )
                        , ( "cardlist-filtered--dynasty", UI.Filters.isCardBackOut filters UI.Filters.Dynasty )
                        , ( "cardlist-filtered--character", UI.Filters.isCardTypeOut filters UI.Filters.Character )
                        , ( "cardlist-filtered--attachment", UI.Filters.isCardTypeOut filters UI.Filters.Attachment )
                        , ( "cardlist-filtered--event", UI.Filters.isCardTypeOut filters UI.Filters.Event )
                        , ( "cardlist-filtered--holding", UI.Filters.isCardTypeOut filters UI.Filters.Holding )
                        ]
                    ]
                    (Dict.values cards
                        |> List.filter
                            (\card ->
                                case card of
                                    Card.StrongholdType _ ->
                                        False

                                    Card.RoleType _ ->
                                        False

                                    Card.ProvinceType _ ->
                                        False

                                    _ ->
                                        Card.isPlayable deck.stronghold.clan deck.role card
                            )
                        |> List.sortWith (compositeSort deck)
                        |> List.map cardRow
                    )
                ]
            ]
        )


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
