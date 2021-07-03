module Pages.Deckbuilder exposing (Model, Msg, page)

import API.DB
import Card
import Clan exposing (Clan(..))
import Deck
import Dict
import Element
import Firestore
import Gen.Params.Deckbuilder exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Influence
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
import Uniqueness
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
    = ChoosingStronghold (Maybe Deck.Deck)
    | Deckbuilding Deck.Deck UI.Filters.Model ProvinceSelectorState NameEditor


init : ( Model, Cmd Msg )
init =
    -- ( ChoosingStronghold Nothing, Cmd.none )
    ( ChoosingStronghold Nothing, Task.perform (always <| SelectedStronghold Card.shiroNishiyama) (Task.succeed ()) )


type NameEditor
    = NotNameEditing
    | NameEditing String


type ProvinceSelectorState
    = ProvinceSelectorIsClosed
    | ProvinceSelectorIsOpen Element.Element


type Msg
    = SelectedStronghold Card.StrongholdProps
    | FilterChanged UI.Filters.Msg
    | ChangedDecklist ( Card.Card, Int )
    | StartUpdateName
    | UpdateName String
    | DoneUpdateName String
    | ProvinceSelectorToggle
    | ProvinceSelectorChangeElement Element.Element
    | ToggleProvince Card.ProvinceProps
    | SavedDocument (Result Firestore.Error Deck.Deck)
    | Save


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case shared of
        Shared.Error ->
            ( model, Cmd.none )

        Shared.Loading ->
            ( model, Cmd.none )

        Shared.Loaded { cards, user } ->
            case ( model, msg ) of
                ( ChoosingStronghold Nothing, SelectedStronghold newStronghold ) ->
                    ( Deckbuilding
                        { meta = Deck.initMeta user newStronghold.clan
                        , cards = Deck.initDeckCards newStronghold
                        }
                        UI.Filters.init
                        ProvinceSelectorIsClosed
                        NotNameEditing
                    , Cmd.none
                    )

                ( ChoosingStronghold (Just oldDeck), SelectedStronghold newStronghold ) ->
                    let
                        oldCards =
                            oldDeck.cards

                        newDeck =
                            { oldDeck | cards = { oldCards | stronghold = newStronghold } }
                    in
                    ( Deckbuilding newDeck UI.Filters.init ProvinceSelectorIsClosed NotNameEditing, Cmd.none )

                ( Deckbuilding deck f p _, StartUpdateName ) ->
                    ( Deckbuilding deck f p (NameEditing <| Maybe.withDefault "" deck.meta.name), Cmd.none )

                ( Deckbuilding d f p (NameEditing _), UpdateName newName ) ->
                    ( Deckbuilding d f p (NameEditing newName), Cmd.none )

                ( Deckbuilding oldDeck f p _, DoneUpdateName newName ) ->
                    let
                        oldMeta =
                            oldDeck.meta

                        newMeta =
                            { oldMeta | name = Just newName }

                        newDeck =
                            { oldDeck | meta = newMeta }
                    in
                    ( Deckbuilding newDeck f p NotNameEditing, Cmd.none )

                ( Deckbuilding d oldFilters p n, FilterChanged subMsg ) ->
                    ( Deckbuilding d (UI.Filters.update subMsg oldFilters) p n, Cmd.none )

                ( Deckbuilding oldDeck f p n, ChangedDecklist ( card, qty ) ) ->
                    let
                        replace items ( props, q ) =
                            let
                                preList =
                                    List.filter (Tuple.first >> (.id >> (/=) props.id)) items
                            in
                            if qty == 0 then
                                preList

                            else
                                ( props, q ) :: preList

                        oldCards =
                            oldDeck.cards

                        newCards =
                            case card of
                                Card.RoleType (Card.Role props) ->
                                    { oldCards | role = Just props }

                                Card.StrongholdType (Card.Stronghold props) ->
                                    { oldCards | stronghold = props }

                                Card.ProvinceType (Card.Province props) ->
                                    { oldCards
                                        | provinces =
                                            if qty == 0 then
                                                List.filter (.id >> (==) props.id) oldCards.provinces

                                            else
                                                props :: List.filter (.id >> (==) props.id) oldCards.provinces
                                    }

                                Card.AttachmentType (Card.Attachment props) ->
                                    { oldCards | attachments = replace oldCards.attachments ( props, qty ) }

                                Card.CharacterType (Card.ConflictCharacter props) ->
                                    { oldCards | conflictCharacters = replace oldCards.conflictCharacters ( props, qty ) }

                                Card.CharacterType (Card.DynastyCharacter props) ->
                                    { oldCards | dynastyCharacters = replace oldCards.dynastyCharacters ( props, qty ) }

                                Card.EventType (Card.ConflictEvent props) ->
                                    { oldCards | conflictEvents = replace oldCards.conflictEvents ( props, qty ) }

                                Card.EventType (Card.DynastyEvent props) ->
                                    { oldCards | dynastyEvents = replace oldCards.dynastyEvents ( props, qty ) }

                                Card.HoldingType (Card.Holding props) ->
                                    { oldCards | holdings = replace oldCards.holdings ( props, qty ) }
                    in
                    ( Deckbuilding { oldDeck | cards = newCards } f p n, Cmd.none )

                ( Deckbuilding dc fs ProvinceSelectorIsClosed ne, ProvinceSelectorToggle ) ->
                    ( Deckbuilding dc fs (ProvinceSelectorIsOpen Element.Air) ne, Cmd.none )

                ( Deckbuilding dc fs (ProvinceSelectorIsOpen _) ne, ProvinceSelectorChangeElement newElement ) ->
                    ( Deckbuilding dc fs (ProvinceSelectorIsOpen newElement) ne, Cmd.none )

                ( Deckbuilding dc fs (ProvinceSelectorIsOpen _) ne, ProvinceSelectorToggle ) ->
                    ( Deckbuilding dc fs ProvinceSelectorIsClosed ne, Cmd.none )

                ( Deckbuilding dc fs ps ne, ToggleProvince province ) ->
                    let
                        oldCards =
                            dc.cards

                        newCards =
                            { oldCards
                                | provinces =
                                    if List.member province oldCards.provinces then
                                        List.filter (.id >> (/=) province.id) oldCards.provinces

                                    else
                                        province :: oldCards.provinces
                            }
                    in
                    ( Deckbuilding { dc | cards = newCards } fs ps ne, Cmd.none )

                ( Deckbuilding dc _ _ _, Save ) ->
                    ( model, API.DB.saveDeck SavedDocument cards dc )

                ( _, SavedDocument result ) ->
                    case result of
                        Err j ->
                            Debug.log "ERROR"
                                Debug.log
                                (Debug.toString j)
                                ( model, Cmd.none )

                        Ok deck ->
                            Debug.log "SUCCESS"
                                Debug.log
                                (Debug.toString deck)
                                ( model, Cmd.none )

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

                ( Shared.Loaded { cards, user }, Deckbuilding deck filters ps ne ) ->
                    [ header [ class "deckbuilder-header" ]
                        [ button [ onClick Save ]
                            [ text "save"
                            ]
                        ]
                    , main_ [ class "deckbuilder-decklist" ]
                        [ UI.Decklist.view decklistActions
                            { name = deck.meta.name
                            , editingName =
                                case ne of
                                    NameEditing _ ->
                                        True

                                    _ ->
                                        False
                            , author = user
                            , deck = deck.cards
                            }
                        ]
                    , aside [ class "deckbuilder-builder" ]
                        (List.concat
                            [ case ps of
                                ProvinceSelectorIsOpen displayEl ->
                                    [ provinceSelector cards deck.cards displayEl ]

                                _ ->
                                    []
                            , [ viewFilters filters ]
                            , [ viewCardsOptions cards deck.cards filters ]
                            ]
                        )
                    ]
    in
    UI.Page.view route viewsForStep


provinceSelector : CardCollection -> Deck.DeckCards -> Element.Element -> Html Msg
provinceSelector =
    let
        viewProvinceSelector collection deck displayEl =
            div [ class "provinceselector" ]
                [ div [ class "provinceselector-dismiss" ]
                    [ button [ onClick ProvinceSelectorToggle ]
                        [ text "Close province selector" ]
                    ]
                , div [ class "provinceselector-tabs" ]
                    (Element.list
                        |> List.map
                            (\el ->
                                label
                                    [ class "provinceselector-tab"
                                    , classList
                                        [ ( "provinceselector-tab--active", el == displayEl )
                                        ]
                                    ]
                                    [ text <| Element.name el
                                    , text " "
                                    , UI.Icon.small <| UI.Icon.element el
                                    , input
                                        [ type_ "radio"
                                        , name "provinceSelectorTab"
                                        , checked <| el == displayEl
                                        , onClick <| ProvinceSelectorChangeElement el
                                        ]
                                        []
                                    ]
                            )
                    )
                , div [ class "provinceselector-provinces" ] <|
                    List.filterMap (viewProvinceLine deck displayEl) (Dict.values collection)
                ]
    in
    Lazy.lazy3 viewProvinceSelector


viewProvinceLine : Deck.DeckCards -> Element.Element -> Card.Card -> Maybe (Html Msg)
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


decklistActions : Maybe { startUpdateName : Msg, updateName : String -> Msg, doneUpdateName : String -> Msg, toggleProvinceSelector : Msg }
decklistActions =
    Just
        { startUpdateName = StartUpdateName
        , updateName = UpdateName
        , doneUpdateName = DoneUpdateName
        , toggleProvinceSelector = ProvinceSelectorToggle
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


viewFilters : UI.Filters.Model -> Html Msg
viewFilters filters =
    div [ class "filters" ]
        [ p [] [ text "Filters" ]
        , UI.Filters.view [ class "filters-row" ] FilterChanged filters
        ]


viewCardsOptions : Dict.Dict String Card.Card -> Deck.DeckCards -> UI.Filters.Model -> Html Msg
viewCardsOptions cards deck filters =
    div [ class "cards" ]
        [ table [ class "cardlist" ]
            [ cardListHeader
            , cardListBody cards filters deck
            ]
        ]


cardListHeader : Html Msg
cardListHeader =
    thead [ class "cardlist-headers" ]
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


cardListBody : Dict.Dict String Card.Card -> UI.Filters.Model -> Deck.DeckCards -> Html Msg
cardListBody =
    let
        viewcardListBody : Dict.Dict String Card.Card -> UI.Filters.Model -> Deck.DeckCards -> Html Msg
        viewcardListBody cards filters deck =
            let
                cardRowx card =
                    let
                        copiesInDeck =
                            Deck.copiesOf deck <| Card.id card
                    in
                    cardRow deck.stronghold.clan card (cardQuantitySelector card copiesInDeck)
            in
            tbody
                [ classList <|
                    List.concat
                        [ List.map (\clan -> ( "cardlist-filtered--" ++ Clan.toString clan, True )) (UI.Filters.blockedClans filters)
                        , List.map (\cardBack -> ( "cardlist-filtered--" ++ Card.cardBackToString cardBack, True )) (UI.Filters.blockedCardBacks filters)
                        , List.map (\cardType -> ( "cardlist-filtered--" ++ Card.cardTypeToString cardType, True )) (UI.Filters.blockedCardTypes filters)
                        , List.map (\uniqueness -> ( "cardlist-filtered--" ++ Uniqueness.toString uniqueness, True )) (UI.Filters.blockedUniqueness filters)
                        , List.map (\influenceCost -> ( "cardlist-filtered--" ++ Influence.toString influenceCost, True )) (UI.Filters.blockedInfluenceCost filters)
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
                    |> List.map cardRowx
                )
    in
    Lazy.lazy3 viewcardListBody


cardRow : Clan -> Card.Card -> Html Msg -> Html Msg
cardRow =
    let
        viewCardRow deckClan card viewCardQuantitySelector =
            tr
                [ class "cardlist-row"
                , class ("cardlist-row--" ++ Clan.toString (Card.clan card))
                , class ("cardlist-row--" ++ Card.backToString card)
                , class ("cardlist-row--" ++ Card.typeToString card)
                , class ("cardlist-row--" ++ Uniqueness.toString (Card.uniqueness card))
                , classList
                    [ ( "cardlist-row--" ++ Influence.toString (Card.influence card), deckClan /= Card.clan card )
                    ]
                ]
                [ td [ class "cardlist-quantity" ] [ viewCardQuantitySelector ]
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
                , td [ class "cardlist-title" ]
                    (List.concat
                        [ [ text <| Card.title card ]
                        , if Card.isUnique card then
                            [ text " "
                            , UI.Icon.copy UI.Icon.Unique
                            ]

                          else
                            []
                        ]
                    )
                , td [ class "cardlist-influence" ]
                    (if deckClan == Card.clan card then
                        []

                     else
                        [ UI.Icon.influence UI.Icon.medium <| Card.influence card ]
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
    Lazy.lazy3 viewCardRow


cardQuantitySelector : Card.Card -> Int -> Html Msg
cardQuantitySelector =
    let
        viewCardQuantitySelector card copiesInDeck =
            div [ class "cardlist-picker" ]
                (List.range 0 3
                    |> List.map
                        (\n ->
                            label
                                [ class "cardlist-option"
                                , classList [ ( "cardlist-option--active", n == copiesInDeck ) ]
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
    Lazy.lazy2 viewCardQuantitySelector


compositeSort : Deck.DeckCards -> Card.Card -> Card.Card -> Order
compositeSort deck a b =
    let
        xyz =
            List.concat
                [ List.map (Tuple.mapFirst .id) deck.attachments
                , List.map (Tuple.mapFirst .id) deck.holdings
                , List.map (Tuple.mapFirst .id) deck.dynastyCharacters
                , List.map (Tuple.mapFirst .id) deck.conflictCharacters
                , List.map (Tuple.mapFirst .id) deck.dynastyEvents
                , List.map (Tuple.mapFirst .id) deck.conflictEvents
                ]

        selected card =
            List.filter (Tuple.first >> (==) (Card.id card)) xyz
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
    in
    case compare (selected a) (selected b) of
        GT ->
            LT

        LT ->
            GT

        EQ ->
            case Card.compareType a b of
                GT ->
                    GT

                LT ->
                    LT

                EQ ->
                    case Card.compareCost a b of
                        GT ->
                            GT

                        LT ->
                            LT

                        EQ ->
                            Card.compareTitle a b
