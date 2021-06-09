module UI.Decklist exposing (Model, view)

import Card
import Clan exposing (Clan(..))
import Deck exposing (Deck)
import Html exposing (..)
import Html.Attributes exposing (..)
import UI.Icon


type alias DecklistEntry c =
    ( c, Int )


type alias Model =
    { name : Maybe String
    , author : String
    , deck : Deck
    }


view : Model -> Html msg
view model =
    let
        { name, author, deck } =
            model

        { attachments, stronghold, role, conflictCharacters, conflictEvents, dynastyCharacters, dynastyEvents, provinces, holdings } =
            deck
    in
    div
        [ class "decklist" ]
        [ div [ class "decklist-stronghold" ]
            [ img
                [ src <| "/assets/card-" ++ stronghold.id ++ ".webp"
                , attribute "loading" "lazy"
                ]
                []
            ]
        , div [ class "decklist-header" ]
            [ h1 [ class "decklist-title" ] [ text <| Maybe.withDefault "Unnamed" name ]
            , p [ class "decklist-byline" ] [ text <| "By " ++ author ]
            , h2 [ class "decklist-synth" ]
                [ strong [] [ text <| stronghold.title ]
                , text " - "
                , text <| Maybe.withDefault "" <| Maybe.map .title role
                ]
            , p []
                [ strong [] [ text "Influence:" ]
                , text <| String.fromInt <| sumInfluence stronghold.clan attachments + sumInfluence stronghold.clan conflictCharacters + sumInfluence stronghold.clan conflictEvents
                , text " of "
                , text <|
                    String.fromInt <|
                        case role of
                            Just { traits } ->
                                if List.member Card.KeeperRole traits then
                                    stronghold.influenceValue + 3

                                else
                                    stronghold.influenceValue

                            _ ->
                                stronghold.influenceValue
                ]
            , ul [ class "decklist-provincelist" ] <| provinceEntry provinces
            ]
        , div [ class "decklist-dynasty_deck" ]
            (div []
                [ text "Dynasty Deck ("
                , text <| String.fromInt <| sumCards dynastyCharacters + sumCards holdings + sumCards dynastyEvents
                , text ")"
                ]
                :: cardBlock "Characters" dynastyCharacters
                ++ cardBlock "Events" dynastyEvents
                ++ cardBlock "Holdings" holdings
            )
        , div [ class "decklist-conflict_deck" ]
            (div []
                [ text "Conflict Deck ("
                , text <| String.fromInt <| sumCards attachments + sumCards conflictCharacters + sumCards conflictEvents
                , text ")"
                ]
                :: cardBlock "Attachments" attachments
                ++ cardBlock "Characters" conflictCharacters
                ++ cardBlock "Events" conflictEvents
            )
        ]


provinceEntry : List Card.ProvinceProps -> List (Html msg)
provinceEntry =
    List.map
        (\province ->
            li [ class "decklist-province" ]
                (text province.title
                    :: (case province.element of
                            Card.Single el ->
                                [ UI.Icon.small <| UI.Icon.element el ]

                            Card.Double el1 el2 ->
                                [ UI.Icon.small <| UI.Icon.element el1, UI.Icon.small <| UI.Icon.element el2 ]

                            Card.Tomoe ->
                                [ UI.Icon.small UI.Icon.Fiverings ]
                       )
                )
        )


cardBlock : String -> List (DecklistEntry { t | title : String }) -> List (Html msg)
cardBlock title cards =
    let
        cardRow ( card, n ) =
            if n < 1 then
                Nothing

            else
                Just (li [] [ text (String.fromInt n ++ "x " ++ card.title) ])
    in
    if sumCards cards > 0 then
        [ div [] [ text <| title ++ " (" ++ String.fromInt (sumCards cards) ++ ")" ]
        , ul [] <| List.filterMap cardRow cards
        ]

    else
        []


sumCards : List (DecklistEntry a) -> Int
sumCards =
    List.sum << List.map Tuple.second


sumInfluence : Clan -> List (DecklistEntry { t | clan : Clan, influenceCost : Maybe Int }) -> Int
sumInfluence deckClan =
    let
        totalInfluence ( card, n ) =
            if card.clan == deckClan then
                0

            else
                Maybe.map ((*) n) card.influenceCost |> Maybe.withDefault 0
    in
    List.sum << List.map totalInfluence


partition :
    List (DecklistEntry Card.Card)
    ->
        { role : Maybe Card.RoleProps
        , provinces : List Card.ProvinceProps
        , conflictAttachments : List (DecklistEntry Card.AttachmentProps)
        , conflictEvents : List (DecklistEntry Card.ConflictEventProps)
        , conflictCharacters : List (DecklistEntry Card.ConflictCharacterProps)
        , dynastyCharacters : List (DecklistEntry Card.DynastyCharacterProps)
        , dynastyEvents : List (DecklistEntry Card.DynastyEventProps)
        , dynastyHoldings : List (DecklistEntry Card.HoldingProps)
        }
partition =
    let
        intoCategories ( card, n ) categories =
            case card of
                Card.RoleType (Card.Role props) ->
                    { categories | role = Just props }

                Card.AttachmentType (Card.Attachment props) ->
                    { categories | conflictAttachments = ( props, n ) :: categories.conflictAttachments }

                Card.CharacterType (Card.ConflictCharacter props) ->
                    { categories | conflictCharacters = ( props, n ) :: categories.conflictCharacters }

                Card.CharacterType (Card.DynastyCharacter props) ->
                    { categories | dynastyCharacters = ( props, n ) :: categories.dynastyCharacters }

                Card.EventType (Card.ConflictEvent props) ->
                    { categories | conflictEvents = ( props, n ) :: categories.conflictEvents }

                Card.EventType (Card.DynastyEvent props) ->
                    { categories | dynastyEvents = ( props, n ) :: categories.dynastyEvents }

                Card.HoldingType (Card.Holding props) ->
                    { categories | dynastyHoldings = ( props, n ) :: categories.dynastyHoldings }

                Card.ProvinceType (Card.Province props) ->
                    { categories | provinces = props :: categories.provinces }

                _ ->
                    categories
    in
    List.foldl intoCategories { role = Nothing, provinces = [], conflictAttachments = [], conflictEvents = [], conflictCharacters = [], dynastyCharacters = [], dynastyEvents = [], dynastyHoldings = [] }
