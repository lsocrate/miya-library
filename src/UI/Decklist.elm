module UI.Decklist exposing (Model, view)

import Card
import Html exposing (..)
import Html.Attributes exposing (..)


type alias DecklistEntry c =
    ( c, Int )


type alias Model =
    { name : Maybe String
    , author : String
    , cards : List (DecklistEntry Card.Card)
    }


view : Model -> Html msg
view deck =
    let
        firstCard =
            List.head >> Maybe.map Tuple.first

        grouped =
            partition deck.cards

        role =
            firstCard grouped.roles

        { provinces, conflictAttachments, conflictCharacters, conflictEvents, dynastyCharacters, dynastyEvents, dynastyHoldings } =
            grouped
    in
    case firstCard grouped.strongholds of
        Nothing ->
            div [] []

        Just stronghold ->
            div [ class "decklist", id "decklist" ]
                [ div [ class "decklist-name" ]
                    [ text <| Maybe.withDefault "Unnamed" deck.name ++ " by " ++ deck.author
                    ]
                , div [ class "decklist-stronghold" ]
                    [ img
                        [ src <| Maybe.withDefault "http://placekitten.com/300/419" stronghold.image
                        , attribute "loading" "lazy"
                        ]
                        []
                    ]
                , div [ class "decklist-header" ]
                    [ h2 [] [ text stronghold.title ]
                    , h3 [] [ text <| Maybe.withDefault "" <| Maybe.map .title role ]
                    , ul [] <| List.map (\( province, _ ) -> li [] [ text <| province.title ++ " " ++ (String.join "/" <| List.map Card.elementName province.elements) ]) provinces
                    ]
                , div [ class "decklist-dynasty_deck" ]
                    (div []
                        [ text <|
                            "Dynasty Deck ("
                                ++ String.fromInt
                                    (sumCards dynastyCharacters
                                        + sumCards dynastyHoldings
                                        + sumCards dynastyEvents
                                    )
                                ++ ")"
                        ]
                        :: cardBlock "Characters" dynastyCharacters
                        ++ cardBlock "Events" dynastyEvents
                        ++ cardBlock "Holdings" dynastyHoldings
                    )
                , div [ class "decklist-conflict_deck" ]
                    (div []
                        [ text <|
                            "Conflict Deck ("
                                ++ String.fromInt
                                    (sumCards conflictAttachments
                                        + sumCards conflictCharacters
                                        + sumCards conflictEvents
                                    )
                                ++ ")"
                        ]
                        :: cardBlock "Attachments" conflictAttachments
                        ++ cardBlock "Characters" conflictCharacters
                        ++ cardBlock "Events" conflictEvents
                    )
                ]


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


partition :
    List (DecklistEntry Card.Card)
    ->
        { strongholds : List (DecklistEntry Card.StrongholdProps)
        , roles : List (DecklistEntry Card.RoleProps)
        , provinces : List (DecklistEntry Card.ProvinceProps)
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
                    { categories | roles = ( props, n ) :: categories.roles }

                Card.StrongholdType (Card.Stronghold props) ->
                    { categories | strongholds = ( props, n ) :: categories.strongholds }

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
                    { categories | provinces = ( props, n ) :: categories.provinces }
    in
    List.foldl intoCategories { strongholds = [], roles = [], provinces = [], conflictAttachments = [], conflictEvents = [], conflictCharacters = [], dynastyCharacters = [], dynastyEvents = [], dynastyHoldings = [] }
