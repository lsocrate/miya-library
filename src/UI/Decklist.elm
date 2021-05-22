module UI.Decklist exposing (Model, view)

import Card
import Html exposing (..)
import Html.Attributes exposing (..)


type alias DecklistEntry c =
    ( c, Int )


type alias Model =
    { name : Maybe String
    , cards : List (DecklistEntry Card.Card)
    }


view : Model -> Html msg
view deck =
    let
        d =
            partition deck.cards
    in
    main_ [ class "decklist", id "decklist" ]
        [ div [ class "decklist-deck_name" ]
            [ h1 [ class "decklist-deck_name" ]
                [ text <| Maybe.withDefault "Unnamed" deck.name ]
            ]
        , div [ class "decklist-header" ]
            [ div [ class "decklist-header_stronghold" ]
                [ text <| Maybe.withDefault "" deck.name ]
            , div [ class "decklist-header_details" ]
                [ h2 []
                    [ text <| firstTitle d.strongholds ]
                , h3 [] [ text <| firstTitle d.roles ]
                ]
            ]
        , div [ class "decklist-decks" ]
            [ div [ class "decklist-deck" ] []
            , div [ class "decklist-deck" ] []
            ]
        ]


firstTitle : List (DecklistEntry { t | title : String }) -> String
firstTitle shs =
    List.head shs
        |> Maybe.map (Tuple.first >> .title)
        |> Maybe.withDefault ""


viewDeckSide : List (DecklistEntry Card.Card) -> List (Html msg)
viewDeckSide cards =
    let
        cardItem ( card, qty ) =
            li [] [ text (String.fromInt qty ++ "x " ++ Card.title card) ]
    in
    [ ul [] (List.map cardItem cards) ]


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
