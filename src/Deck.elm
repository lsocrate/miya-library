module Deck exposing (Deck, Decklist, fromDecklist, maxInfluence)

import Card
import Dict
import Shared exposing (CardCollection)


type alias Decklist =
    List ( String, Int )


type alias Deck =
    { stronghold : Card.StrongholdProps
    , role : Maybe Card.RoleProps
    , provinces : List Card.ProvinceProps
    , attachments : List ( Card.AttachmentProps, Int )
    , holdings : List ( Card.HoldingProps, Int )
    , dynastyCharacters : List ( Card.DynastyCharacterProps, Int )
    , conflictCharacters : List ( Card.ConflictCharacterProps, Int )
    , dynastyEvents : List ( Card.DynastyEventProps, Int )
    , conflictEvents : List ( Card.ConflictEventProps, Int )
    }


fromDecklist : CardCollection -> Decklist -> Maybe Deck
fromDecklist collection decklist =
    let
        deckCards =
            List.filterMap
                (\( cardId, n ) ->
                    Dict.get cardId collection |> Maybe.map (\card -> ( card, n ))
                )
                decklist

        categories =
            List.foldl intoCategories emptyCategories deckCards

        { attachments, holdings, dynastyCharacters, conflictCharacters, dynastyEvents, conflictEvents } =
            categories

        stronghold =
            case categories.stronghold of
                [ ( sh, 1 ) ] ->
                    Just sh

                _ ->
                    Nothing

        role =
            case categories.role of
                [ ( rol, 1 ) ] ->
                    Just rol

                _ ->
                    Nothing

        provinces =
            List.filterMap
                (\( p, n ) ->
                    if n == 1 then
                        Just p

                    else
                        Nothing
                )
                categories.provinces
    in
    Maybe.map
        (\sh ->
            { stronghold = sh
            , role = role
            , provinces = provinces
            , attachments = attachments
            , holdings = holdings
            , dynastyCharacters = dynastyCharacters
            , conflictCharacters = conflictCharacters
            , dynastyEvents = dynastyEvents
            , conflictEvents = conflictEvents
            }
        )
        stronghold



--------------
-- CATEGORIZER
--------------


type alias Categories =
    { stronghold : List ( Card.StrongholdProps, Int )
    , role : List ( Card.RoleProps, Int )
    , provinces : List ( Card.ProvinceProps, Int )
    , attachments : List ( Card.AttachmentProps, Int )
    , holdings : List ( Card.HoldingProps, Int )
    , dynastyCharacters : List ( Card.DynastyCharacterProps, Int )
    , conflictCharacters : List ( Card.ConflictCharacterProps, Int )
    , dynastyEvents : List ( Card.DynastyEventProps, Int )
    , conflictEvents : List ( Card.ConflictEventProps, Int )
    }


emptyCategories : Categories
emptyCategories =
    { stronghold = []
    , role = []
    , provinces = []
    , attachments = []
    , holdings = []
    , dynastyCharacters = []
    , conflictCharacters = []
    , dynastyEvents = []
    , conflictEvents = []
    }


intoCategories : ( Card.Card, Int ) -> Categories -> Categories
intoCategories ( card, n ) cats =
    case card of
        Card.RoleType (Card.Role props) ->
            { cats | role = ( props, n ) :: cats.role }

        Card.StrongholdType (Card.Stronghold props) ->
            { cats | stronghold = ( props, n ) :: cats.stronghold }

        Card.AttachmentType (Card.Attachment props) ->
            { cats | attachments = ( props, n ) :: cats.attachments }

        Card.CharacterType (Card.ConflictCharacter props) ->
            { cats | conflictCharacters = ( props, n ) :: cats.conflictCharacters }

        Card.CharacterType (Card.DynastyCharacter props) ->
            { cats | dynastyCharacters = ( props, n ) :: cats.dynastyCharacters }

        Card.EventType (Card.ConflictEvent props) ->
            { cats | conflictEvents = ( props, n ) :: cats.conflictEvents }

        Card.EventType (Card.DynastyEvent props) ->
            { cats | dynastyEvents = ( props, n ) :: cats.dynastyEvents }

        Card.HoldingType (Card.Holding props) ->
            { cats | holdings = ( props, n ) :: cats.holdings }

        Card.ProvinceType (Card.Province props) ->
            { cats | provinces = ( props, n ) :: cats.provinces }



--------------
-- HELPERS
--------------


maxInfluence : Deck -> Int
maxInfluence { role, stronghold } =
    case role of
        Just { traits } ->
            if List.member Card.KeeperRole traits then
                stronghold.influenceValue + 3

            else
                stronghold.influenceValue

        _ ->
            stronghold.influenceValue
