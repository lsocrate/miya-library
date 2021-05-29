module UI.Filter.CardType exposing (Model, init, isTypeAllowed, view)

import Card
import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List


type Model
    = Filters Bool Bool Bool Bool


type CardType
    = Attachment
    | Character
    | Event
    | Holding


init : Model
init =
    Filters False False False False


view : Model -> (Model -> msg) -> Html msg
view filters changeMsg =
    let
        icon type_ =
            case type_ of
                Holding ->
                    "🏨"

                Character ->
                    "🧑"

                Attachment ->
                    "🗡️"

                Event ->
                    "⚡"

        option type_ =
            li [ class "cardtypefilter-item" ]
                [ button
                    [ classList
                        [ ( "cardtypefilter-button", True )
                        , ( "cardtypefilter-button--active", isTypeActive filters type_ )
                        ]
                    , onClick (changeMsg (toggleFilter filters type_))
                    ]
                    [ text <| icon type_ ]
                ]
    in
    ul [ class "cardtypefilter" ] <| List.map option [ Attachment, Character, Event, Holding ]


isTypeAllowed : Model -> Card.Card -> Bool
isTypeAllowed filters card =
    let
        isBackFilterActive =
            case card of
                Card.AttachmentType (Card.Attachment _) ->
                    isTypeActive filters Attachment

                Card.CharacterType (Card.ConflictCharacter _) ->
                    isTypeActive filters Character

                Card.CharacterType (Card.DynastyCharacter _) ->
                    isTypeActive filters Character

                Card.EventType (Card.ConflictEvent _) ->
                    isTypeActive filters Event

                Card.EventType (Card.DynastyEvent _) ->
                    isTypeActive filters Event

                Card.HoldingType (Card.Holding _) ->
                    isTypeActive filters Holding

                _ ->
                    False
    in
    isBackFilterActive || (not <| hasFilterActive filters)


isTypeActive : Model -> CardType -> Bool
isTypeActive filters cardType =
    case ( filters, cardType ) of
        ( Filters True _ _ _, Attachment ) ->
            True

        ( Filters _ True _ _, Character ) ->
            True

        ( Filters _ _ True _, Event ) ->
            True

        ( Filters _ _ _ True, Holding ) ->
            True

        ( _, _ ) ->
            False


hasFilterActive : Model -> Bool
hasFilterActive model =
    case model of
        Filters False False False False ->
            False

        _ ->
            True


toggleFilter : Model -> CardType -> Model
toggleFilter filters cardType =
    case filters of
        Filters attachment character event holding ->
            case cardType of
                Attachment ->
                    Filters (not attachment) character event holding

                Character ->
                    Filters attachment (not character) event holding

                Event ->
                    Filters attachment character (not event) holding

                Holding ->
                    Filters attachment character event (not holding)
