module UI.Filter.CardBack exposing (Model, init, isBackAllowed, view)

import Card
import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import UI.Icon exposing (Icon(..))


type Model
    = Filters Bool Bool


type Back
    = Conflict
    | Dynasty


init : Model
init =
    Filters False False


view : Model -> (Model -> msg) -> Html msg
view filters changeMsg =
    let
        option back =
            li [ class "cardbackfilter-item" ]
                [ button
                    [ classList
                        [ ( "cardbackfilter-button", True )
                        , ( "cardbackfilter-button--conflict", back == Conflict )
                        , ( "cardbackfilter-button--dynasty", back == Dynasty )
                        , ( "cardbackfilter-button--active", isBackActive filters back )
                        ]
                    , onClick (changeMsg (toggleFilter filters back))
                    ]
                    [ UI.Icon.medium Fiverings ]
                ]
    in
    ul [ class "cardbackfilter" ] <| List.map option [ Dynasty, Conflict ]


isBackAllowed : Model -> Card.Card -> Bool
isBackAllowed filters card =
    let
        isBackFilterActive =
            case card of
                Card.RoleType _ ->
                    False

                Card.StrongholdType _ ->
                    False

                Card.ProvinceType _ ->
                    False

                Card.AttachmentType (Card.Attachment _) ->
                    isBackActive filters Conflict

                Card.CharacterType (Card.ConflictCharacter _) ->
                    isBackActive filters Conflict

                Card.EventType (Card.ConflictEvent _) ->
                    isBackActive filters Conflict

                Card.EventType (Card.DynastyEvent _) ->
                    isBackActive filters Dynasty

                Card.HoldingType (Card.Holding _) ->
                    isBackActive filters Dynasty

                Card.CharacterType (Card.DynastyCharacter _) ->
                    isBackActive filters Dynasty
    in
    isBackFilterActive || (not <| hasFilterActive filters)


isBackActive : Model -> Back -> Bool
isBackActive filters back =
    case ( filters, back ) of
        ( Filters True _, Conflict ) ->
            True

        ( Filters _ True, Dynasty ) ->
            True

        ( _, _ ) ->
            False


hasFilterActive : Model -> Bool
hasFilterActive model =
    case model of
        Filters False False ->
            False

        _ ->
            True


toggleFilter : Model -> Back -> Model
toggleFilter filters back =
    case filters of
        Filters conflict dynasty ->
            case back of
                Conflict ->
                    Filters (not conflict) dynasty

                Dynasty ->
                    Filters conflict (not dynasty)
