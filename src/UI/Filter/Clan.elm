module UI.Filter.Clan exposing (Model, init, isClanAllowed, view)

import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List


type Model
    = Filters Bool Bool Bool Bool Bool Bool Bool Bool Bool


init : Model
init =
    Filters False False False False False False False False False


isClanAllowed : Model -> Clan -> Bool
isClanAllowed filters clan =
    isClanActive filters clan || (not <| hasFilterActive filters)


isClanActive : Model -> Clan -> Bool
isClanActive filters clan =
    case ( filters, clan ) of
        ( Filters True _ _ _ _ _ _ _ _, Crab ) ->
            True

        ( Filters _ True _ _ _ _ _ _ _, Crane ) ->
            True

        ( Filters _ _ True _ _ _ _ _ _, Dragon ) ->
            True

        ( Filters _ _ _ True _ _ _ _ _, Lion ) ->
            True

        ( Filters _ _ _ _ True _ _ _ _, Phoenix ) ->
            True

        ( Filters _ _ _ _ _ True _ _ _, Scorpion ) ->
            True

        ( Filters _ _ _ _ _ _ True _ _, Unicorn ) ->
            True

        ( Filters _ _ _ _ _ _ _ True _, Neutral ) ->
            True

        ( Filters _ _ _ _ _ _ _ _ True, Shadowlands ) ->
            True

        ( _, _ ) ->
            False


hasFilterActive : Model -> Bool
hasFilterActive model =
    case model of
        Filters False False False False False False False False False ->
            False

        _ ->
            True


view : Model -> (Model -> msg) -> Html msg
view filters changeMsg =
    let
        selector clan =
            li [ class "clanfilter-item" ]
                [ button
                    [ classList
                        [ ( "clanfilter-button", True )
                        , ( "clanfilter-button--active", isClanActive filters clan )
                        ]
                    , onClick (changeMsg (toggleFilter clan filters))
                    ]
                    [ img
                        [ src <| Clan.mon clan
                        , alt <| Clan.name clan
                        ]
                        []
                    ]
                ]
    in
    ul [ class "clanfilter" ] (List.map selector options)


options : List Clan
options =
    [ Crab, Crane, Dragon, Lion, Phoenix, Scorpion, Unicorn, Neutral, Shadowlands ]


toggleFilter : Clan -> Model -> Model
toggleFilter clan filters =
    case filters of
        Filters crb crn drg lio phx scp uni ntr shl ->
            case clan of
                Crab ->
                    Filters (not crb) crn drg lio phx scp uni ntr shl

                Crane ->
                    Filters crb (not crn) drg lio phx scp uni ntr shl

                Dragon ->
                    Filters crb crn (not drg) lio phx scp uni ntr shl

                Lion ->
                    Filters crb crn drg (not lio) phx scp uni ntr shl

                Phoenix ->
                    Filters crb crn drg lio (not phx) scp uni ntr shl

                Scorpion ->
                    Filters crb crn drg lio phx (not scp) uni ntr shl

                Unicorn ->
                    Filters crb crn drg lio phx scp (not uni) ntr shl

                Neutral ->
                    Filters crb crn drg lio phx scp uni (not ntr) shl

                Shadowlands ->
                    Filters crb crn drg lio phx scp uni ntr (not shl)
