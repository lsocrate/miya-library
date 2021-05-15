module UI.ClanFilterSelector exposing (Model, init, isClanAllowed, view)

import Clan exposing (Clan(..), clanName)
import EverySet exposing (EverySet, empty, insert, isEmpty, member, remove)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List


type alias Model =
    EverySet Clan


init : Model
init =
    empty


isClanAllowed : Model -> Clan -> Bool
isClanAllowed filters clan =
    isEmpty filters || member clan filters


view : Model -> (Model -> msg) -> Html msg
view filters changeMsg =
    let
        selector clan =
            li [ class "clanfilter-item" ]
                [ button
                    [ classList
                        [ ( "clanfilter-button", True )
                        , ( "clanfilter-button--active", member clan filters )
                        ]
                    , onClick (changeMsg (toggleFilter clan filters))
                    ]
                    [ text (clanName clan) ]
                ]
    in
    ul [ class "clanfilter" ] (List.map selector options)


options : List Clan
options =
    [ Crab, Crane, Dragon, Lion, Phoenix, Scorpion, Unicorn, Neutral, Shadowlands ]


toggleFilter : Clan -> Model -> Model
toggleFilter clan filters =
    (if member clan filters then
        remove

     else
        insert
    )
        clan
        filters
