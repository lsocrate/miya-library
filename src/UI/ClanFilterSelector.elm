module UI.ClanFilterSelector exposing (Model, init, isClanAllowed, view)

import EverySet exposing (EverySet, empty, insert, isEmpty, member, remove)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Rules.Clans exposing (Clan(..), clanName)


type alias Model =
    EverySet Clan


init : Model
init =
    empty


view : Model -> (Model -> msg) -> Html msg
view filters changeMsg =
    let
        options =
            [ Crab, Crane, Dragon, Lion, Phoenix, Scorpion, Unicorn, Neutral ]

        selector clan =
            li [ class "clanfilter-item" ]
                [ button
                    [ classList
                        [ ( "clanfilter-button", True )
                        , ( "clanfilter-button--active", isActive filters clan )
                        ]
                    , onClick (changeMsg (toggleFilter filters clan))
                    ]
                    [ text (clanName clan) ]
                ]
    in
    ul [ class "clanfilter" ] (List.map selector options)


toggleFilter : Model -> Clan -> Model
toggleFilter filters clan =
    let
        update =
            if member clan filters then
                remove

            else
                insert
    in
    update clan filters


isActive : Model -> Clan -> Bool
isActive filters clan =
    member clan filters


isClanAllowed : Model -> Clan -> Bool
isClanAllowed filters clan =
    isEmpty filters || member clan filters
