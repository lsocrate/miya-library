module UI.Icon exposing (Icon(..), medium)

import Html exposing (Html, i)
import Html.Attributes exposing (class)


type Icon
    = Influence3plus
    | Influence4
    | Influence3
    | Influence2
    | Influence1
    | Seeker
    | Keeper
    | Fiverings
    | Fate
    | Honor
    | Unique
    | Void
    | Water
    | Fire
    | Air
    | Earth
    | Military
    | Political
    | Crab
    | Crane
    | Dragon
    | Lion
    | Neutral
    | Phoenix
    | Scorpion
    | Shadowlands
    | Unicorn
    | Ph1
    | Ph2
    | Ph3
    | Ph4


medium : Icon -> Html msg
medium ico =
    i [ class "icon", class "icon--medium", class <| iconClass ico ] []


iconClass : Icon -> String
iconClass ico =
    case ico of
        Influence3plus ->
            "icon-influence3plus"

        Influence4 ->
            "icon-influence4"

        Influence3 ->
            "icon-influence3"

        Influence2 ->
            "icon-influence2"

        Influence1 ->
            "icon-influence1"

        Seeker ->
            "icon-seeker"

        Keeper ->
            "icon-keeper"

        Fiverings ->
            "icon-fiverings"

        Neutral ->
            "icon-neutral"

        Fate ->
            "icon-fate"

        Honor ->
            "icon-honor"

        Unique ->
            "icon-unique"

        Void ->
            "icon-void"

        Water ->
            "icon-water"

        Fire ->
            "icon-fire"

        Air ->
            "icon-air"

        Earth ->
            "icon-earth"

        Military ->
            "icon-military"

        Political ->
            "icon-political"

        Scorpion ->
            "icon-scorpion"

        Dragon ->
            "icon-dragon"

        Unicorn ->
            "icon-unicorn"

        Phoenix ->
            "icon-phoenix"

        Lion ->
            "icon-lion"

        Crane ->
            "icon-crane"

        Crab ->
            "icon-crab"

        Ph1 ->
            "icon-ph1"

        Ph2 ->
            "icon-ph2"

        Ph3 ->
            "icon-ph3"

        Ph4 ->
            "icon-ph4"

        Shadowlands ->
            "icon-shadowlands"
