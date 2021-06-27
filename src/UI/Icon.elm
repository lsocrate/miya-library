module UI.Icon exposing (Icon(..), clan, element, influence, large, medium, small)

import Clan
import Element
import Html exposing (Attribute, Html, i, text)
import Html.Attributes exposing (class)
import Influence
import Uniqueness exposing (Uniqueness(..))


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
    | Character
    | Attachment
    | Event
    | Holding
    | Restricted
    | NonUnique


clan : Clan.Clan -> Icon
clan c =
    case c of
        Clan.Crab ->
            Crab

        Clan.Crane ->
            Crane

        Clan.Dragon ->
            Dragon

        Clan.Lion ->
            Lion

        Clan.Phoenix ->
            Phoenix

        Clan.Scorpion ->
            Scorpion

        Clan.Unicorn ->
            Unicorn

        Clan.Neutral ->
            Neutral

        Clan.Shadowlands ->
            Shadowlands


element : Element.Element -> Icon
element el =
    case el of
        Element.Air ->
            Air

        Element.Earth ->
            Earth

        Element.Fire ->
            Fire

        Element.Void ->
            Void

        Element.Water ->
            Water


influence : (Icon -> Html msg) -> Influence.InfluenceCost -> Html msg
influence size inf =
    case inf of
        Influence.None ->
            noIcon

        Influence.InfluenceCost1 ->
            size Influence1

        Influence.InfluenceCost2 ->
            size Influence2

        Influence.InfluenceCost3 ->
            size Influence3

        Influence.InfluenceCost4 ->
            size Influence4


small : Icon -> Html msg
small =
    icon (class "icon--small")


medium : Icon -> Html msg
medium =
    icon (class "icon--medium")


large : Icon -> Html msg
large =
    icon (class "icon--large")


icon : Attribute msg -> Icon -> Html msg
icon classForSize ico =
    case ico of
        Character ->
            i [ class "material-icons", classForSize ] [ text "person" ]

        Attachment ->
            i [ class "material-icons", classForSize ] [ text "attach_file" ]

        Event ->
            i [ class "material-icons", classForSize ] [ text "bolt" ]

        Holding ->
            i [ class "material-icons", classForSize ] [ text "home" ]

        Restricted ->
            i [ class "material-icons", classForSize ] [ text "warning" ]

        NonUnique ->
            i [ class "material-icons", classForSize ] [ text "trip_origin" ]

        _ ->
            i [ class "icon", classForSize, classForIcon ico ] []


noIcon : Html msg
noIcon =
    i [] []


classForIcon : Icon -> Attribute msg
classForIcon ico =
    case ico of
        Influence3plus ->
            class "icon-influence3plus"

        Influence4 ->
            class "icon-influence4"

        Influence3 ->
            class "icon-influence3"

        Influence2 ->
            class "icon-influence2"

        Influence1 ->
            class "icon-influence1"

        Seeker ->
            class "icon-seeker"

        Keeper ->
            class "icon-keeper"

        Fiverings ->
            class "icon-fiverings"

        Neutral ->
            class "icon-neutral"

        Fate ->
            class "icon-fate"

        Honor ->
            class "icon-honor"

        Unique ->
            class "icon-unique"

        Void ->
            class "icon-void"

        Water ->
            class "icon-water"

        Fire ->
            class "icon-fire"

        Air ->
            class "icon-air"

        Earth ->
            class "icon-earth"

        Military ->
            class "icon-military"

        Political ->
            class "icon-political"

        Scorpion ->
            class "icon-scorpion"

        Dragon ->
            class "icon-dragon"

        Unicorn ->
            class "icon-unicorn"

        Phoenix ->
            class "icon-phoenix"

        Lion ->
            class "icon-lion"

        Crane ->
            class "icon-crane"

        Crab ->
            class "icon-crab"

        Ph1 ->
            class "icon-ph1"

        Ph2 ->
            class "icon-ph2"

        Ph3 ->
            class "icon-ph3"

        Ph4 ->
            class "icon-ph4"

        Shadowlands ->
            class "icon-shadowlands"

        _ ->
            class ""
