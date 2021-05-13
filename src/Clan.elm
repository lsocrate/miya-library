module Clan exposing (..)


type Clan
    = Crab
    | Crane
    | Dragon
    | Lion
    | Phoenix
    | Scorpion
    | Unicorn
    | Neutral


clanName : Clan -> String
clanName clan =
    case clan of
        Crab ->
            "Crab"

        Crane ->
            "Crane"

        Dragon ->
            "Dragon"

        Lion ->
            "Lion"

        Phoenix ->
            "Phoenix"

        Scorpion ->
            "Scorpion"

        Unicorn ->
            "Unicorn"

        Neutral ->
            "Neutral"
