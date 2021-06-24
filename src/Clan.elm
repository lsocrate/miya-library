module Clan exposing (Clan(..), comparable, toString)


type Clan
    = Crab
    | Crane
    | Dragon
    | Lion
    | Phoenix
    | Scorpion
    | Unicorn
    | Neutral
    | Shadowlands


comparable : Clan -> Int
comparable clan =
    case clan of
        Crab ->
            1

        Crane ->
            2

        Dragon ->
            3

        Lion ->
            4

        Phoenix ->
            5

        Scorpion ->
            6

        Unicorn ->
            7

        Neutral ->
            8

        Shadowlands ->
            9


toString : Clan -> String
toString clan =
    case clan of
        Crab ->
            "crab"

        Crane ->
            "crane"

        Dragon ->
            "dragon"

        Lion ->
            "lion"

        Phoenix ->
            "phoenix"

        Scorpion ->
            "scorpion"

        Unicorn ->
            "unicorn"

        Neutral ->
            "neutral"

        Shadowlands ->
            "shadowlands"
