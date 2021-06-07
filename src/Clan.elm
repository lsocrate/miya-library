module Clan exposing (Clan(..), comparable)


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
