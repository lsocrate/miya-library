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
    | Shadowlands


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

        Shadowlands ->
            "Shadowlands"


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
