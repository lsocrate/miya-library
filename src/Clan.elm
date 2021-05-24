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


name : Clan -> String
name clan =
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


icon : Clan -> String
icon clan =
    case clan of
        Crab ->
            "🦀"

        Crane ->
            "🦢"

        Dragon ->
            "🐉"

        Lion ->
            "🦁"

        Phoenix ->
            "🐣"

        Scorpion ->
            "🦂"

        Unicorn ->
            "🦄"

        Neutral ->
            "✨"

        Shadowlands ->
            "👹"


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


mon : Clan -> String
mon clan =
    case clan of
        Crab ->
            "/assets/mon-crab.svg"

        Crane ->
            "/assets/mon-crane.svg"

        Dragon ->
            "/assets/mon-dragon.svg"

        Lion ->
            "/assets/mon-lion.svg"

        Phoenix ->
            "/assets/mon-phoenix.svg"

        Scorpion ->
            "/assets/mon-scorpion.svg"

        Unicorn ->
            "/assets/mon-unicorn.svg"

        Neutral ->
            "/assets/mon-neutral.svg"

        Shadowlands ->
            "/assets/mon-neutral.svg"
