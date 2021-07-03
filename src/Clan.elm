module Clan exposing (Clan(..), comparable, fromString, toString)


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


fromString : String -> Maybe Clan
fromString str =
    case str of
        "crab" ->
            Just Crab

        "crane" ->
            Just Crane

        "dragon" ->
            Just Dragon

        "lion" ->
            Just Lion

        "phoenix" ->
            Just Phoenix

        "scorpion" ->
            Just Scorpion

        "unicorn" ->
            Just Unicorn

        "neutral" ->
            Just Neutral

        "shadowlands" ->
            Just Shadowlands

        _ ->
            Nothing
