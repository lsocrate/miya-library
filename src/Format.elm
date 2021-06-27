module Format exposing (Format(..), default, fromString, toString)


type Format
    = Stronghold RL
    | Skirmish
    | Enlightenment
    | Draft


type RL
    = ImperialLaw
    | JadeEdict


default : Format
default =
    Stronghold JadeEdict


toString : Format -> String
toString format =
    case format of
        Skirmish ->
            "skirmish"

        Enlightenment ->
            "enlightenment"

        Draft ->
            "draft"

        Stronghold ImperialLaw ->
            "imperial"

        Stronghold JadeEdict ->
            "jade"


fromString : String -> Maybe Format
fromString str =
    case str of
        "skirmish" ->
            Just Skirmish

        "enlightenment" ->
            Just Enlightenment

        "draft" ->
            Just Draft

        "imperial" ->
            Just <| Stronghold ImperialLaw

        "jade" ->
            Just <| Stronghold JadeEdict

        _ ->
            Nothing
