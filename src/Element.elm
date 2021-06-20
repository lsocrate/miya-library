module Element exposing (Element(..), fromString, list, name, toString)


type Element
    = Air
    | Earth
    | Fire
    | Void
    | Water


list : List Element
list =
    [ Air, Earth, Water, Fire, Void ]


toString : Element -> String
toString el =
    case el of
        Air ->
            "air"

        Earth ->
            "earth"

        Fire ->
            "fire"

        Void ->
            "void"

        Water ->
            "water"


fromString : String -> Maybe Element
fromString str =
    case str of
        "air" ->
            Just Air

        "earth" ->
            Just Earth

        "fire" ->
            Just Fire

        "void" ->
            Just Void

        "water" ->
            Just Water

        _ ->
            Nothing


name : Element -> String
name el =
    case el of
        Air ->
            "Air"

        Earth ->
            "Earth"

        Fire ->
            "Fire"

        Void ->
            "Void"

        Water ->
            "Water"
