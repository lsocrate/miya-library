module Numerical exposing (Numerical(..), toString)


type Numerical
    = Fixed Int
    | Dash
    | Variable


toString : Numerical -> String
toString num =
    case num of
        Fixed n ->
            String.fromInt n

        Dash ->
            "—"

        Variable ->
            "X"
