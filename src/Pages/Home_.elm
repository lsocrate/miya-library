module Pages.Home_ exposing (view)

import Components.Header
import Html exposing (..)
import Html.Attributes exposing (..)
import View exposing (View)


view : View msg
view =
    { title = "Miya Library"
    , body =
        [ Components.Header.view
        , text "Hello, world!"
        ]
    }
