module Pages.Home_ exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import UI.Header
import View exposing (View)


view : View msg
view =
    { title = "Miya Library"
    , body =
        [ UI.Header.view
        , text "Hello, world!"
        ]
    }
