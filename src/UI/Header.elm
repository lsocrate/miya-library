module UI.Header exposing (..)

import Gen.Route as Route
import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    header [ class "header" ]
        [ div [ class "header-logo" ]
            [ text "Miya Library"
            ]
        , nav [ class "nav" ]
            [ ul [ class "nav-list" ]
                [ li [ class "nav-item" ]
                    [ a [ href (Route.toHref Route.Deckbuilder) ]
                        [ text "Deckbuilder"
                        ]
                    ]
                ]
            ]
        ]
