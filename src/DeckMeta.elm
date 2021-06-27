module DeckMeta exposing (..)

import Format exposing (Format)


type alias DeckMeta =
    { id : String
    , authorId : String
    , format : Format
    , name : Maybe String
    , description : Maybe String
    }
