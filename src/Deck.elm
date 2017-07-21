module Deck exposing (..)

import Card exposing (Card)


type Deck
    = EmptyDeck
    | Deck (List Card)
