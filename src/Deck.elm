module Deck exposing (..)

import Card exposing (Card)


type Deck
    = Deck (List Card)


new : List Card -> Deck
new cards =
    Deck cards


cards : Deck -> List Card
cards (Deck cs) =
    cs


count : Deck -> Int
count (Deck cs) =
    List.length cs
