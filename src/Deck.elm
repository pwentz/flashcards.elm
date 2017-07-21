module Deck exposing (..)

import Card exposing (Card)


type alias Deck =
    List Card


count : Deck -> Int
count cards =
    List.length cards
