module Deck exposing (..)

import Card exposing (Card)


type alias Deck =
    List Card


count : Deck -> Int
count cards =
    List.length cards


defaultCard : Card
defaultCard =
    { question = "", answer = "" }


rotate : Deck -> Deck
rotate deck =
    List.append (List.drop 1 deck) (List.take 1 deck)


topCard : Deck -> Card
topCard deck =
    List.head deck
        |> Maybe.withDefault defaultCard
