module Deck exposing (..)

import Card exposing (Card)


type Deck
    = EmptyDeck
    | Deck (List Card)


new : List Card -> Deck
new cards =
    if List.isEmpty cards then
        EmptyDeck
    else
        Deck cards


count : Deck -> Int
count deck =
    case deck of
        EmptyDeck ->
            0

        Deck xs ->
            List.length xs


topCard : Deck -> Maybe Card
topCard deck =
    case deck of
        EmptyDeck ->
            Nothing

        Deck xs ->
            List.head xs


deckTail : Deck -> List Card
deckTail deck =
    case deck of
        EmptyDeck ->
            []

        Deck [] ->
            []

        Deck (x :: xs) ->
            xs



-- getCards : Deck -> Maybe (List Card)
-- getCards deck =
--     case deck of
--         EmptyDeck ->
--             Nothing
--         Deck [] ->
--             Nothing
--         Deck cards ->
--             Just cards
