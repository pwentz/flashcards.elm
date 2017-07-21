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



-- getCards : Deck -> Maybe (List Card)
-- getCards deck =
--     case deck of
--         EmptyDeck ->
--             Nothing
--         Deck [] ->
--             Nothing
--         Deck cards ->
--             Just cards
