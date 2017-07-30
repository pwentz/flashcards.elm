module Deck
    exposing
        ( Deck
        , new
        , map
        , topCard
        , rotate
        )

import Card exposing (Card)


type Deck
    = EmptyDeck
    | Deck Cards


type alias Cards =
    List Card


new : Cards -> Deck
new cards =
    case cards of
        [] ->
            EmptyDeck

        xs ->
            Deck xs


map : (Cards -> Cards) -> Deck -> Deck
map f deck =
    case deck of
        EmptyDeck ->
            EmptyDeck

        Deck xs ->
            Deck (f xs)


topCard : Deck -> Maybe Card
topCard deck =
    case deck of
        EmptyDeck ->
            Nothing

        Deck [] ->
            Nothing

        Deck (x :: _) ->
            Just x


rotate : Deck -> Deck
rotate deck =
    map
        (\xs ->
            List.append (List.drop 1 xs) (List.take 1 xs)
        )
        deck
