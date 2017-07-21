module Round exposing (..)

import Deck exposing (Deck(EmptyDeck, Deck))
import Card exposing (Card)
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))


type alias Round =
    { deck : Deck
    , guesses : List Guess
    , currentCard : Maybe Card
    }


new : Deck -> Maybe Round
new deck =
    case deck of
        EmptyDeck ->
            Nothing

        Deck [] ->
            Nothing

        Deck (x :: xs) ->
            Just
                { deck = Deck xs
                , guesses = []
                , currentCard = Just x
                }
