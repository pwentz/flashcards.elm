module Round exposing (..)

import Deck exposing (Deck)
import Card exposing (Card)
import Guess exposing (Guess)


type Round
    = Round
        { deck : Deck
        , guesses : List Guess
        , currentCard : Maybe Card
        , numberCorrect : Int
        }


new : Deck -> Round
new deck =
    Round
        { deck = deck
        , guesses = []
        , currentCard = (List.head << Deck.cards) deck
        , numberCorrect = 0
        }
