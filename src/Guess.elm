module Guess exposing (..)

import Card exposing (Card)


type Guess
    = IncorrectGuess
    | CorrectGuess


new : String -> Card -> Guess
new response card =
    if response == (Card.answer card) then
        CorrectGuess
    else
        IncorrectGuess


isCorrect : Guess -> Bool
isCorrect guess =
    case guess of
        IncorrectGuess ->
            False

        CorrectGuess ->
            True
