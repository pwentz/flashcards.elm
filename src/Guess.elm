module Guess exposing (..)

import Card exposing (Card)


type Guess
    = IncorrectGuess
    | CorrectGuess


new : String -> Card -> Guess
new response card =
    if response == (.answer card) then
        CorrectGuess
    else
        IncorrectGuess
