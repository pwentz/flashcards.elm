module Guess exposing (..)

import Card exposing (Card)


type Guess
    = IncorrectGuess
    | CorrectGuess


type alias UserResponse =
    String


new : UserResponse -> Card -> Guess
new response card =
    if response == (.answer card) then
        CorrectGuess
    else
        IncorrectGuess
