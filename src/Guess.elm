module Guess exposing (..)

import Card exposing (Card)


type alias Guess =
    { response : String, card : Card }


isCorrect : Guess -> Bool
isCorrect guess =
    guess.response == guess.card.answer
