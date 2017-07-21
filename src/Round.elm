module Round exposing (..)

import Deck exposing (Deck)
import Card exposing (Card)
import Guess exposing (Guess)


type alias Round =
    { deck : Deck
    , guesses : List Guess
    , currentCard : Card
    , numberCorrect : Int
    }


recordGuess : Round -> String -> Round
recordGuess round input =
    let
        guess =
            { response = input, card = round.currentCard }

        updatedRound =
            { round
                | guesses = guess :: round.guesses
                , currentCard =
                    List.tail round.deck
                        |> Maybe.andThen List.head
                        |> Maybe.withDefault { question = "", answer = "" }
            }
    in
        if Guess.isCorrect guess then
            { updatedRound
                | numberCorrect = updatedRound.numberCorrect + 1
                , deck = Maybe.withDefault [] (List.tail updatedRound.deck)
            }
        else
            { updatedRound
                | deck = List.append (List.drop 1 updatedRound.deck) (List.take 1 updatedRound.deck)
            }


percentCorrect : Round -> Float
percentCorrect round =
    toFloat round.numberCorrect / (toFloat (List.length round.guesses)) * 100
