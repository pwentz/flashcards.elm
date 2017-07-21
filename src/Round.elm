module Round exposing (..)

import Deck exposing (Deck)
import Card exposing (Card)
import Guess exposing (Guess)


type alias Round =
    { deck : Deck
    , guesses : List Guess
    }



-- DO WE NEED A DEFAULT CARD STILL?


currentCard : Round -> Card
currentCard round =
    .deck round
        |> List.head
        |> Maybe.withDefault Deck.defaultCard


recordGuess : Round -> Guess -> Round
recordGuess round guess =
    let
        updatedRound =
            { round
                | guesses = guess :: round.guesses
            }
    in
        if Guess.isCorrect guess then
            { updatedRound
                | deck =
                    List.drop 1 round.deck
            }
        else
            { updatedRound | deck = Deck.rotate (.deck round) }


isOver : Round -> Bool
isOver round =
    .deck round
        |> List.length
        |> ((==) 0)


percentCorrect : Round -> Float
percentCorrect round =
    toFloat (numberOfCorrectGuesses round) / (toFloat (List.length round.guesses)) * 100


numberOfCorrectGuesses : Round -> Int
numberOfCorrectGuesses round =
    round.guesses
        |> List.filter Guess.isCorrect
        |> List.length
