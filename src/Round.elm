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
            }
    in
        if Guess.isCorrect guess then
            { updatedRound
                | numberCorrect = round.numberCorrect + 1
                , currentCard =
                    List.tail round.deck
                        |> Maybe.andThen List.head
                        |> Maybe.withDefault Deck.defaultCard
                , deck =
                    List.tail round.deck
                        |> Maybe.withDefault []
            }
        else
            let
                newDeck =
                    Deck.topCardToBottom (.deck round)
            in
                { updatedRound
                    | deck = newDeck
                    , currentCard =
                        Deck.topCard newDeck
                }


percentCorrect : Round -> Float
percentCorrect round =
    toFloat round.numberCorrect / (toFloat (List.length round.guesses)) * 100
