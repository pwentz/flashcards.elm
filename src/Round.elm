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

        Deck ((x :: xs) as cards) ->
            Just
                { deck = Deck cards
                , guesses = []
                , currentCard = Just x
                }


recordGuess : String -> Round -> Maybe Round
recordGuess input round =
    case (.currentCard round) of
        Nothing ->
            Nothing

        Just card ->
            let
                guess =
                    Guess.new input card

                updatedRound =
                    { round | guesses = (guess :: (.guesses round)) }
            in
                case guess of
                    IncorrectGuess ->
                        let
                            newDeck =
                                Deck (List.append (Deck.deckTail (.deck round)) [ card ])
                        in
                            Just
                                { updatedRound
                                    | deck = newDeck
                                    , currentCard = Deck.topCard newDeck
                                }

                    CorrectGuess ->
                        Just
                            { updatedRound
                                | deck = Deck (Deck.deckTail (.deck round))
                                , currentCard =
                                    .deck round
                                        |> Deck.deckTail
                                        |> List.head
                            }


percentCorrect : Round -> Float
percentCorrect round =
    let
        isCorrect guess =
            case guess of
                IncorrectGuess ->
                    False

                CorrectGuess ->
                    True

        numberCorrect =
            round.guesses
                |> List.filter isCorrect
                |> List.length
    in
        (toFloat numberCorrect) / toFloat (List.length (.guesses round)) * 100
