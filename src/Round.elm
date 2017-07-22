module Round exposing (..)

import Deck exposing (Deck)
import Card exposing (Card)
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))


type alias Guesses =
    List Guess


type alias ProgressData =
    { deck : Deck
    , guesses : Guesses
    }


type alias RoundSummary =
    { percentCorrect : Float }


type Round
    = FinishedRound RoundSummary
    | RoundInProgress ProgressData


new : ProgressData -> Round
new { deck, guesses } =
    case deck of
        [] ->
            (FinishedRound
                { percentCorrect = getPercentage guesses }
            )

        cards ->
            (RoundInProgress
                { deck = cards
                , guesses = guesses
                }
            )


deck : Round -> Maybe Deck
deck round =
    case round of
        FinishedRound _ ->
            Nothing

        RoundInProgress { deck, guesses } ->
            Just deck


currentCard : Round -> Maybe Card
currentCard round =
    case round of
        FinishedRound _ ->
            Nothing

        RoundInProgress { deck, guesses } ->
            case deck of
                [] ->
                    Nothing

                cards ->
                    List.head cards


getGuesses : Round -> Maybe Guesses
getGuesses round =
    case round of
        FinishedRound _ ->
            Nothing

        RoundInProgress { deck, guesses } ->
            Just guesses


recordGuess : Guess -> Round -> Round
recordGuess guess round =
    case round of
        FinishedRound _ ->
            round

        RoundInProgress { deck, guesses } ->
            let
                roundData =
                    { deck = deck
                    , guesses = guess :: guesses
                    }
            in
                case guess of
                    IncorrectGuess ->
                        new
                            { roundData
                                | deck =
                                    List.take 1 deck
                                        |> List.append (List.drop 1 deck)
                            }

                    CorrectGuess ->
                        new
                            { roundData
                                | deck = List.drop 1 deck
                            }


getPercentage : Guesses -> Float
getPercentage guesses =
    let
        isCorrect guess =
            case guess of
                IncorrectGuess ->
                    False

                CorrectGuess ->
                    True

        numberCorrect =
            guesses
                |> List.filter isCorrect
                |> List.length
    in
        (toFloat numberCorrect) / toFloat (List.length guesses) * 100
