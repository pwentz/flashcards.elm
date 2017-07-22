module Round
    exposing
        ( Round
        , new
        , either
        , recordGuess
        , guesses
        , getDeck
        , getPercentage
        , currentCard
        )

import Deck exposing (Deck)
import Card exposing (Card)
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))


type Round
    = FinishedRound { percentCorrect : Float }
    | RoundInProgress
        { deck : Deck
        , guesses : List Guess
        }


type alias Guesses =
    List Guess


type alias Percent =
    Float


type alias ProgressData =
    { deck : Deck, guesses : Guesses }


type alias RoundSummary =
    { percentCorrect : Percent }


new : ProgressData -> Round
new { deck, guesses } =
    case Deck.topCard deck of
        Nothing ->
            FinishedRound
                { percentCorrect = getPercentage guesses }

        Just _ ->
            RoundInProgress
                { deck = deck
                , guesses = guesses
                }


either : (ProgressData -> a) -> (RoundSummary -> a) -> Round -> a
either onInProgress onRoundOver round =
    case round of
        RoundInProgress progressData ->
            onInProgress progressData

        FinishedRound roundSummary ->
            onRoundOver roundSummary


getDeck : Round -> Maybe Deck
getDeck round =
    let
        onRoundOver _ =
            Nothing

        onInProgress { deck, guesses } =
            Just deck
    in
        either onInProgress onRoundOver round


currentCard : Round -> Maybe Card
currentCard round =
    let
        onRoundOver _ =
            Nothing

        onInProgress { deck, guesses } =
            Deck.topCard deck
    in
        either onInProgress onRoundOver round


guesses : Round -> Maybe Guesses
guesses round =
    let
        onRoundOver _ =
            Nothing

        onInProgress { deck, guesses } =
            Just guesses
    in
        either onInProgress onRoundOver round


recordGuess : Guess -> Round -> Round
recordGuess guess round =
    let
        onRoundOver _ =
            round

        onInProgress { deck, guesses } =
            let
                updatedGuesses =
                    guess :: guesses

                rotate cards =
                    List.take 1 cards
                        |> List.append (List.drop 1 cards)
            in
                case guess of
                    IncorrectGuess ->
                        new
                            { guesses = updatedGuesses
                            , deck =
                                Deck.map rotate deck
                            }

                    CorrectGuess ->
                        new
                            { guesses = updatedGuesses
                            , deck =
                                deck
                                    |> Deck.map (List.drop 1)
                            }
    in
        either onInProgress onRoundOver round


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
