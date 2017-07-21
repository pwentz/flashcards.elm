module Round exposing (..)

import Deck exposing (Deck(EmptyDeck, Deck))
import Card exposing (Card)
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))


type Round
    = FinishedRound { percentCorrect : Float }
    | RoundInProgress
        { deck : Deck
        , guesses : List Guess
        , currentCard : Card
        }


new : Deck -> Round
new deck =
    case deck of
        EmptyDeck ->
            (FinishedRound { percentCorrect = 0.0 })

        Deck [] ->
            (FinishedRound { percentCorrect = 0.0 })

        Deck ((x :: xs) as cards) ->
            (RoundInProgress
                { deck = Deck cards
                , guesses = []
                , currentCard = x
                }
            )


getDeck : Round -> Maybe Deck
getDeck round =
    case round of
        FinishedRound _ ->
            Nothing

        RoundInProgress { deck, guesses, currentCard } ->
            Just deck


getCurrentCard : Round -> Maybe Card
getCurrentCard round =
    case round of
        FinishedRound _ ->
            Nothing

        RoundInProgress { deck, guesses, currentCard } ->
            Just currentCard


getGuesses : Round -> Maybe (List Guess)
getGuesses round =
    case round of
        FinishedRound _ ->
            Nothing

        RoundInProgress { deck, guesses, currentCard } ->
            Just guesses


recordGuess : String -> Round -> Round
recordGuess input round =
    case round of
        FinishedRound _ ->
            round

        RoundInProgress { deck, guesses, currentCard } ->
            let
                guess =
                    Guess.new input currentCard

                updatedGuesses =
                    guess :: guesses

                nextUp =
                    List.head (Deck.deckTail deck)
            in
                case ( guess, nextUp ) of
                    ( IncorrectGuess, maybeCard ) ->
                        RoundInProgress
                            { deck = Deck (List.append (Deck.deckTail deck) [ currentCard ])
                            , guesses = updatedGuesses
                            , currentCard = Maybe.withDefault currentCard maybeCard
                            }

                    ( CorrectGuess, Just card ) ->
                        RoundInProgress
                            { deck = Deck (Deck.deckTail deck)
                            , guesses = updatedGuesses
                            , currentCard = card
                            }

                    ( CorrectGuess, Nothing ) ->
                        FinishedRound
                            { percentCorrect = getPercentage updatedGuesses }


getPercentage : List Guess -> Float
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
