module RoundTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Card
import Guess exposing (Guess(CorrectGuess, IncorrectGuess))
import Deck exposing (Deck)
import Round exposing (..)


suite : Test
suite =
    describe "Round"
        [ describe "new"
            [ test "it takes a deck and some guesses and returns a RoundInProgress" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guesses =
                            [ CorrectGuess, IncorrectGuess ]

                        d =
                            [ topCard ]

                        newRound =
                            RoundInProgress ({ deck = d, guesses = guesses })
                    in
                        { deck = d, guesses = guesses }
                            |> new
                            |> Expect.equal newRound
            , test "it returns a finished round w/ guesses calculated if passed empty deck" <|
                \_ ->
                    let
                        guesses =
                            [ CorrectGuess, IncorrectGuess ]
                    in
                        { deck = [], guesses = guesses }
                            |> new
                            |> Expect.equal (FinishedRound { percentCorrect = 50.0 })
            ]
        , describe "recordGuess"
            [ test "it removes the top card of deck if guess is correct" <|
                \_ ->
                    let
                        topCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        guess =
                            Guess.new "Fine, thank you." topCard

                        bottomCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        d =
                            [ topCard
                            , bottomCard
                            ]
                    in
                        { deck = d, guesses = [] }
                            |> new
                            |> recordGuess guess
                            |> deck
                            |> Expect.equal (Just [ bottomCard ])
            , test "it places the top card of deck on the bottom if guess is incorrect" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        nextCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        guess =
                            Guess.new "something something ice" topCard

                        d =
                            [ topCard
                            , nextCard
                            ]
                    in
                        { deck = d, guesses = [] }
                            |> new
                            |> recordGuess guess
                            |> deck
                            |> Expect.equal (Just [ nextCard, topCard ])
            , test "it sets the current card to equal the next card from the deck" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        nextCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        guess =
                            Guess.new "something something" topCard

                        d =
                            [ topCard, nextCard ]
                    in
                        { deck = d, guesses = [] }
                            |> new
                            |> (recordGuess guess)
                            |> currentCard
                            |> Expect.equal (Just nextCard)
            , test "it updates guesses array" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            Guess.new "Cool" topCard

                        d =
                            [ topCard
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]
                    in
                        { deck = d, guesses = [] }
                            |> new
                            |> recordGuess guess
                            |> getGuesses
                            |> Expect.equal (Just [ guess ])
            , test "it returns a FinishedRound when deck has one card left and response is correct" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        d =
                            [ card ]

                        guess =
                            Guess.new "Enough to break the ice!" card
                    in
                        { deck = d, guesses = [] }
                            |> new
                            |> recordGuess guess
                            |> Expect.equal (FinishedRound { percentCorrect = 100.0 })
            , test "it does not change the deck when one card left in deck and answer is incorrect" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        d =
                            [ card ]

                        guess =
                            Guess.new "Wrong" card
                    in
                        { deck = d, guesses = [] }
                            |> new
                            |> recordGuess guess
                            |> Expect.equal
                                (RoundInProgress
                                    { deck = d
                                    , guesses = [ guess ]
                                    }
                                )
            ]
        ]
