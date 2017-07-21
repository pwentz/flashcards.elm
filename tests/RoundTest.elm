module RoundTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Card
import Guess exposing (Guess(CorrectGuess))
import Deck exposing (Deck(EmptyDeck, Deck))
import Round exposing (..)


suite : Test
suite =
    describe "Round"
        [ describe "new"
            [ test "it takes a deck and returns a RoundInProgress with currentCard and guesses already set" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck [ topCard ]

                        newRound =
                            RoundInProgress ({ deck = deck, guesses = [], currentCard = topCard })
                    in
                        new deck
                            |> Expect.equal newRound
            , test "it returns a FinishedRound if passed an EmptyDeck" <|
                \_ ->
                    new EmptyDeck
                        |> Expect.equal (FinishedRound { percentCorrect = 0.0 })
            , test "it returns Nothing if passed a Deck with an empty list" <|
                \_ ->
                    new (Deck [])
                        |> Expect.equal (FinishedRound { percentCorrect = 0.0 })
            ]
        , describe "recordGuess"
            [ test "it removes the top card of deck if guess is correct" <|
                \_ ->
                    let
                        bottomCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        d =
                            (Deck
                                [ { question = "How goes it?", answer = "Fine, thank you." }
                                , bottomCard
                                ]
                            )
                    in
                        new d
                            |> recordGuess "Fine, thank you."
                            |> getDeck
                            |> Expect.equal (Just (Deck [ bottomCard ]))
            , test "it places the top card of deck on the bottom if guess is incorrect" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        nextCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        d =
                            (Deck
                                [ topCard
                                , nextCard
                                ]
                            )
                    in
                        new d
                            |> recordGuess "Cool"
                            |> getDeck
                            |> Expect.equal (Just (Deck [ nextCard, topCard ]))
            , test "it sets the current card to equal the next card from the deck" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        nextCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        deck =
                            Deck ([ topCard, nextCard ])
                    in
                        new deck
                            |> (recordGuess "I'm not too sure")
                            |> getCurrentCard
                            |> Expect.equal (Just nextCard)
            , test "it creates a Guess record and updates guesses array" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck
                                ([ topCard
                                 , { question = "How goes it?", answer = "Fine, thank you." }
                                 ]
                                )

                        guess =
                            Guess.new "cool" topCard
                    in
                        new deck
                            |> recordGuess "Cool"
                            |> getGuesses
                            |> Expect.equal (Just [ guess ])
            , test "it returns a FinishedRound when deck has one card left and response is correct" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck
                                ([ card ])

                        guess =
                            Guess.new "Enough to break the ice!" card
                    in
                        new deck
                            |> recordGuess "Enough to break the ice!"
                            |> Expect.equal (FinishedRound { percentCorrect = 100.0 })
            , test "it does not change the currentCard or deck when one card left in deck and answer is incorrect" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck
                                ([ card ])

                        guess =
                            Guess.new "Wrong" card
                    in
                        new deck
                            |> recordGuess "Wrong"
                            |> Expect.equal
                                (RoundInProgress
                                    { deck = deck
                                    , currentCard = card
                                    , guesses = [ guess ]
                                    }
                                )
            ]
        ]
