module RoundTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Card
import Guess
import Deck
import Round exposing (..)


suite : Test
suite =
    describe "Round"
        [ describe "currentCard"
            [ test "it returns the top card from the deck" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            [ topCard
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]

                        round =
                            { deck = deck, guesses = [] }
                    in
                        currentCard round
                            |> Expect.equal topCard
            , test "it returns a default card if deck is empty" <|
                \_ ->
                    let
                        round =
                            { deck = [], guesses = [] }

                        defaultCard =
                            { question = "", answer = "" }
                    in
                        currentCard round
                            |> Expect.equal defaultCard
            ]
        , describe "recordGuess"
            [ test "it removes the top card of deck if guess is correct" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "Enough to break the ice!", card = topCard }

                        deck =
                            [ topCard
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]

                        round =
                            { deck = deck, guesses = [] }
                    in
                        recordGuess round guess
                            |> .deck
                            |> Deck.count
                            |> Expect.equal 1
            , test "it places the top card of deck on the bottom if guess is incorrect" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "Cool", card = topCard }

                        deck =
                            [ topCard
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]

                        round =
                            { deck = deck, guesses = [] }
                    in
                        recordGuess round guess
                            |> .deck
                            |> List.tail
                            |> Expect.equal (Just [ topCard ])
            , test "it sets the current card to equal the next card from the deck" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "I'm not too sure", card = topCard }

                        nextCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        deck =
                            [ topCard
                            , nextCard
                            ]

                        round =
                            { deck = deck, guesses = [] }
                    in
                        recordGuess round guess
                            |> currentCard
                            |> Expect.equal nextCard
            , test "it updates guesses array" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            [ topCard
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]

                        guess =
                            { response = "Cool", card = topCard }

                        round =
                            { deck = deck, guesses = [] }
                    in
                        recordGuess round guess
                            |> .guesses
                            |> Expect.equal [ guess ]
            , test "it puts a blank card as next currentCard when passed a round with an empty deck" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "Cool", card = topCard }

                        round =
                            { deck = [], guesses = [] }
                    in
                        recordGuess round guess
                            |> currentCard
                            |> Expect.equal { question = "", answer = "" }
            , test "it puts a blank card as next currentCard when guess is correct and deck has 1 card left" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "Enough to break the ice!", card = topCard }

                        round =
                            { deck = [ topCard ], guesses = [] }
                    in
                        recordGuess round guess
                            |> currentCard
                            |> Expect.equal { question = "", answer = "" }
            , test "it keeps the same current card if guess is incorrect and deck has 1 card left" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "something something ice", card = topCard }

                        round =
                            { deck = [ topCard ], guesses = [] }
                    in
                        recordGuess round guess
                            |> currentCard
                            |> Expect.equal topCard
            ]
        , describe "percentCorrect"
            [ test "it returns the pct of guesses that are correct" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "Enough to break the ice!", card = topCard }

                        deck =
                            [ topCard
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]

                        round =
                            { deck = deck, guesses = [] }
                    in
                        recordGuess round guess
                            |> percentCorrect
                            |> Expect.equal 100.0
            ]
        , describe "numberOfCorrectGuesses" <|
            [ test "it returns the number of guesses made correctly" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        guess =
                            { response = "Enough to break the ice!", card = topCard }

                        deck =
                            [ topCard
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]

                        round =
                            { deck = deck, guesses = [] }
                    in
                        recordGuess round guess
                            |> numberOfCorrectGuesses
                            |> Expect.equal 1
            ]
        , describe "isOver" <|
            [ test "is returns True if deck has no cards in it" <|
                \_ ->
                    let
                        round =
                            { deck = [], guesses = [] }
                    in
                        Round.isOver round
                            |> Expect.equal True
            , test "it returns False if deck has cards in it" <|
                \_ ->
                    let
                        someCard =
                            { question = "What if God was one of us?", answer = "?" }

                        round =
                            { deck = [ someCard ], guesses = [] }
                    in
                        Round.isOver round
                            |> Expect.equal False
            ]
        ]
