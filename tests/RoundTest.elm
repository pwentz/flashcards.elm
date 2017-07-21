module RoundTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Card
import Guess
import Deck exposing (Deck(EmptyDeck, Deck))
import Round exposing (..)


suite : Test
suite =
    describe "Round"
        [ describe "new"
            [ test "it takes a deck and returns a Maybe Round with currentCard and guesses already set" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck [ topCard ]

                        newRound =
                            { deck = Deck [], guesses = [], currentCard = Just topCard }
                    in
                        new deck
                            |> Expect.equal (Just newRound)
            , test "it returns Nothing if passed an EmptyDeck" <|
                \_ ->
                    new EmptyDeck
                        |> Expect.equal Nothing
            , test "it returns Nothing if passed a Deck with an empty list" <|
                \_ ->
                    new (Deck [])
                        |> Expect.equal Nothing
            ]

        -- describe "recordGuess"
        -- [
        -- test "it removes the top card of deck if guess is correct" <|
        --   \_ ->
        --       let
        --           topCard =
        --               { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
        --           deck =
        --               [ topCard
        --               , { question = "How goes it?", answer = "Fine, thank you." }
        --               ]
        --           round =
        --               { deck = deck, guesses = [], currentCard = topCard }
        --       in
        --           recordGuess round "Enough to break the ice!"
        --               |> .deck
        --               |> Deck.count
        --               |> Expect.equal 1
        -- , test "it places the top card of deck on the bottom if guess is incorrect" <|
        --     \_ ->
        --         let
        --             topCard =
        --                 { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
        --             deck =
        --                 [ topCard
        --                 , { question = "How goes it?", answer = "Fine, thank you." }
        --                 ]
        --             round =
        --                 { deck = deck, guesses = [], currentCard = topCard }
        --         in
        --             recordGuess round "Cool"
        --                 |> .deck
        --                 |> List.tail
        --                 |> Expect.equal (Just [ topCard ])
        -- , test "it sets the current card to equal the next card from the deck" <|
        --     \_ ->
        --         let
        --             topCard =
        --                 { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
        --             nextCard =
        --                 { question = "How goes it?", answer = "Fine, thank you." }
        --             deck =
        --                 [ topCard
        --                 , nextCard
        --                 ]
        --             round =
        --                 { deck = deck, guesses = [], currentCard = topCard }
        --         in
        --             recordGuess round "I'm not too sure"
        --                 |> .currentCard
        --                 |> Expect.equal nextCard
        -- , test "it creates a Guess record and updates guesses array" <|
        --     \_ ->
        --         let
        --             topCard =
        --                 { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
        --             deck =
        --                 [ topCard
        --                 , { question = "How goes it?", answer = "Fine, thank you." }
        --                 ]
        --             guess =
        --                 { response = "Cool", card = topCard }
        --             round =
        --                 { deck = deck, guesses = [], currentCard = topCard }
        --         in
        --             recordGuess round "Cool"
        --                 |> .guesses
        --                 |> Expect.equal [ guess ]
        -- , test "it puts a blank card as next currentCard when passed a round with an empty deck" <|
        --     \_ ->
        --         let
        --             topCard =
        --                 { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
        --             guess =
        --                 { response = "Cool", card = topCard }
        --             round =
        --                 { deck = [], guesses = [], currentCard = topCard }
        --         in
        --             recordGuess round "Cool"
        --                 |> .currentCard
        --                 |> Expect.equal { question = "", answer = "" }
        -- ]
        -- , describe "percentCorrect"
        -- [ test "it returns the pct of guesses that are correct" <|
        --     \_ ->
        --         let
        --             topCard =
        --                 { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
        --             deck =
        --                 [ topCard
        --                 , { question = "How goes it?", answer = "Fine, thank you." }
        --                 ]
        --             round =
        --                 { deck = deck, guesses = [], currentCard = topCard }
        --         in
        --             recordGuess round "Enough to break the ice!"
        --                 |> percentCorrect
        --                 |> Expect.equal 100.0
        -- ]
        -- , describe "numberOfCorrectGuesses" <|
        -- [ test "it returns the number of guesses made correctly" <|
        --     \_ ->
        --         let
        --             topCard =
        --                 { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
        --             deck =
        --                 [ topCard
        --                 , { question = "How goes it?", answer = "Fine, thank you." }
        --                 ]
        --             round =
        --                 { deck = deck, guesses = [], currentCard = topCard }
        --         in
        --             recordGuess round "Enough to break the ice!"
        --                 |> numberOfCorrectGuesses
        --                 |> Expect.equal 1
        -- ]
        ]
