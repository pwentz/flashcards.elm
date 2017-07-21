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
            [ test "it takes a deck and returns a Maybe Round with currentCard and guesses already set" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck [ topCard ]

                        newRound =
                            { deck = deck, guesses = [], currentCard = Just topCard }
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
        , describe "recordGuess"
            [ test "it removes the top card of deck if guess is correct" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            (Deck
                                [ topCard
                                , { question = "How goes it?", answer = "Fine, thank you." }
                                ]
                            )

                        round =
                            { deck = deck, guesses = [], currentCard = topCard }
                    in
                        new deck
                            |> Maybe.andThen (recordGuess "Enough to break the ice!")
                            |> Maybe.map .deck
                            |> Maybe.map Deck.count
                            |> Expect.equal (Just 1)
            , test "it places the top card of deck on the bottom if guess is incorrect" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        nextCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        deck =
                            (Deck
                                [ topCard
                                , nextCard
                                ]
                            )

                        round =
                            { deck = deck, guesses = [], currentCard = topCard }
                    in
                        new deck
                            |> Maybe.andThen (recordGuess "Cool")
                            |> Maybe.map .deck
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

                        round =
                            { deck = deck, guesses = [], currentCard = topCard }
                    in
                        new deck
                            |> Maybe.andThen (recordGuess "I'm not too sure")
                            |> Maybe.andThen .currentCard
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
                            |> Maybe.andThen (recordGuess "Cool")
                            |> Maybe.map .guesses
                            |> Expect.equal (Just [ guess ])
            , test "it returns Nothing when round has no current card" <|
                \_ ->
                    let
                        round =
                            { deck = EmptyDeck, guesses = [], currentCard = Nothing }
                    in
                        recordGuess "Cool" round
                            |> Expect.equal Nothing
            , test "it sets its currentCard to Nothing if only one card left in deck and answer is correct" <|
                \_ ->
                    let
                        deck =
                            Deck
                                ([ { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" } ])
                    in
                        new deck
                            |> Maybe.andThen (recordGuess "Enough to break the ice!")
                            |> Expect.equal (Just { deck = Deck [], guesses = [ CorrectGuess ], currentCard = Nothing })
            , test "it does not change the currentCard when one card left in deck and answer is incorrect" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck
                                ([ card ])
                    in
                        new deck
                            |> Maybe.andThen (recordGuess "Wrong")
                            |> Maybe.andThen .currentCard
                            |> Expect.equal (Just card)
            ]
        , describe "percentCorrect"
            [ test "it returns the pct of guesses that are correct" <|
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
                    in
                        new deck
                            |> Maybe.andThen (recordGuess "Enough to break the ice!")
                            |> Maybe.map percentCorrect
                            |> Expect.equal (Just 100.0)
            ]
        ]
