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

                        roundData =
                            { deck = Deck.new [ topCard ]
                            , guesses = [ IncorrectGuess, CorrectGuess ]
                            }
                    in
                        new roundData
                            |> guesses
                            |> Expect.equal (Just [ IncorrectGuess, CorrectGuess ])
            , test "it returns a finished round w/ pct calculated if deck passed is an EmptyDeck" <|
                \_ ->
                    let
                        roundData =
                            { deck = Deck.new []
                            , guesses = [ IncorrectGuess, CorrectGuess ]
                            }

                        expectedFn { percentCorrect } =
                            percentCorrect

                        failFn _ =
                            9.9999
                    in
                        new roundData
                            |> either failFn expectedFn
                            |> Expect.equal 50.0
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
                            (Deck.new
                                [ topCard
                                , bottomCard
                                ]
                            )
                    in
                        new { deck = d, guesses = [] }
                            |> recordGuess guess
                            |> getDeck
                            |> Expect.equal (Just (Deck.new [ bottomCard ]))
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
                            Deck.new
                                [ topCard
                                , nextCard
                                ]
                    in
                        new { deck = d, guesses = [] }
                            |> recordGuess guess
                            |> getDeck
                            |> Expect.equal (Just (Deck.new [ nextCard, topCard ]))
            , test "it sets the current card to equal the next card from the deck" <|
                \_ ->
                    let
                        topCard =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        nextCard =
                            { question = "How goes it?", answer = "Fine, thank you." }

                        guess =
                            Guess.new "something something" topCard

                        deck =
                            Deck.new [ topCard, nextCard ]
                    in
                        new { deck = deck, guesses = [] }
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

                        deck =
                            Deck.new
                                [ topCard
                                , { question = "How goes it?", answer = "Fine, thank you." }
                                ]
                    in
                        new { deck = deck, guesses = [] }
                            |> recordGuess guess
                            |> guesses
                            |> Expect.equal (Just [ guess ])
            , test "it returns a FinishedRound when deck has one card left and response is correct" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck.new
                                ([ card ])

                        failFn =
                            (\_ -> "this means failure")

                        successFn =
                            (\_ -> "this means success")

                        guess =
                            Guess.new "Enough to break the ice!" card
                    in
                        new { deck = deck, guesses = [] }
                            |> recordGuess guess
                            |> either failFn successFn
                            |> Expect.equal "this means success"
            , test "it does not change the deck when one card left in deck and answer is incorrect" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        deck =
                            Deck.new
                                ([ card ])

                        failFn =
                            (\_ -> "this means failure")

                        successFn =
                            (\_ -> "this means success")

                        guess =
                            Guess.new "Wrong" card
                    in
                        new { deck = deck, guesses = [] }
                            |> recordGuess guess
                            |> either successFn failFn
                            |> Expect.equal "this means success"
            ]
        , describe "either"
            [ test "it takes a ({deck, guesses} -> a) fn and a ({percentCorrect} -> a) fn and a round and returns a" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        primaryFn =
                            (\{ deck, guesses } -> Deck.topCard deck)

                        defaultFn =
                            (\{ percentCorrect } -> Nothing)

                        round =
                            new
                                { deck = Deck.new [ card ]
                                , guesses = []
                                }
                    in
                        round
                            |> either primaryFn defaultFn
                            |> Expect.equal (Just card)
            , test "it runs the secondaryFn if Round is over" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }

                        primaryFn =
                            (\{ deck, guesses } -> "this function was not")

                        defaultFn =
                            (\{ percentCorrect } -> "this function was run")
                    in
                        new
                            { deck = (Deck.new [])
                            , guesses = []
                            }
                            |> either primaryFn defaultFn
                            |> Expect.equal "this function was run"
            ]
        ]
