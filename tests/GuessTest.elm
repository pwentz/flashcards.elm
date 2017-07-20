module GuessTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Guess exposing (..)
import Card


suite : Test
suite =
    describe "Guess"
        [ describe "new"
            [ test "it returns an IncorrectGuess if response matches answer" <|
                \_ ->
                    let
                        res =
                            "250 pounds"

                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            "Enough to break the ice!"
                    in
                        new res (Card.new q a)
                            |> Expect.equal IncorrectGuess
            , test "it returns a CorrectGuess if response does not match answer" <|
                \_ ->
                    let
                        res =
                            "Enough to break the ice!"

                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            res
                    in
                        new res (Card.new q a)
                            |> Expect.equal CorrectGuess
            ]
        , describe "isCorrect"
            [ test "it returns True if constructor is a CorrectGuess" <|
                \_ ->
                    let
                        res =
                            "Enough to break the ice!"

                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            res
                    in
                        new res (Card.new q a)
                            |> isCorrect
                            |> Expect.equal True
            , test "it returns False if constructor is IncorrectGuess" <|
                \_ ->
                    let
                        res =
                            "I don't know."

                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            "Enough to break the ice!"
                    in
                        new res (Card.new q a)
                            |> isCorrect
                            |> Expect.equal False
            ]
        ]
