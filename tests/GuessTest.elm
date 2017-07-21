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
            [ test "it returns a CorrectGuess if answer on given card matches given response" <|
                \_ ->
                    let
                        correctResponse =
                            "Enough to break the ice!"

                        card =
                            { question = "How much does a polar bear weigh?", answer = correctResponse }
                    in
                        new correctResponse card
                            |> Expect.equal CorrectGuess
            , test "it returns an IncorrectGuess if answer does not match given response" <|
                \_ ->
                    let
                        card =
                            { question = "How much does a polar bear weigh?", answer = "Enough to break the ice!" }
                    in
                        new "something something" card
                            |> Expect.equal IncorrectGuess
            ]
        ]
