module GuessTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Guess exposing (..)
import Card


suite : Test
suite =
    describe "Guess"
        [ describe "isCorrect"
            [ test "it returns True if constructor is a CorrectGuess" <|
                \_ ->
                    let
                        res =
                            "Enough to break the ice!"

                        card =
                            { question = "How much does a Polar Bear weigh?"
                            , answer = res
                            }
                    in
                        { response = res, card = card }
                            |> isCorrect
                            |> Expect.equal True
            , test "it returns False if constructor is IncorrectGuess" <|
                \_ ->
                    let
                        res =
                            "No thank you"

                        card =
                            { question = "How much does a Polar Bear weigh?"
                            , answer = "Enough to break the ice!"
                            }
                    in
                        { response = res, card = card }
                            |> isCorrect
                            |> Expect.equal False
            ]
        ]
