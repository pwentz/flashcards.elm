module CardTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Card exposing (..)


suite : Test
suite =
    describe "Card"
        [ describe "question"
            [ test "it takes a Card and returns the question prop" <|
                \_ ->
                    let
                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            "Enough to break the ice!"

                        card =
                            { question = q, answer = a }
                    in
                        Expect.equal q card.question
            ]
        , describe "answer"
            [ test "it takes a Card and returns the answer prop" <|
                \_ ->
                    let
                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            "Enough to break the ice!"

                        card =
                            { question = q, answer = a }
                    in
                        Expect.equal a card.answer
            ]
        ]
