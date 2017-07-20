module CardTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Card exposing (..)


suite : Test
suite =
    describe "Card"
        [ describe "new"
            [ test "it takes a question and an answer and returns a Card" <|
                \_ ->
                    let
                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            "Enough to break the ice!"
                    in
                        new q a
                            |> Expect.equal
                                (Card
                                    { question = q
                                    , answer = a
                                    }
                                )
            ]
        , describe "question"
            [ test "it takes a Card and returns the question prop" <|
                \_ ->
                    let
                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            "Enough to break the ice!"
                    in
                        new q a
                            |> question
                            |> Expect.equal q
            ]
        , describe "answer"
            [ test "it takes a Card and returns the answer prop" <|
                \_ ->
                    let
                        q =
                            "How much does a Polar Bear weigh?"

                        a =
                            "Enough to break the ice!"
                    in
                        new q a
                            |> answer
                            |> Expect.equal a
            ]
        ]
