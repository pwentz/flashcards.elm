module DeckTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Deck exposing (..)
import Card


suite : Test
suite =
    describe "Deck"
        [ describe "count"
            [ test "it returns the number of cards" <|
                \_ ->
                    let
                        deck =
                            [ { question = "How much does a Polar Bear weigh?", answer = "Enough to break the ice!" }
                            , { question = "How goes it?", answer = "Fine, thank you." }
                            ]
                    in
                        deck
                            |> count
                            |> Expect.equal 2
            ]
        ]
