module DeckTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Deck exposing (..)
import Card


suite : Test
suite =
    describe "Deck"
        [ describe "cards"
            [ test "it takes a list of cards to act as deck" <|
                \_ ->
                    let
                        givenCards =
                            [ Card.new "How much does a Polar Bear weigh?" "Enough to break the ice!"
                            , Card.new "How goes it?" "Fine, thank you."
                            ]
                    in
                        new givenCards
                            |> cards
                            |> Expect.equal givenCards
            ]
        , describe "count"
            [ test "it returns the number of cards" <|
                \_ ->
                    let
                        givenCards =
                            [ Card.new "How much does a Polar Bear weigh?" "Enough to break the ice!"
                            , Card.new "How goes it?" "Fine, thank you."
                            ]
                    in
                        new givenCards
                            |> count
                            |> Expect.equal 2
            ]
        ]
