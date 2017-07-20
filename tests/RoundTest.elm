module RoundTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Round exposing (..)
import Card
import Deck


suite : Test
suite =
    describe "Round"
        [ describe "new"
            [ test "it takes a deck and returns a Round" <|
                \_ ->
                    let
                        givenCards =
                            [ Card.new "How much does a Polar Bear weigh?" "Enough to break the ice!"
                            , Card.new "How goes it?" "Fine, thank you."
                            ]

                        givenDeck =
                            Deck.new givenCards

                        (Round { deck, guesses, currentCard, numberCorrect }) =
                            new givenDeck
                    in
                        Expect.equal deck givenDeck
            ]
        ]
