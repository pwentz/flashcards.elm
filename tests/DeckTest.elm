module DeckTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Deck exposing (..)
import Card


suite : Test
suite =
    describe "Deck"
        [ describe "EmptyDeck"
            [ test "it has no attributes" <|
                \_ ->
                    Expect.equal (EmptyDeck) (EmptyDeck)
            ]
        , describe "(not an empty) Deck"
            [ test "it gets constructed with a list of cards" <|
                \_ ->
                    let
                        cards =
                            [ { question = "When is the last time the Cubs won the World Series", answer = "2016" }
                            , { question = "Do you know the muffin man?", answer = "No." }
                            ]

                        deck =
                            Deck cards

                        cardsFromDeck =
                            case deck of
                                EmptyDeck ->
                                    Nothing

                                Deck cs ->
                                    Just cs
                    in
                        Expect.equal (Just cards) cardsFromDeck
            ]
        ]
