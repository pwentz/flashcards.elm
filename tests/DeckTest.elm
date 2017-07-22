module DeckTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Deck exposing (..)
import Card


suite : Test
suite =
    describe "Deck"
        [ describe "new"
            [ test "it returns an EmptyDeck if passed an empty array" <|
                \_ ->
                    new []
                        |> topCard
                        |> Expect.equal Nothing
            , test "it takes an array and returns a Deck" <|
                \_ ->
                    let
                        top =
                            { question = "When is the last time the Cubs won the World Series"
                            , answer = "2016"
                            }

                        cards =
                            [ top
                            , { question = "Do you know the muffin man?", answer = "No." }
                            ]
                    in
                        new cards
                            |> topCard
                            |> Expect.equal (Just top)
            ]
        , describe "map"
            [ test "it takes a (List Cards -> List Card) fn and a Deck and returns an new deck with the function applied" <|
                \_ ->
                    let
                        bottom =
                            { question = "Do you know the muffin man?", answer = "No." }

                        cards =
                            [ { question = "When is the last time the Cubs won the World Series", answer = "2016" }
                            , bottom
                            ]
                    in
                        new cards
                            |> map (List.drop 1)
                            |> Deck.topCard
                            |> Expect.equal (Just bottom)
            ]
        , describe "rotate"
            [ test "it returns a list of cards with the top on the bottom and the next card as head" <|
                \_ ->
                    let
                        top =
                            { question = "When is the last time the Cubs won the World Series", answer = "2016" }

                        bottom =
                            { question = "Do you know the muffin man?", answer = "No." }

                        cards =
                            [ top
                            , bottom
                            ]
                    in
                        new cards
                            |> rotate
                            |> Expect.equal (new [ bottom, top ])
            , test "it returns the same array when passed a deck w/ one element in it" <|
                \_ ->
                    let
                        card =
                            { question = "Do you know the muffin man?", answer = "No." }
                    in
                        new [ card ]
                            |> rotate
                            |> Expect.equal (new [ card ])
            ]

        -- , describe "append"
        --     [ test "it combines two decks into one" <|
        --         \_ ->
        --             let
        --                 top =
        --                     { question = "When is the last time the Cubs won the World Series", answer = "2016" }
        --                 bottom =
        --                     { question = "Do you know the muffin man?", answer = "No." }
        --             in
        --                 new [ top ]
        --                     |> append (new [ bottom ])
        --                     |> Expect.equal (new [ bottom, top ])
        --     ]
        , describe "topCard"
            [ test "it gets the top card of the deck" <|
                \_ ->
                    let
                        top =
                            { question = "When is the last time the Cubs won the World Series", answer = "2016" }

                        bottom =
                            { question = "Do you know the muffin man?", answer = "No." }
                    in
                        new [ top, bottom ]
                            |> topCard
                            |> Expect.equal (Just top)
            ]
        ]
