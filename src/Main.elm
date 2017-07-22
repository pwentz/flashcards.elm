module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Round exposing (Round(FinishedRound, RoundInProgress))
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))
import Deck exposing (Deck(Deck, EmptyDeck))
import Card exposing (Card)
import Samples
import Styles


main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { round : Round
    , input : String
    , message : String
    , display : String
    }


initialModel : Model
initialModel =
    { round = Round.new (Deck.new Samples.cards)
    , input = ""
    , message = "Enter your response and press ENTER!"
    , display =
        Samples.cards
            |> List.head
            |> Maybe.map .question
            |> Maybe.withDefault ""
    }



-- VIEW


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        didPressEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not enter"
    in
        on "keydown" (Json.andThen didPressEnter keyCode)


view : Model -> Html Msg
view model =
    div
        [ Styles.containerStyles ]
        [ h1
            [ Styles.titleStyles ]
            [ text "Flashcards" ]
        , div
            [ Styles.fillerStyles ]
            []
        , div
            [ Styles.flashcardStyles ]
            [ div
                [ Styles.fillerStyles ]
                []
            , div
                [ Styles.questionContainerStyles ]
                [ p
                    []
                    [ text (.display model) ]
                ]
            ]
        , button
            [ type_ "submit"
            , onClick SeeAnswer
            , Styles.buttonStyles
            , Styles.viewAnswerStyles
            ]
            [ text "View Answer" ]
        , button
            [ type_ "submit"
            , onClick NextQuestion
            , Styles.buttonStyles
            , Styles.nextQuestionStyles
            ]
            [ text "Next Question" ]
        , br [] []
        , input
            [ type_ "text"
            , onInput UpdateField
            , onEnter RecordGuess
            , value (.input model)
            , autofocus True
            , Styles.inputStyles
            ]
            []
        , p
            [ Styles.messageStyles
            , Styles.messageColorize (.message model)
            ]
            [ text (.message model) ]
        ]



-- UPDATE


type Msg
    = UpdateField String
    | RecordGuess
    | SeeAnswer
    | NextQuestion


updateDisplayWithScore : Model -> Float -> Model
updateDisplayWithScore model percentCorrect =
    let
        congrats =
            "Congratulations! You racked up a score of "
    in
        { model | display = congrats ++ (toString percentCorrect) ++ "%" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField userInput ->
            { model | input = userInput }

        RecordGuess ->
            case (Round.recordGuess (.input model) (.round model)) of
                (FinishedRound { percentCorrect }) as newRound ->
                    (updateDisplayWithScore
                        { model
                            | input = ""
                            , round = newRound
                            , message = "Game Over!"
                        }
                        percentCorrect
                    )

                (RoundInProgress { deck, guesses, currentCard }) as newRound ->
                    let
                        lastGuess =
                            Guess.new (.input model) currentCard
                    in
                        case lastGuess of
                            IncorrectGuess ->
                                { model
                                    | input = ""
                                    , round = newRound
                                    , message = "Incorrect!"
                                }

                            CorrectGuess ->
                                { input = ""
                                , round = newRound
                                , message = "Correct!"
                                , display = .question currentCard
                                }

        SeeAnswer ->
            case (Round.getCurrentCard (.round model)) of
                Nothing ->
                    model

                Just { question, answer } ->
                    { model | display = answer }

        NextQuestion ->
            case (Round.getDeck (.round model)) of
                Nothing ->
                    model

                Just EmptyDeck ->
                    model

                Just (Deck []) ->
                    model

                Just (Deck (x :: xs)) ->
                    let
                        current =
                            xs
                                |> List.head
                                |> Maybe.withDefault x
                    in
                        { input = ""
                        , display = .question current
                        , message = ""
                        , round =
                            RoundInProgress
                                { deck = Deck.new (List.append xs [ x ])
                                , currentCard = current
                                , guesses =
                                    (.round model)
                                        |> Round.getGuesses
                                        |> Maybe.withDefault []
                                }
                        }



-- update : Msg -> Model -> Model
-- update msg model =
--     case msg of
--         UpdateField userInput ->
--             { model | input = userInput }
--         RecordGuess ->
--             case ((.currentCard << .round) model) of
--                 Nothing ->
--                     { model
--                         | input = ""
--                         , message = "Game over!"
--                     }
--                 Just card ->
--                     case (Round.recordGuess (.input model) (.round model)) of
--                         Nothing ->
--                             model
--                         Just updatedRound ->
--                             case (Guess.new (.input model) card) of
--                                 IncorrectGuess ->
--                                     let
--                                         newModel =
--                                             { model
--                                                 | input = ""
--                                                 , round = updatedRound
--                                                 , message = "Incorrect!"
--                                             }
--                                     in
--                                         case (.currentCard updatedRound) of
--                                             Nothing ->
--                                                 newModel
--                                             Just currentCard ->
--                                                 { newModel | display = .question currentCard }
--                                 CorrectGuess ->
--                                     let
--                                         newModel =
--                                             { model
--                                                 | input = ""
--                                                 , round = updatedRound
--                                                 , message = "Correct!"
--                                             }
--                                     in
--                                         case (.currentCard updatedRound) of
--                                             Nothing ->
--                                                 updateDisplayWithScore newModel
--                                             Just currentCard ->
--                                                 { newModel | display = .question currentCard }
--         SeeAnswer ->
--             case ((.currentCard << .round) model) of
--                 Nothing ->
--                     model
--                 Just card ->
--                     { model | display = (.answer card) }
--         NextQuestion ->
--             case ((.currentCard << .round) model) of
--                 Nothing ->
--                     model
--                 Just card ->
--                     let
--                         round =
--                             (.round model)
--                         newDeck =
--                             Deck (List.append (Deck.deckTail (.deck round)) [ card ])
--                     in
--                         { round =
--                             { round
--                                 | deck = newDeck
--                                 , currentCard = Deck.topCard newDeck
--                             }
--                         , display =
--                             newDeck
--                                 |> Deck.topCard
--                                 |> Maybe.map .question
--                                 |> Maybe.withDefault ""
--                         , input = ""
--                         , message = ""
--                         }
