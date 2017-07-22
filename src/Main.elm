module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Round exposing (Round(FinishedRound, RoundInProgress))
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))
import Deck exposing (Deck)
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
    { round =
        Round.new
            { deck = Samples.cards
            , guesses = []
            }
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


congratsMsg : Float -> String
congratsMsg pct =
    "Congratulations! You racked up a score of "
        ++ (toString pct)
        ++ "%"


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField userInput ->
            { model | input = userInput }

        RecordGuess ->
            case (Round.deck model.round) of
                Nothing ->
                    model

                Just [] ->
                    model

                Just (top :: moreCards) ->
                    let
                        guess =
                            Guess.new model.input top

                        updatedModel =
                            { model
                                | input = ""
                                , round = Round.recordGuess guess model.round
                            }

                        nextQuestion =
                            moreCards
                                |> List.head
                                |> Maybe.withDefault top
                                |> .question
                    in
                        case ( guess, updatedModel.round ) of
                            ( _, FinishedRound { percentCorrect } ) ->
                                { updatedModel
                                    | message = "Game Over!"
                                    , display =
                                        congratsMsg percentCorrect
                                }

                            ( IncorrectGuess, RoundInProgress { deck, guesses } ) ->
                                { updatedModel
                                    | message = "Incorrect!"
                                    , display =
                                        nextQuestion
                                }

                            ( CorrectGuess, RoundInProgress { deck, guesses } ) ->
                                { updatedModel
                                    | message = "Correct!"
                                    , display =
                                        nextQuestion
                                }

        SeeAnswer ->
            case (Round.currentCard (.round model)) of
                Nothing ->
                    model

                Just { question, answer } ->
                    { model | display = answer }

        NextQuestion ->
            case (Round.deck (.round model)) of
                Nothing ->
                    model

                Just [] ->
                    model

                Just (top :: moreCards) ->
                    let
                        newDisplay =
                            moreCards
                                |> List.head
                                |> Maybe.withDefault top
                                |> .question
                    in
                        { input = ""
                        , display = newDisplay
                        , message = ""
                        , round =
                            RoundInProgress
                                { deck = List.append moreCards [ top ]
                                , guesses =
                                    .round model
                                        |> Round.getGuesses
                                        |> Maybe.withDefault []
                                }
                        }
