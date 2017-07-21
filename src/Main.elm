module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Round exposing (Round)
import Guess
import Deck
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


initialRound =
    { deck = Samples.cards
    , guesses = []
    }


initialModel : Model
initialModel =
    { round = initialRound
    , input = ""
    , message = "Enter your response and press ENTER!"
    , display =
        Round.currentCard initialRound
            |> .question
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField userInput ->
            { model | input = userInput }

        RecordGuess ->
            case model.round.deck of
                [] ->
                    { model
                        | input = ""
                    }

                top :: moreCards ->
                    let
                        guess =
                            { response = model.input
                            , card = top
                            }

                        updatedModel =
                            { model
                                | round = Round.recordGuess model.round guess
                                , input = ""
                            }

                        congratulatoryMsg =
                            "Congratulations, you posted a percentage of "
                                ++ (toString (Round.percentCorrect updatedModel.round))
                                ++ "%"

                        nextDisplay =
                            moreCards
                                |> List.head
                                |> Maybe.withDefault top
                                |> .question
                    in
                        if (Guess.isCorrect guess) then
                            let
                                isRoundOver =
                                    updatedModel.round.deck
                                        |> List.length
                                        |> ((==) 0)
                            in
                                if isRoundOver then
                                    { updatedModel
                                        | message = "Game Over!"
                                        , display =
                                            congratulatoryMsg
                                    }
                                else
                                    { updatedModel
                                        | message = "Correct!"
                                        , display = nextDisplay
                                    }
                        else
                            { updatedModel
                                | message = "Incorrect!"
                                , display = nextDisplay
                            }

        SeeAnswer ->
            if Round.isOver model.round then
                model
            else
                { model | display = (.answer << Round.currentCard << .round) model }

        NextQuestion ->
            case model.round.deck of
                [] ->
                    model

                top :: moreCards ->
                    let
                        round =
                            model.round
                    in
                        { round =
                            { round
                                | deck = List.append moreCards [ top ]
                            }
                        , input = ""
                        , message = ""
                        , display =
                            moreCards
                                |> List.head
                                |> Maybe.withDefault top
                                |> .question
                        }
