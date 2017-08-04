module Main exposing (..)

import Html exposing (..)
import Http as Http
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Round exposing (Round)
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))
import Deck exposing (Deck)
import Card exposing (Card)
import Styles


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { round : Round
    , input : String
    , message : String
    , display : String
    , inProgress : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { round =
            Round.new
                { deck = Deck.new []
                , guesses = []
                }
      , input = ""
      , message = "Enter your response and press ENTER!"
      , display = "Fetching Questions..."
      , inProgress = True
      }
    , fetchQuestions
    )



-- VIEW


actionButtons : Model -> Html Msg
actionButtons model =
    if model.inProgress then
        div
            []
            [ button
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
            ]
    else
        div
            []
            [ button
                [ type_ "submit"
                , onClick Replay
                , Styles.buttonStyles
                , Styles.replayStyles
                ]
                [ text "Replay" ]
            ]


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
        , actionButtons model
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


type alias TriviaQuestion =
    { question : String
    , correct_answer : String
    }


type Msg
    = UpdateField String
    | Replay
    | RecordGuess
    | SeeAnswer
    | NextQuestion
    | NewQuestions (Result Http.Error (List TriviaQuestion))


questionDecoder =
    Json.map2 TriviaQuestion (Json.field "question" Json.string) (Json.field "correct_answer" Json.string)


fetchQuestions : Cmd Msg
fetchQuestions =
    let
        url =
            "https://opentdb.com/api.php?amount=10"

        request =
            Http.get url decodeQuestions
    in
        Http.send NewQuestions request


decodeQuestions : Json.Decoder (List TriviaQuestion)
decodeQuestions =
    Json.at [ "results" ] (Json.list questionDecoder)


congratsMsg : Float -> String
congratsMsg pct =
    "Congratulations! You were correct on "
        ++ (toString pct)
        ++ "% of your guesses!"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField userInput ->
            ( { model | input = userInput }, Cmd.none )

        RecordGuess ->
            case (Round.currentCard model.round) of
                Nothing ->
                    ( { model | input = "" }, Cmd.none )

                Just card ->
                    let
                        guess =
                            Guess.new model.input card

                        updatedModel =
                            { model
                                | input = ""
                                , round = Round.recordGuess guess model.round
                            }

                        nextQuestion =
                            updatedModel.round
                                |> Round.currentCard
                                |> Maybe.map .question

                        onRoundOver { percentCorrect } =
                            { updatedModel
                                | message = "Game Over!"
                                , display = congratsMsg percentCorrect
                                , inProgress = False
                            }

                        onInProgress { deck, guesses } =
                            case guess of
                                IncorrectGuess ->
                                    { updatedModel
                                        | message = "Incorrect!"
                                        , display =
                                            nextQuestion
                                                |> Maybe.withDefault card.question
                                    }

                                CorrectGuess ->
                                    { updatedModel
                                        | message = "Correct!"
                                        , display =
                                            nextQuestion
                                                |> Maybe.withDefault (congratsMsg (Round.getPercentage guesses))
                                    }
                    in
                        ( (Round.either onInProgress onRoundOver updatedModel.round), Cmd.none )

        SeeAnswer ->
            case (Round.currentCard model.round) of
                Nothing ->
                    ( model, Cmd.none )

                Just { question, answer } ->
                    ( { model | display = answer }, Cmd.none )

        NextQuestion ->
            let
                onRoundOver _ =
                    model

                onInProgress { deck, guesses } =
                    { model
                        | input = ""
                        , message = ""
                        , round =
                            Round.new
                                { deck = Deck.rotate deck
                                , guesses = guesses
                                }
                        , display =
                            deck
                                |> Deck.rotate
                                |> Deck.topCard
                                |> Maybe.map .question
                                |> Maybe.withDefault (congratsMsg (Round.getPercentage guesses))
                    }
            in
                ( (Round.either onInProgress onRoundOver model.round), Cmd.none )

        NewQuestions (Ok questions) ->
            let
                cards =
                    questions
                        |> List.map
                            (\x ->
                                { question = x.question, answer = x.correct_answer }
                            )
            in
                ( { model
                    | round =
                        Round.new
                            { deck = (Deck.new cards)
                            , guesses = []
                            }
                    , display =
                        cards
                            |> List.head
                            |> Maybe.map .question
                            |> Maybe.withDefault "Something went wrong while we were fetching your questions!"
                  }
                , Cmd.none
                )

        NewQuestions (Err _) ->
            ( model, Cmd.none )

        Replay ->
            init
