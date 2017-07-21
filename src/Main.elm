module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Round exposing (Round)
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


initialModel : Model
initialModel =
    { round =
        { deck = Samples.cards
        , guesses = []
        , currentCard = Deck.topCard Samples.cards
        , numberCorrect = 0
        }
    , input = ""
    , message = "Enter your response and press ENTER!"
    , display =
        Deck.topCard Samples.cards
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


checkIfCardsLeft : Model -> Model
checkIfCardsLeft model =
    if Deck.count ((.deck << .round) model) == 0 then
        let
            congrats =
                "Congratulations, you posted a percentage of "
        in
            { model | display = congrats ++ (toString (Round.percentCorrect (.round model))) ++ "%" }
    else
        { model | display = (.question << .currentCard << .round) model }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField userInput ->
            { model | input = userInput }

        RecordGuess ->
            if Deck.count ((.deck << .round) model) == 0 then
                { model
                    | input = ""
                    , message = "Game over!"
                }
            else
                let
                    updatedRound =
                        Round.recordGuess (.round model) (.input model)
                in
                    if (((.numberCorrect << .round) model) == (.numberCorrect updatedRound)) then
                        checkIfCardsLeft
                            { model
                                | input = ""
                                , round = updatedRound
                                , message = "Incorrect!"
                            }
                    else
                        checkIfCardsLeft
                            { model
                                | input = ""
                                , round = updatedRound
                                , message = "Correct!"
                            }

        SeeAnswer ->
            if Deck.count ((.deck << .round) model) == 0 then
                model
            else
                { model | display = (.answer << .currentCard << .round) model }

        NextQuestion ->
            if Deck.count ((.deck << .round) model) == 0 then
                model
            else
                let
                    round =
                        (.round model)

                    newDeck =
                        Deck.topCardToBottom (.deck round)
                in
                    { round =
                        { round
                            | deck = newDeck
                            , currentCard = Deck.topCard newDeck
                        }
                    , display =
                        Deck.topCard newDeck
                            |> .question
                    , input = ""
                    , message = ""
                    }
