module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Round exposing (Round)
import Guess exposing (Guess(IncorrectGuess, CorrectGuess))
import Deck exposing (Deck(Deck))
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


defaultRound : Round
defaultRound =
    { deck = Deck ([])
    , guesses = []
    , currentCard = Nothing
    }


initialModel : Model
initialModel =
    { round =
        Round.new (Deck.new Samples.cards)
            |> Maybe.withDefault defaultRound
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


updateDisplayWithScore : Model -> Model
updateDisplayWithScore model =
    let
        congrats =
            "Congratulations! You racked up a score of "
    in
        { model | display = congrats ++ (toString (Round.percentCorrect (.round model))) ++ "%" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField userInput ->
            { model | input = userInput }

        RecordGuess ->
            case ((.currentCard << .round) model) of
                Nothing ->
                    { model
                        | input = ""
                        , message = "Game over!"
                    }

                Just card ->
                    case (Round.recordGuess (.input model) (.round model)) of
                        Nothing ->
                            model

                        Just updatedRound ->
                            case (Guess.new (.input model) card) of
                                IncorrectGuess ->
                                    let
                                        newModel =
                                            { model
                                                | input = ""
                                                , round = updatedRound
                                                , message = "Incorrect!"
                                            }
                                    in
                                        case (.currentCard updatedRound) of
                                            Nothing ->
                                                newModel

                                            Just currentCard ->
                                                { newModel | display = .question currentCard }

                                CorrectGuess ->
                                    let
                                        newModel =
                                            { model
                                                | input = ""
                                                , round = updatedRound
                                                , message = "Correct!"
                                            }
                                    in
                                        case (.currentCard updatedRound) of
                                            Nothing ->
                                                updateDisplayWithScore newModel

                                            Just currentCard ->
                                                { newModel | display = .question currentCard }

        SeeAnswer ->
            case ((.currentCard << .round) model) of
                Nothing ->
                    model

                Just card ->
                    { model | display = (.answer card) }

        NextQuestion ->
            case ((.currentCard << .round) model) of
                Nothing ->
                    model

                Just card ->
                    let
                        round =
                            (.round model)

                        newDeck =
                            Deck (List.append (Deck.deckTail (.deck round)) [ card ])
                    in
                        { round =
                            { round
                                | deck = newDeck
                                , currentCard = Deck.topCard newDeck
                            }
                        , display =
                            newDeck
                                |> Deck.topCard
                                |> Maybe.map .question
                                |> Maybe.withDefault ""
                        , input = ""
                        , message = ""
                        }
