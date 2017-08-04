module Styles exposing (..)

import Css exposing (..)
import Html exposing (Attribute)
import Html.Attributes
import Colors


styles cssPairs =
    asPairs cssPairs
        |> Html.Attributes.style


titleStyles : Attribute msg
titleStyles =
    styles
        [ color (hex Colors.titleText) ]


containerStyles : Attribute msg
containerStyles =
    styles
        [ margin auto
        , textAlign center
        ]


flashcardStyles : Attribute msg
flashcardStyles =
    styles
        [ margin auto
        , border (px 5)
        , borderColor (hex Colors.flashcardBorder)
        , borderStyle solid
        , height (px 335)
        , width (px 675)
        , backgroundColor (hex Colors.flashcardBackground)
        , color (hex Colors.flashcardText)
        , property "font-family" "'Acme', sans-serif"
        ]


fillerStyles : Attribute msg
fillerStyles =
    styles
        [ height (px 60) ]


questionContainerStyles : Attribute msg
questionContainerStyles =
    styles
        [ margin auto
        , width (pct 75)
        , fontSize (px 32)
        ]


inputStyles : Attribute msg
inputStyles =
    styles
        [ marginTop (px 50)
        , height (px 50)
        , width (px 150)
        , textAlign center
        , fontSize (px 20)
        , fontWeight bold
        , backgroundColor (hex Colors.background)
        , color (hex Colors.inputText)
        , property "font-family" "'Acme', sans-serif"
        ]


viewAnswerStyles : Attribute msg
viewAnswerStyles =
    styles
        [ backgroundColor (hex Colors.answerButton)
        ]


nextQuestionStyles : Attribute msg
nextQuestionStyles =
    styles
        [ backgroundColor (hex Colors.nextQuestionButton) ]


replayStyles : Attribute msg
replayStyles =
    styles
        [ backgroundColor (hex Colors.nextQuestionButton) ]


buttonStyles : Attribute msg
buttonStyles =
    styles
        [ width (px 150)
        , padding2 (px 14) (px 20)
        , marginTop (px 35)
        , border (px 0)
        , borderRadius (px 4)
        , fontSize (px 16)
        , marginLeft (px 25)
        , marginRight (px 25)
        , color (hex Colors.buttonText)
        ]


messageStyles : Attribute msg
messageStyles =
    styles
        [ marginTop (px 40)
        , fontWeight bold
        , fontSize (px 26)
        ]


messageColorize : String -> Attribute msg
messageColorize text =
    if text == "Incorrect!" then
        styles
            [ color (hex Colors.incorrectText) ]
    else if text == "Correct!" then
        styles
            [ color (hex Colors.correctText) ]
    else
        styles
            [ color (hex Colors.titleText) ]
