module Card exposing (..)


type Card
    = Card { question : String, answer : String }


new : String -> String -> Card
new question answer =
    Card
        { question = question
        , answer = answer
        }


question : Card -> String
question (Card { question, answer }) =
    question


answer : Card -> String
answer (Card { question, answer }) =
    answer
