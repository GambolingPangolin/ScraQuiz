module View exposing (view)

import CustomTypes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import List as L
import List exposing (take, drop)

import Dict as D

-- VIEW
view : Model -> Html Msg
view model =
    let
        (s,t) = model.score
        curScore = 100 * toFloat s / toFloat t |> floor 
        bgcolor = case (curScore < 33, curScore < 66) of
            (True,_)     -> class "bad"
            (False,True) -> class "okay"
            _            -> class "good"
        scoreMessage = curScore |> toString |> (++) "%"
        scoreboard =
            if t > 0
               then div [class "score", bgcolor] [text scoreMessage]
               else div [class "score"] []
           
    in
    div [class "main"]
      [
      div [ class "banner" ] [ text "Scrabble quizzes" ]
      , div [ class "left" ]
          [
          scoreboard
          , div [ class "menu" ] (renderMenu model.quiz)
          ]
      , div [ class "right" ]
        [
        renderBoard model.showScore model.board
        , div [ class "controls"] [
            if not model.showScore
               then span [onClick CheckScore] [text "Check Score"] 
               else text ""
            , span [onClick MakeNewBoard] [text "New Quiz"]
            ]
        ]
      ]
    
renderMenu : QuizList -> List (Html Msg)
renderMenu ql =
    let
        makeClass x =
            if x == ql 
               then class "active"
               else class "inactive"
        makeElement x label =
            span [ makeClass x, onClick (ChangeList x) ] [ text label ]
    in
    [ makeElement Twos "Twos"
    , makeElement Threes "Threes"
    , makeElement Qnou "Q (no U)"
    , makeElement Q "Q"
    , makeElement Jqxz "2 of JXQZ"
    , makeElement Cons "Consonants"
    , makeElement ConsY "Consonants (and Y)"
    ]
    

renderBoard : Bool -> Board -> Html Msg
renderBoard ss b = 
    let
        nRows = 6
        addClasses x atts = 
            case (x.isWord, x.isPicked) of
                (True, True) -> atts ++ [class "isPicked", class "isWord"]
                (True, False) -> (class "isWord") :: atts
                (False, True) -> (class "isPicked") :: atts
                (False, False) -> atts
        addAtts x atts =
            if ss 
               then addClasses x atts
               else addClasses x (onClick (ToggleTile x.word) :: atts)

        viewTile x = span (addAtts x [class "panel"]) [text x.word]
        chop : Int -> List a -> List (List a)
        chop n xs = 
            if L.length xs >= n 
               then (take n xs) :: chop n (drop n xs)
               else [xs]
        viewRow cs = div [class "row"] <| L.map viewTile cs
        result = if ss then class "result" else class "playing"
        theBoard = case b of
            Blank -> []
            Board ts ->
                L.map viewRow (chop nRows ts) 

    in
    div [class "board", result] theBoard 
