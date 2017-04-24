module View exposing (view)

import CustomTypes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import List as L
import List exposing (take, drop)

-- VIEW
view : Model -> Html Msg
view model =
    let
        result = 
            if model.showScore
               then [class "result"]
               else []
        (s,t) = model.score
        curScore = 100 * toFloat s / toFloat t |> floor |> toString 
        scoreAtts = 
            if t > 0 
               then [class "score"]
               else [class "score", class "off"]
           
    in
    div [class "main"]
      [ node "link" [ rel "stylesheet", href "all.css" ] [] 
      , h1 [] [ text "Scrabble Quiz" ]
      , div scoreAtts [ text ("Current score: " ++ curScore ++ "%") ]
      , div [ class "menu" ] (renderMenu model.quiz)
      , div ([ class "board" ] ++ result) (renderBoard model.showScore model.board)
      , div [ class "footer"] [
          span [onClick MakeNewBoard] [text "New Quiz"]
          , text "|"
          , span [onClick CheckScore] [text "Check"] ]
      ]
    
renderMenu : QuizList -> List (Html Msg)
renderMenu ql =
    let
        makeClass x =
            if x == ql 
               then class "active"
               else class "inactive"
        makeElement x label =
            span [ makeClass x, onClick (ChangeQuiz x) ] [ text label ]
    in
    [ makeElement Twos "Twos"
    --, makeElement Threes "Threes"
    , makeElement Qnou "Q (no U)"
    , makeElement Q "Q"
    , makeElement Jqxz "2 of JXQZ"
    , makeElement Cons "Consonants"
    , makeElement ConsY "Consonants (and Y)"
    , makeElement JustVowels "Just vowels"
    ]
    

renderBoard : Bool -> Board -> List (Html Msg)
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
    in
    case b of
        Blank -> []
        Board ts ->
            L.map viewRow (chop nRows ts) 

