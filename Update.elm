module Update exposing (update, makeBoard)

--import QuizData exposing (..)
import Wordlists exposing (getWordlist)
import CustomTypes exposing (..)
import RandUtils exposing (..)

import Random as R
import Random exposing (Generator, float, generate, andThen)

import List as L
import List exposing (head, drop)

import Array as A
import Array exposing (Array, fromList, get)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeQuiz x ->
            ({ model | quiz = x, board = Blank, showScore = False}, getWordlist (quizName x)) 
        Wordlist wl ->
          (model, makeBoard model.quiz wl)
        MakeNewBoard ->
            ( { model | board = Blank, showScore = False }, getWordlist (quizName model.quiz) )
        NewBoard x ->
            -- We might have generated a board with duplicates
            -- Check and rebuild if needed
            let
                noDup ws =
                    case ws of
                        [] -> True
                        w :: rest ->
                            not (L.member w rest) && noDup rest
                isOkay = 
                    case x of
                        Blank -> True
                        Board ts -> noDup (L.map (\t -> t.word) ts)
            in
               if isOkay 
                  then ({model | board = x, showScore = False }, Cmd.none )
                  else (model, getWordlist (quizName model.quiz))
        CheckScore ->
            let
                agger t sc = 
                    let
                        (u,v) = sc
                    in
                       if t.isWord == t.isPicked then (u+1,v+1) else (u,v+1)
                (oldScore, oldTotal) = model.score
                (thisScore, thisTotal) = 
                    case model.board of
                        Blank -> (0,0)
                        Board ts -> L.foldl agger (0,0) ts

            in  
            ({ model | showScore = True, score = (thisScore+oldScore, thisTotal+oldTotal) }, Cmd.none)
        ToggleTile x ->
            let
                subst : Tile -> Tile
                subst t =
                    if t.word == x 
                       then { t | isPicked = not t.isPicked }
                       else t
                toggledBoard = case model.board of
                    Board b -> Board (L.map subst b)
                    Blank -> Blank
            in
            ( { model | board = toggledBoard }, Cmd.none )

-- HELPER FUNCTIONS

-- Fuzz words
-- Randomly modify letters in a word, taking care to produce a word of the same type

fuzz : QuizList -> String -> Generator String
fuzz q w =
  let
      l = String.length w
      -- Probability of replacing a letter should be about q 
      -- ( 1 - (1-p)^l) = p2 
      p2 = 0.5
      p = 1 - e ^ ( (1 / toFloat l) * logBase e ( 1 - p2 ))
      g = R.list l (float 0 1)

      fuzzChar exs f c = 
          let
              f2 = floor (f * toFloat 51)
              newCons = choose <| head (drop (f2 % 21) consonants)
              newVow  = choose <| head (drop (f2 % 5) vowels)
              choose x = case x of
                  Just t -> t
                  Nothing -> '*'
              newChar = if L.member c consonants then newCons else newVow
          in
          if f < p && not (L.member c exs) then newChar else c

      simpleFuzz excs w = 
          let
              fuzzer fs = 
                  L.map2 (fuzzChar excs) fs (String.toList w) |> String.fromList

          in
          R.map fuzzer g 
  in
  case q of
    -- It is possible for a non-Y to be replace by a Y
    Cons       -> simpleFuzz [] w
    ConsY      -> simpleFuzz [] w
    JustVowels -> simpleFuzz [] w
    Twos       -> simpleFuzz [] w
    Threes     -> simpleFuzz [] w
    Q          -> simpleFuzz ['Q'] w
    -- It is possible for a different vowel to be replaced by a U
    Qnou       -> simpleFuzz [] w 
    Jqxz       -> simpleFuzz ['J','Q','X','Z'] w

-- Check words

checkWord : String -> Array String -> Bool
checkWord w wl = A.toList wl |> L.member w 

-- Create a new board 

makeBoard : QuizList -> Array String -> Cmd Msg
makeBoard q wl = 
    let
        n = A.length wl
        k = 18
        g = R.list k (float 0 1)
        makeTile : String -> Tile
        makeTile w = { word = w, isWord = checkWord w wl, isPicked = False }
        getter : Int -> List String
        getter i =
          case get i wl of
            Just w  -> [w]
            Nothing -> []
        drawWords : List Float -> List String
        drawWords = 
          randSubset n >> 
          L.concatMap getter 
        boardGen = 
            g |> 
            andThen (drawWords >> L.map (fuzz q >> R.map makeTile) >> combine) 

    in
    generate (Board >> NewBoard) boardGen 


vowels = ['A','E','I','O','U']
consonants = ['B','C','D','F','G','H','J','K','L','M','N','P','Q','R','S','T','V','W','X','Y','Z']
