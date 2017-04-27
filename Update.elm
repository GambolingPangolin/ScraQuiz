module Update exposing (update)

import Wordlists exposing (getWordlist)
import Types exposing (..)
import RandUtils exposing (..)

import Random as R
import Random exposing (Generator, float, generate, andThen)

import List as L
import List exposing (head, drop)

import Array as A
import Array exposing (Array, fromList, get)

import Maybe as M

import Dict exposing (Dict)
import Dict as D

import Tuple exposing (first, second, mapFirst)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Intro ->
            case msg of
                LeaveIntro ->
                    (Model {
                        quiz = Twos
                        , wordlist = A.empty
                        , board = Blank
                        , log = D.empty
                        , score = (0,0)
                        , showScore = False
                        }, getWordlist "twos")
                _ -> (model, Cmd.none)
        Model x -> updateModel msg x |> mapFirst Model

updateModel : Msg -> ModelData -> (ModelData, Cmd Msg)
updateModel msg model =
    case msg of
        ChangeList x ->
            ({ model | quiz = x, board = Blank, showScore = False}, getWordlist (quizName x)) 
        Wordlist wl ->
            let
                newModel = {model| wordlist = wl}
            in
            ( newModel, makeBoard newModel)
        MakeNewBoard ->
            ( { model | board = Blank, showScore = False }, makeBoard model )
        NewBoard x ->
            ({model | board = x, showScore = False }, Cmd.none)
        CheckScore ->
            let
                (oldScore, oldTotal) = model.score
                (thisScore, thisTotal) = 
                    case model.board of
                        Blank -> (0,0)
                        Board ts -> 
                          (ts |> L.filter (\ t -> t.isWord == t.isPicked) |> L.length, L.length ts)
                newLog = 
                  case model.board of
                    Blank -> model.log
                    Board ts ->
                      let 
                          unseen = L.filter (\ t -> D.member t.word model.log |> not) ts
                          inserted = L.foldl (\ t -> D.insert t.word 0) model.log unseen
                      in 
                      L.foldl (
                        \ t -> 
                          D.update t.word (M.map (\ s -> if t.isWord == t.isPicked then s+1 else s-1))
                        ) inserted ts
            in  
            ({model | showScore = True
                      , score = (thisScore+oldScore, thisTotal+oldTotal) 
                      , log = newLog
            }, Cmd.none)
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
        _ -> (model, Cmd.none)

-- HELPER FUNCTIONS

sample : Array String -> Generator String 
sample xs =
    let
        l = A.length xs
        g = R.int 0 (l-1) 
    in
       R.map (
           flip get xs >>
           M.withDefault "*" 
           ) g
           
adjust : Dict String Int -> Array String -> Array String
adjust d ws = 
  D.toList d
  |> L.filter (first >> flip L.member (A.toList ws))
  |> L.concatMap (\ (w,s) -> L.repeat (max 0 (negate s)) w)
  |> A.fromList
  |> A.append ws

checkWord : Array String -> String -> Bool
checkWord wl w = A.toList wl |> L.member w 

makeBoard : ModelData -> Cmd Msg
makeBoard model = [] |> extendBoard model >> R.map Board >> generate NewBoard

extendBoard : ModelData -> List String -> Generator (List Tile)
extendBoard model ws =
    let
        makeTile : String -> Tile
        makeTile w = {word = w, isWord = checkWord model.wordlist w, isPicked = False}
        n = min 18 (A.length model.wordlist)
    in
    if L.length ws >= n
       then L.map makeTile ws |> generatorUnit
       else 
           sample (adjust model.log model.wordlist) 
           |> andThen (fuzz model.quiz model.wordlist)
           |> R.map (
               L.singleton >> 
               L.filter (flip L.member ws >> not) 
               >> (++) ws
               )
           |> andThen (extendBoard model)

fuzz : QuizList -> Array String -> String -> Generator String
fuzz q wl w =
    case q of
        Twos       -> fuzz1 [] [] w
        Threes     -> fuzz1 [] [] w
        Q          -> fuzz1 ['Q'] [] w
        Qnou       -> fuzz1 ['Q'] ['U'] w
        Cons       -> fuzz1 [] ['Y'] w
        ConsY      -> fuzz1 [] [] w
        Jqxz       -> fuzz1 ['J','X','Q','Z'] [] w
        JustVowels -> fuzz1 [] [] w

fuzz1 : List Char -> List Char -> String -> Generator String
fuzz1 noReplace noAdd w =
    let
        l = String.length w
        g = R.list l (R.float 0 1)
        p = 0.65 
        q = 1 - e ^ ( (1 / toFloat l) * logBase e (1 - p) )
        replacer : Char -> Float -> Char
        replacer c x =
            let
                vows2 = L.filter (flip L.member noAdd >> not) vowels
                cons2 = L.filter (flip L.member noAdd >> not) consonants
                newVow = 
                    vows2
                    |> drop (floor (x * 511) % (L.length vows2)) 
                    |> head 
                    |> M.withDefault '*'  
                newCons =
                    cons2
                    |> drop (floor (x * 511) % (L.length cons2)) 
                    |> head
                    |> M.withDefault '*'
                newChar = if L.member c vowels then newVow else newCons
            in
            if x >= q || L.member c noReplace
               then c
               else newChar
    in
       R.map (
           (w |> String.toList |> L.map replacer |> L.map2 (<|)) >> String.fromList
           ) g

vowels = ['A','E','I','O','U']
consonants = ['B','C','D','F','G','H','J','K','L','M','N','P','Q','R','S','T','V','W','X','Y','Z']
