module Update exposing (update, makeBoard)

import Wordlists exposing (getWordlist)
import CustomTypes exposing (..)
import RandUtils exposing (..)

import Random as R
import Random exposing (Generator, float, generate, andThen)

import List as L
import List exposing (head, drop)

import Array as A
import Array exposing (Array, fromList, get)

import Maybe as M

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeList x ->
            ({ model | quiz = x, board = Blank, showScore = False}, getWordlist (quizName x)) 
        Wordlist wl ->
          ({model|wordlist = wl}, makeBoard model.quiz wl)
        MakeNewBoard ->
            ( { model | board = Blank, showScore = False }, makeBoard model.quiz model.wordlist )
        NewBoard x ->
            ({model | board = x, showScore = False }, Cmd.none)
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
           

checkWord : Array String -> String -> Bool
checkWord wl w = A.toList wl |> L.member w 

makeBoard : QuizList -> Array String -> Cmd Msg
makeBoard q wl = [] |> extendBoard q wl >> R.map Board >> generate NewBoard

extendBoard : QuizList -> Array String -> List String -> Generator (List Tile)
extendBoard q wl ws =
    let
        makeTile : String -> Tile
        makeTile w = {word = w, isWord = checkWord wl w, isPicked = False}
        n = min 18 (A.length wl)
    in
    if L.length ws >= n
       then L.map makeTile ws |> generatorUnit
       else 
           sample wl 
           |> andThen (fuzz q wl)
           |> R.map (
               L.singleton >> 
               L.filter (flip L.member ws >> not) 
               >> (++) ws
               )
           |> andThen (extendBoard q wl)

fuzz : QuizList -> Array String -> String -> Generator String
fuzz q wl w =
    case q of
        Twos       -> fuzz1 [] [] w
        Threes     -> fuzz1 [] [] w
        Q          -> fuzz1 ['Q'] [] w
        Qnou       -> fuzz1 [] ['U'] w
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
                newVow = 
                    vowels
                    |> L.filter (flip L.member noAdd >> not)
                    |> drop ((floor x * 511) % 5) 
                    |> head 
                    |> M.withDefault '*'  
                newCons =
                    consonants
                    |> L.filter (flip L.member noAdd >> not)
                    |> drop ((floor x * 511) % 21) 
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
