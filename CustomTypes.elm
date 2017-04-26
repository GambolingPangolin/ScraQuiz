module CustomTypes exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)

type alias Tile = { word : String, isWord : Bool, isPicked : Bool }

type Board = 
    Board (List Tile)
    | Blank

type QuizList =
    Twos
    | Threes
    | Q
    | Qnou
    | Jqxz
    | Cons
    | ConsY
    | JustVowels

type alias Model = { 
    quiz : QuizList
    , wordlist: Array String
    , board : Board
    -- key: word
    -- value: wordscore = nCorrect - nIncorrect 
    , log : Dict String Int 
    , score: (Int, Int) 
    , showScore : Bool
    }

type Msg =
    ChangeList QuizList
    | ToggleTile String
    | MakeNewBoard
    | NewBoard Board
    | CheckScore
    | Wordlist (Array String)

quizName : QuizList -> String
quizName q =
  case q of
    Twos -> "twos"
    Threes -> "threes"
    Q -> "q"
    Qnou -> "q_no_u"
    Jqxz -> "2_jqxz"
    Cons -> "cons"
    ConsY -> "cons_and_y"
    JustVowels -> "just_vowels"
