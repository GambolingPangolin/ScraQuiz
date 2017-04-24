module CustomTypes exposing (..)
import Array exposing (Array)

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
    , board : Board
    , score: (Int, Int) 
    , showScore : Bool
    }

type Msg =
    ChangeQuiz QuizList
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
