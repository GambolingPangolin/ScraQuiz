module CustomTypes exposing (..)

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


