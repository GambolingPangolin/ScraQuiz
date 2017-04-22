import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (..)
import Random exposing (int)
import QuizData exposing (..)
import RandUtils exposing (randSubset)
import Debug exposing (log)

main = Html.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- SUBSCRIPTIONS stub
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL

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

-- Create a new board 

makeTile : String -> Tile
makeTile w = { word = w, isWord = True, isPicked = False }

buildBoard : List String -> Msg
buildBoard ws = 
    NewBoard <| Board (List.map makeTile ws)

makeBoard : QuizList -> Cmd Msg
makeBoard q = 
    let
        wl = Array.fromList (wordlist q)
        n = Array.length wl
        k = 18
        g = Random.list k (Random.float 0 1)
        getter : Int -> List String
        getter i =
          case get i wl of
            Just w  -> [w]
            Nothing -> []
        builder = randSubset n >> List.concatMap getter >> buildBoard
    in
    Random.generate builder g

type alias Model = { quiz : QuizList, board : Board, showScore : Bool }

-- INIT with a Twos board

init : (Model, Cmd Msg)
init = ( Model Twos Blank False, makeBoard Twos )

-- UPDATE

type Msg =
    ChangeQuiz QuizList
    | ToggleTile String
    | MakeNewBoard
    | NewBoard Board
    | CheckScore

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeQuiz x ->
            ( Model x Blank False, makeBoard x )
        MakeNewBoard ->
            ( { model | board = Blank, showScore = False }, makeBoard model.quiz )
        NewBoard x ->
            ( { model | board = x, showScore = False }, Cmd.none )
        CheckScore ->
            ( { model | showScore = True }, Cmd.none )
        ToggleTile x ->
            let
                subst : Tile -> Tile
                subst t =
                    if t.word == x 
                       then { t | isPicked = not t.isPicked }
                       else t
                toggledBoard = case model.board of
                    Board b -> Board <| List.map subst b
                    Blank -> Blank
            in
            ( { model | board = toggledBoard }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    let
        result = 
            if model.showScore
               then [class "result"]
               else []
    in
    div [class "main"]
      [ Html.node "link" [ rel "stylesheet", href "quiz.css" ] [] 
      , h1 [] [ text "Scrabble Quiz" ]
      , div [ class "menu" ] (renderMenu model.quiz)
      , div ([ class "board" ] ++ result) (renderBoard model.board)
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
    , makeElement Threes "Threes"
    , makeElement Qnou "Q (no U)"
    , makeElement Q "Q"
    , makeElement Jqxz "2 of JXQZ"
    , makeElement Cons "Consonants"
    , makeElement ConsY "Consonants (and Y)"
    , makeElement JustVowels "Just vowels"
    ]
    

renderBoard : Board -> List (Html Msg)
renderBoard b = 
    let
        addClasses x atts = 
            case (x.isWord, x.isPicked) of
                (True, True) -> atts ++ [class "isPicked", class "isWord"]
                (True, False) -> (class "isWord") :: atts
                (False, True) -> (class "isPicked") :: atts
                (False, False) -> atts

        viewTile x = span (addClasses x [class "panel", onClick (ToggleTile x.word)]) [text x.word]
    in
    case b of
        Blank -> []
        Board ts ->
            List.map viewTile ts 

wordlist : QuizList -> List String
wordlist q =
    case q of
        Twos -> twos
        Threes -> threes 
        Cons -> consonants
        Qnou -> q_no_u
        Q -> hasq
        Jqxz -> jqxz
        ConsY -> cons_and_y
        JustVowels -> just_vowels
