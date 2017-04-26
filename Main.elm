module Main exposing (..)

import Html exposing (program)

import CustomTypes exposing (..)
import Update exposing (update, makeBoard)
import View exposing (view)
import Wordlists exposing (..)

import Array exposing (empty)

main = program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- SUBSCRIPTIONS stub
subscriptions : Model -> Sub Msg
subscriptions model = setWordlist Wordlist

-- INIT with a Twos board
init : (Model, Cmd Msg)
init = ({quiz=Twos, wordlist=empty, board=Blank, score=(0,0), showScore=False}, getWordlist "twos")

