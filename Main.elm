module Main exposing (..)

import Html exposing (program)

import Types exposing (..)
import Update exposing (update)
import View exposing (view)
import Wordlists exposing (..)

import Array exposing (empty)
import Dict as D

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
init = (Intro, Cmd.none)
