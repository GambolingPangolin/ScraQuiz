import Html exposing (program)

import CustomTypes exposing (..)
import Update exposing (update, makeBoard)
import View exposing (view)

main = program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- SUBSCRIPTIONS stub
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- INIT with a Twos board
init : (Model, Cmd Msg)
init = (Model Twos Blank (0,0) False, makeBoard Twos)

