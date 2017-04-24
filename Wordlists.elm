port module Wordlists exposing (..)
import Array exposing (Array)

port getWordlist : String -> Cmd msg
port setWordlist : (Array String -> msg) -> Sub msg
