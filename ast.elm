port module Ast exposing (..)

import Html exposing (span, text)

port transform : (String -> msg) -> Sub msg

main = span [] [text "Hello, World!"]
