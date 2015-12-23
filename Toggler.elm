module Toggler where

import Html exposing (Html,button,text)
import Html.Events exposing (onClick)

-- MODEL
type alias Model = Bool

-- UPDATE
type Action = Toggle

update action model =
  case action of
    Toggle ->
      not model

-- VIEW
viewButton : String -> String -> Signal.Address Action -> Model -> Html
viewButton false true address model =
  button
    [onClick address Toggle]
    [text <| if model then true else false]
