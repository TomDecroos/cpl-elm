module Reminder where

import Html exposing (Html,div,span,text,br)
import Html.Attributes exposing (style,class)

-- MODEL
type alias Model =
  { body: String
  , created: String
  }

view :  Model -> Html
view model =
  [ ("Created",model.created) ]
  |> List.map prettyField
  |> flip List.append [text model.body]
  |> List.intersperse (br [] [])
  |> div []

-- Helper methods
prettyField : (String,String) -> Html
prettyField (name,value) =
  span []
    [ span [class "bold"] [text <| name ++ " : "]
    , text value
    ]
