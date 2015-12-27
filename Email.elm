module Email where

import Html exposing (Html,div,text,br,span)
import Html.Attributes exposing (class)
-- MODEL
type alias Model =
  { from: String
  , to: String
  , title: String
  , body: String
  , date: String
  }

-- VIEW
view : Model -> Html
view model =
  [ ("From",model.from)
  , ("To",model.to)
  , ("Date",model.date)
  , ("Title",model.title) ]
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
