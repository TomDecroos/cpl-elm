module ReminderForm where

import Reminder
import Html exposing (Html,input,div,input,button,text,textarea,br)
import Html.Attributes exposing (type',value,cols,rows,placeholder)
import Html.Events exposing (onClick, on,targetValue)

-- MODEL
type alias Model = Reminder.Model

init : Model
init =
  { body = ""
  , created = ""
  }
-- UPDATE
type Action
  = UpdateBody String
  | UpdateCreated String
  | Clear

update : Action -> Model -> Model
update action model =
  case action of
    UpdateBody body ->
      {model | body <- body}

    UpdateCreated created ->
      {model | created <- created}

    Clear ->
      init

-- VIEW
type alias Context =
  { submit : Signal.Address Reminder.Model
  , modify : Signal.Address Action
  }

view : Context -> Model -> Html
view context model =
    -- Body
    [ textarea
        [ cols 40
        , rows 3
        , value model.body
        , on "input" targetValue (Signal.message context.modify << UpdateBody)
        ]
        []
    -- Created
    , input
        [ type' "date"
        , value model.created
        , Html.Attributes.required True
        , on "input" targetValue (Signal.message context.modify << UpdateCreated)
        ]
        []
    , button [onClick context.submit model] [text "Submit"]
    ]
    |> List.intersperse (br [] [])
    |> div []
 --}
