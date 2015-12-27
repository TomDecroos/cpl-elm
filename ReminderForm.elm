module ReminderForm where

import Reminder
import Toggler
import Html exposing (Html,input,div,input,button,text,textarea,br,span)
import Html.Attributes exposing (type',value,cols,rows,placeholder,class)
import Html.Events exposing (onClick, on,targetValue)

-- MODEL
type alias Model =
  { body : String
  , created: String
  , visible : Bool
  }

init : Model
init =
  { body = ""
  , created = ""
  , visible = True
  }

toReminder model =
  Reminder.Model model.body model.created

-- UPDATE
type Action
  = UpdateBody String
  | UpdateCreated String
  | Reset String
  | Hide Toggler.Action


update : Action -> Model -> Model
update action model =
  case action of
    UpdateBody body ->
      { model | body <- body}

    UpdateCreated created ->
      { model | created <- created}

    Reset today ->
      { model | body <- ""
              , created <- today }

    Hide toggleAction->
      { model | visible <- Toggler.update toggleAction model.visible}

-- VIEW
type alias Context =
  { submit : Signal.Address Reminder.Model
  , modify : Signal.Address Action
  }

view : Context -> Model -> Html
view context model =
  if model.visible then
    div
      []
      [ span [class "title" ] [text "ADD REMINDER"]
      , [ textarea
            [ cols 40
            , rows 3
            , value model.body
            , on "input" targetValue (Signal.message context.modify << UpdateBody)
            ]
            []
        , input
            [ type' "date"
            , value model.created
            , Html.Attributes.required True
            , on "input" targetValue (Signal.message context.modify << UpdateCreated)
            ]
            []
        , button [onClick context.submit (toReminder model)] [text "Submit"]
        ]
        |> List.intersperse (br [] [])
        |> div [class "reminder-form"]
      ]
    else
      text ""
 --}
