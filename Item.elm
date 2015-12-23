module Item where

import EmailViewer
import Email
import Reminder
import Html exposing (Html,div,button,text)
import Html.Attributes exposing (style,src,height,width)
import Html.Events exposing (onClick)
import Toggler
import String

-- MODEL
type Content
  = Email EmailViewer.Model
  | Reminder Reminder.Model


type alias Model =
  { content : Content
  , pinned : Bool
  , done : Bool
  }

initContent content =
  { content = content
  , pinned = False
  , done = False
  }

initEmail : Email.Model -> Model
initEmail = initContent << Email << EmailViewer.init

initReminder : Reminder.Model -> Model
initReminder = initContent << Reminder

-- UPDATE
type Action
  = Pin Toggler.Action
  | Done Toggler.Action
  | Content ContentAction

type alias ContentAction = EmailViewer.Action

update : Action -> Model -> Model
update action model =
  case action of
    Pin t ->
      {model | pinned <- Toggler.update t model.pinned}

    Done t ->
      {model | done <- Toggler.update t model.done}

    Content contentAction ->
      case model.content of
        Email emailModel ->
          { model
          | content <- Email <| EmailViewer.update contentAction emailModel }

        -- currently only emailviewer has internal actions
        _ ->
          model

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [style [("float","left")] ] <| [viewPin model]
    , div
          [style [("overflow","hidden")] ]
          [ viewContent address model
          , viewPinButton address model
          , viewDoneButton address model
          ]
    ]

viewPin : Model -> Html
viewPin model =
  if model.pinned then
      Html.img [ src "img/Pin.jpg"
               , height 50
               , width 50
               ] []
    else
      text ""

viewContent : Signal.Address Action -> Model -> Html
viewContent address model =
  case model.content of
    Email emailModel ->
      EmailViewer.view (Signal.forwardTo address Content) emailModel

    Reminder reminderModel ->
      Reminder.view reminderModel

viewPinButton : Signal.Address Action -> Model -> Html
viewPinButton address model =
  let button = Toggler.viewButton "Pin" "Unpin"
  in
    button (Signal.forwardTo address Pin) model.pinned

viewDoneButton : Signal.Address Action -> Model -> Html
viewDoneButton address model =
  let button = Toggler.viewButton "Mark as done" "Undo"
  in
    button (Signal.forwardTo address Done) model.done
