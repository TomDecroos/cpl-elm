module Item where

import EmailViewer
import Email
import Reminder
import Html exposing (Html,div,button,text)
import Html.Attributes exposing (style,src,height,width,classList)
import Html.Events exposing (onClick)
import Toggler
import String

-- MODEL
type alias ID = Int

type Content
  = Email EmailViewer.Model
  | Reminder Reminder.Model

type alias Model =
  { id : ID
  , content : Content
  , pinned : Bool
  , done : Bool
  , selected : Bool
  }

init : Content -> ID -> Model
init content id =
  { content = content
  , id = id
  , pinned = False
  , done = False
  , selected = False
  }

initEmail : Email.Model -> ID ->  Model
initEmail content = init (Email (EmailViewer.init content))

initReminder : Reminder.Model -> ID -> Model
initReminder content = init (Reminder content)

-- UPDATE
type Action
  = Pin Toggler.Action
  | Done Toggler.Action
  | Select
  | Unselect
  | Content ContentAction

type alias ContentAction = EmailViewer.Action

update : Action -> Model -> Model
update action model =
  case action of
    Pin t ->
      {model | pinned <- Toggler.update t model.pinned}

    Done t ->
      {model | done <- Toggler.update t model.done}

    Select ->
      {model | selected <- True }

    Unselect ->
      {model | selected <- False}

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
  div
    [ classList
      [ ("selected-item", model.selected)
      , ("item",True) ]
    ]
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
