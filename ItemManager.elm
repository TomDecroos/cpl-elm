module ItemManager where

import ItemFeed as Feed
import ReminderForm as Form
import Reminder
import Html exposing (Html,div)
import Html.Attributes exposing (class)

type alias Model =
  { itemfeed : Feed.Model
  , reminderform : Form.Model
  }

init =
  { itemfeed = Feed.init
  , reminderform = Form.init
  }

type Action
  = ModifyFeed Feed.Action
  | ModifyForm Form.Action
  | SubmitReminder Reminder.Model

update action model =
  case action of
    ModifyFeed feedAction ->
      { model | itemfeed <- Feed.update feedAction model.itemfeed}

    ModifyForm formAction ->
      { model | reminderform <- Form.update formAction model.reminderform}

    SubmitReminder reminder ->
      model
        |> update (ModifyFeed (Feed.InsertReminder reminder))
        |> update (ModifyForm (Form.Reset ""))

view address model =
  div
    [class "item-manager"]
    [ let context = Form.Context
            (Signal.forwardTo address SubmitReminder)
            (Signal.forwardTo address ModifyForm)
      in Form.view context model.reminderform
    , Feed.view (Signal.forwardTo address ModifyFeed) model.itemfeed
    ]
