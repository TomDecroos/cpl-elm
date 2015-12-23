module ItemFeed where

import Html exposing (Html,div,text,br,button,ul,li,span,h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import EmailViewer
import Email
import Reminder
import ReminderForm
import Item
import Toggler
import Debug
import String
import ItemFeedSortFunction


-- MODEL
type alias Model =
  { items : List (ID, Item.Model)
  , nextID : ID
  , sortfunction : ItemFeedSortFunction.Model
  , selectedID : ID
  , reminderForm : ReminderForm.Model
  }

type alias ID = Int
-- type alias SortFunction = List (ID,Item.Model) -> List (ID,Item.Model)

init : List Email.Model -> List Reminder.Model -> Model
init emails reminders =
  { items = []
  , nextID = 0
  , sortfunction = ItemFeedSortFunction.basicSort
  , selectedID = 0
  , reminderForm = ReminderForm.init
  }
  |> flip (List.foldl insertEmail)  emails
  |> flip (List.foldl insertReminder) reminders


insertEmail : Email.Model -> Model -> Model
insertEmail email model =
  insertItem Item.initEmail email model

insertReminder : Reminder.Model -> Model -> Model
insertReminder reminder model =
  insertItem Item.initReminder reminder model

insertItem : (a -> Item.Model) -> a -> Model -> Model
insertItem itemInit item model =
  { model
  | items <- ( model.nextID, itemInit item ) :: model.items
  , nextID <- model.nextID + 1
  }

-- UPDATE

type Action
  = Next
  | Previous
  | SetSortFunction ItemFeedSortFunction.Model
  | Modify ID Item.Action
  | ModifySelected Item.Action
  | ModifyForm ReminderForm.Action
  | InsertReminder Reminder.Model
  | HideDone

update action model =
  case Debug.watch "action" action of
    Next ->
      getNewSelectedID model 1

    Previous ->
      getNewSelectedID model (-1)

    SetSortFunction sortfunction ->
      {model | sortfunction <- sortfunction}

    Modify id itemAction ->
      let
        updateItem (itemID, itemModel) =
          if itemID == id then
            (itemID, Item.update itemAction itemModel)
          else
            (itemID, itemModel)
      in
        { model
        | items <- List.map updateItem model.items
        , selectedID <- id }

    ModifySelected itemAction ->
        update (Modify model.selectedID itemAction) model
        {--}
    ModifyForm formAction ->
      {model | reminderForm <- ReminderForm.update formAction model.reminderForm}

    InsertReminder reminder ->
      model
        |> insertReminder reminder
        |> update (ModifyForm ReminderForm.Clear)


getNewSelectedID model offset =
  let
    xs = List.map fst <| model.sortfunction model.items
    n = List.length xs
    ixs = List.map2 (,) [0..n-1] xs
    index = ixs
        |> List.filter (\x -> model.selectedID == snd x)
        |> List.head
        |> Maybe.withDefault (0,0)
        |> fst
    newSelectedID = ixs
        |> List.filter (\x -> (index + offset) % n == fst x)
        |> List.head
        |> Maybe.withDefault (0,0)
        |> snd
  in
    {model | selectedID <- newSelectedID}


-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div
      [style [ ("width","700px")
             , ("margin-left","auto")
             , ("margin-right","auto")
             ]
      ]
      [ h1 [style [("text-align","center")] ] [text "ADD REMINDER"]
      , viewReminderForm address model.reminderForm
      , h1 [style [("text-align","center")] ] [text "TODO"]
      , viewDone False address model
      , h1 [style [("text-align","center")] ] [text "DONE"]
      , viewDone True address model
      ]

viewDone : Bool -> Signal.Address Action -> Model -> Html
viewDone done address model =
      ul
        [ style [ ("list-style-type", "none")
                ]
        ]
        ( model.items
        |> List.filter (\(id,model) -> model.done == done)
        |> model.sortfunction
        |> List.map (viewItem model.selectedID address)
        |> List.map (\x ->li [] [x])
        )

viewItem : ID -> Signal.Address Action -> (ID, Item.Model) -> Html
viewItem selectedID address (id,model) =
  div
    [style [ ("padding-top","20px")
           , ("padding-bottom","20px") ]]
    [ let
        htmlItem = Item.view (Signal.forwardTo address (Modify id)) model
      in
        if id == selectedID then
          selected htmlItem
        else
          htmlItem
    ]

selected : Html -> Html
selected item =
  div
    [style [ ("border-style","none none none groove")
           , ("border-color","lightblue")
           , ("border-width","5px")
           , ("padding-left","5px")
           ]
    ]
    [item]
{--}
viewReminderForm : Signal.Address Action -> ReminderForm.Model -> Html
viewReminderForm address reminderForm =
  let context = ReminderForm.Context
          (Signal.forwardTo address InsertReminder)
          (Signal.forwardTo address ModifyForm)
  in
    [ReminderForm.view context reminderForm]
    |> div [style [("padding-left","50px")] ]
--}
