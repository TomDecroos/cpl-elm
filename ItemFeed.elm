module ItemFeed where

import Html exposing (Html,div,text,br,button,ul,li,span,h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style,class)
import EmailViewer
import Email
import Reminder
import ReminderForm
import Item
import Toggler
import Debug
import String
import ItemSort


-- MODEL
type alias Model =
  { items : List Item.Model
  , nextID : Item.ID
  , sort : ItemSort.Model
  , hideDone : Bool
  }

init : Model
init =
  { items = []
  , nextID = 0
  , sort = ItemSort.basic
  , hideDone = False
  }

type alias SubList =
  { title : String
  , items : List Item.Model
  }


sublists : Model -> List SubList
sublists model =
  let (done,todo) = List.partition .done model.items
  in
    [ ( True
      , { title = "TODO"
        , items = model.sort todo })
    , ( not model.hideDone
      , { title = "DONE"
        , items = model.sort done })
    ]
    |> List.filter fst
    |> List.map snd

visibleItems : Model -> List Item.Model
visibleItems = List.concat << List.map .items << sublists

-- UPDATE

type Action
  = Select Item.ID
  | SelectWithOffset Int
  | SelectNext
  | SelectPrevious
  | SelectRefresh
  | ModifySort ItemSort.Model
  | ModifyItem Item.ID Item.Action
  | ModifySelected Item.Action
  | InsertReminder Reminder.Model
  | InsertEmail Email.Model
  | InsertItem (Item.ID -> Item.Model)
  | HideDone Toggler.Action

update action model =
  case Debug.watch "action" action of
    Select id ->
      let
        updateItem item =
          if item.id == id then
            Item.update Item.Select item
          else
            Item.update Item.Unselect item
      in
        { model | items <- List.map updateItem model.items }

    SelectWithOffset offset ->
        let
          items = visibleItems model
          n = List.length items
          indexedItems = List.map2 (,) [0..n-1] items
          selectedIndex = indexedItems
            |> List.filter (.selected << snd)
            |> List.map fst
            |> List.head
          newID =
            case selectedIndex of
              Just index ->
                indexedItems
                  |> List.filter (\x -> (index + offset) % n == fst x)
                  |> List.head
                  |> Maybe.map (.id << snd)
                  |> Maybe.withDefault -1
              Nothing ->
                items
                  |> List.head
                  |> Maybe.map .id
                  |> Maybe.withDefault -1
        in
          update (Select newID) model

    SelectNext ->
      update (SelectWithOffset 1) model

    SelectPrevious ->
      update (SelectWithOffset -1) model

    SelectRefresh ->
      update (SelectWithOffset 0) model

    ModifySort sort ->
      {model | sort <- sort}

    ModifyItem id itemAction ->
      let
        updateItem item =
          if item.id == id then
            Item.update itemAction item
          else
            item
      in
        { model | items <- List.map updateItem model.items }
        |> update (Select id)

    ModifySelected itemAction ->
      let
        updateItem item =
          if item.selected then
            Item.update itemAction item
          else
            item
      in
        { model | items <- List.map updateItem model.items }

    InsertReminder reminder ->
      update (InsertItem (Item.initReminder reminder)) model

    InsertEmail email ->
      let item = Item.init (Item.Email (EmailViewer.init email))
      in update (InsertItem item) model

    InsertItem item ->
      { model
      | items <- (item model.nextID ) :: model.items
      , nextID <- model.nextID + 1
      }
        |> update (Select model.nextID)

    HideDone toggleAction ->
      {model | hideDone <- Toggler.update toggleAction model.hideDone}
      |> update SelectRefresh

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div
      [class "item-feed"]
      (List.map (viewSubList address) (sublists model))


viewSubList address sublist =
  if List.isEmpty sublist.items then
    text ""
  else
    div
      []
      [ span [class "title" ] [text sublist.title]
      , ul
          [class "item-list" ]
          ( sublist.items
            |> List.map (viewItem address)
            |> List.map (\x ->li [] [x])
          )
      ]

viewItem : Signal.Address Action -> Item.Model -> Html
viewItem address model =
  Item.view (Signal.forwardTo address (ModifyItem model.id)) model
