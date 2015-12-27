module ItemSort where

import Item
import List
import String

-- MODEL
type alias Model = List Item.Model -> List Item.Model

basic : Model
basic = List.sortWith basicCompare

oldItemsOnTop : Model
oldItemsOnTop = List.sortWith oldCompare


-- Helper Methods
booleanCompare a b =
  let boolToInt x = if x then 1 else 0
  in compare (boolToInt a) (boolToInt b)

getDate : Item.Model -> String
getDate model =
  case model.content of
    Item.Email emailviewer ->
      emailviewer.email.date

    Item.Reminder reminder ->
      reminder.created

dateCompare : Item.Model -> Item.Model -> Order
dateCompare a b =
  let
    stringToInt x = Maybe.withDefault 0 <| Result.toMaybe <| String.toInt x
    convert x = x
      |> getDate
      |> String.split "-"
      |> List.map stringToInt
  in compare (convert a) (convert b)

basicCompare : Item.Model -> Item.Model -> Order
basicCompare a b =
  case flip booleanCompare a.pinned b.pinned of
    EQ ->
      dateCompare a b

    order ->
      order

oldCompare : Item.Model -> Item.Model -> Order
oldCompare a b = flip dateCompare a b
