module ItemFeedSortFunction where

import Item
import List
import String

-- MODEL
type alias Model = List (ID,Item.Model) -> List (ID, Item.Model)
type alias ID = Int

basicSort : Model
basicSort = sortByWith snd basicCompare

oldItemsOnTop : Model
oldItemsOnTop = sortByWith snd oldCompare


-- combining List.sortWith and List.sorthBy
sortByWith : (a -> b) -> (b -> b -> Order) -> List a -> List a
sortByWith f compare' = List.sortWith (\a b -> compare' (f a) (f b))


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
  case flip booleanCompare a.done b.done of
    EQ ->
      case flip booleanCompare a.pinned b.pinned of
        EQ ->
          dateCompare a b

        order ->
          order

    order ->
      order

oldCompare : Item.Model -> Item.Model -> Order
oldCompare a b =
  case flip booleanCompare a.done b.done of
    EQ ->
      flip dateCompare a b

    order ->
      order
