module Hotkeys where

import Signal
import ItemFeed
import ItemFeedSortFunction
import Item
import EmailViewer
import Toggler
import Keyboard
import Char
import Debug


-- HOTKEYS SIGNAL
hotkeys: Signal (Maybe Action)
hotkeys
  = [ hotkey 'J' next
    , hotkey 'K' previous
    , hotkey 'O' (modifyselected (content (shorten toggle)))
    , hotkey 'P' (modifyselected (pin toggle))
    , hotkey 'X' (modifyselected (done toggle))
    , holdkey 'S' (setsortfunction olditemsontop) (setsortfunction basicsort)
    ]
    |> Signal.mergeMany

comboKey : Signal Bool
comboKey = Keyboard.alt


type alias Action = ItemFeed.Action
-- ACTION KEYWORDS
modifyselected = ItemFeed.ModifySelected
next = ItemFeed.Next
previous = ItemFeed.Previous
setsortfunction = ItemFeed.SetSortFunction
olditemsontop = ItemFeedSortFunction.oldItemsOnTop
basicsort = ItemFeedSortFunction.basicSort
pin = Item.Pin
done = Item.Done
content = Item.Content
shorten = EmailViewer.Shorten
toggle = Toggler.Toggle

-- IMPLEMENTATION
hotkey: Char -> Action -> Signal (Maybe Action)
hotkey key action =
  let signal = Signal.map2 (&&) comboKey (isDown key)
      convert bool =
        if bool then
            Just action
        else
            Nothing
  in
    Signal.map convert signal

holdkey key action1 action2 =
  let signal = Signal.map2 (&&) comboKey (isDown key)
      convert bool =
        if bool then
            Just action1
        else
            Just action2
  in
    Signal.map convert signal

isDown : Char -> Signal Bool
isDown key = Keyboard.isDown <| Char.toCode key
