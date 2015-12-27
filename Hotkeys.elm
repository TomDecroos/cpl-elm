module Hotkeys where

import Signal
import ItemManager
import ItemFeed
import ItemSort
import Item
import EmailViewer
import Toggler
import Keyboard
import Char
import ReminderForm

type alias Action = Maybe ItemManager.Action
-- HOTKEYS SIGNAL
hotkeys: Signal Action
hotkeys
  = [ hotkey 'J'
        <| modifyfeed selectnext
    , hotkey 'K'
        <| modifyfeed selectprevious
    , hotkey 'O'
        <| modifyfeed <| modifyselected <| content <| shorten toggle
    , hotkey 'P'
        <| modifyfeed <| modifyselected <| pin toggle
    , hotkey 'X'
        <| modifyfeed <| modifyselected <| done toggle
    , holdkey 'S'
        (modifyfeed <| modifysort olditemsontop)
        (modifyfeed <| modifysort basic)
    , hotkey 'T'
        <| modifyfeed <| hidedone toggle
    , hotkey 'H'
        <| modifyform <| hide toggle
    ]
    |> Signal.mergeMany

comboKey : Signal Bool
comboKey = Keyboard.alt

-- ACTION KEYWORDS
modifyfeed = ItemManager.ModifyFeed
modifyselected = ItemFeed.ModifySelected
selectnext = ItemFeed.SelectNext
selectprevious = ItemFeed.SelectPrevious
hidedone = ItemFeed.HideDone
modifysort = ItemFeed.ModifySort
olditemsontop = ItemSort.oldItemsOnTop
basic = ItemSort.basic
pin = Item.Pin
done = Item.Done
content = Item.Content
shorten = EmailViewer.Shorten
toggle = Toggler.Toggle
modifyform = ItemManager.ModifyForm
hide = ReminderForm.Hide

-- IMPLEMENTATION
hotkey: Char -> ItemManager.Action -> Signal Action
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
