module Main where

import Html exposing ( Html )
import Signal
import Static
--import ItemFeed as Root
import Debug
import Hotkeys
import ItemManager
import ItemFeed
import Html exposing (div,node)
import Html.Attributes as A

-- Name: Tom Decroos
-- Student ID: r0297757


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Come up with your own extension!
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- Start of program

main = Signal.map (view mailbox.address) state

state = Signal.foldp update init actions


init =
  let insertEmail =
        update << Just << ItemManager.ModifyFeed << ItemFeed.InsertEmail
      insertReminder =
        update << Just << ItemManager.ModifyFeed << ItemFeed.InsertReminder
  in
    ItemManager.init
      |> flip (List.foldl insertEmail) Static.emails
      |> flip (List.foldl insertReminder) Static.reminders


actions = Signal.merge Hotkeys.hotkeys mailbox.signal


mailbox = Signal.mailbox Nothing


update maybeAction model =
  case maybeAction of
    Just action ->
      ItemManager.update action model

    Nothing ->
      model


view address model =
  div []
      [ stylesheet "style.css"
      , ItemManager.view (Signal.forwardTo address Just) model]

stylesheet : String -> Html
stylesheet href =
  node "link"
    [ A.rel "stylesheet"
    , A.href href
    ] []
