import Keyboard
import ItemFeed
import Item
import Toggler
import Static
import Debug
import Mouse
import Hotkeys exposing (hotkeys)

main = Signal.map (ItemFeed.view address) state

model = ItemFeed.init Static.emails Static.reminders
actions = Signal.mailbox Nothing
address = Signal.forwardTo actions.address Just
update maybeAction model =
  case maybeAction of
    Just action ->
      ItemFeed.update action model

    Nothing ->
      model

state =
  Signal.foldp update model
    <| Signal.merge hotkeys actions.signal
