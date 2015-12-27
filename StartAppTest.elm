import StartApp exposing (start)

import ItemManager
import Task
import Effects exposing (Effects, Never)

app =
    start
      { init = ItemManager.init
      , view = ItemManager.view
      , update = ItemManager.update
      , inputs = [] }

main =
    app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks
