module EmailViewer where

import Html exposing (Html, div,text,br,button)
import Html.Events exposing (onClick)
import Email
import String
import Debug
import Toggler

-- CONSTANTS
shortBodyLength : Int
shortBodyLength = 200

-- TEST

-- MODEL
type alias Model =
  { email : Email.Model
  , shorten : Maybe Toggler.Model
  }


init : Email.Model -> Model
init email =
  if String.length email.body > shortBodyLength
    then {email = email, shorten = Just True}
    else {email = email, shorten = Nothing}


-- UPDATE
type Action = Shorten Toggler.Action

update : Action -> Model -> Model
update action model =
  case action of
    Shorten toggle ->
      case model.shorten of
        Just x ->
          { model | shorten <- Just <| Toggler.update toggle x}
        Nothing ->
          model

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  div []
  [ viewEmail model
  , viewShortenButton address model
  ]

viewEmail : Model -> Html
viewEmail model =
  case model.shorten of
    Just True ->
      let emailModel = model.email
          shortBody = String.left shortBodyLength emailModel.body
      in
        Email.view  { emailModel | body <- shortBody}

    _ ->
      Email.view model.email

viewShortenButton : Signal.Address Action -> Model -> Html
viewShortenButton address model =
  case model.shorten of
    Just value ->
      let button = Toggler.viewButton "Less" "More"
      in
        button (Signal.forwardTo address Shorten) value
    Nothing ->
      text ""
