module Counter (Model, init, Action, update, view) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- Model


type alias Model =
  Int


init : Int -> Model
init count =
  count



-- UPDATE
-- how our model can be transformed


type Action
  = Increment
  | Decrement


update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW super declarative
{- -
 using_mailboxes_in_elm.md (https://gist.github.com/mgold/461dbf37d4d34767e5da)
 https://github.com/elm-guides/elm-for-js
 The root of the problem is that we're creating one signal for each element.
 The key idea behind a mailbox is that it's one signal that can receive updates for many elements.
-
-}


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]


countStyle : Attribute
countStyle =
  style
    [ ( "font-size", "20px" )
    , ( "font-family", "monospace" )
    , ( "display", "inline-block" )
    , ( "width", "50px" )
    , ( "text-align", "center" )
    ]
