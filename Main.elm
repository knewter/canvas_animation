import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard


type alias Model =
  { form : Form
  , position : (Int, Int)
  , keys : { x : Int, y : Int }
  }

type Action
  = Tick Time
  | UpdateKeys { x : Int, y : Int }


initialModel : Model
initialModel =
  let
    shape =
      rect 20 20
      |> filled red
  in
    { form = shape
    , position = (0, 0)
    , keys = { x = 0, y = 0 }
    }


clock : Signal Action
clock =
  Signal.map Tick (Time.fps 30)


update : Action -> Model -> Model
update action model =
  case action of
    Tick _ ->
      let
        newX = (fst model.position) + model.keys.x
        newY = (snd model.position) + model.keys.y
        newPosition = ( newX, newY )
      in
        { model | position = newPosition }
    UpdateKeys keys ->
      { model | keys = keys }


view : Model -> Element
view model =
  let
    x = fst model.position
    y = snd model.position
    newForm =
      model.form
      |> move (toFloat x, toFloat y)
  in
    collage 800 800
      [ newForm ]


actions : Signal Action
actions =
  Signal.mergeMany
    [ Signal.map UpdateKeys Keyboard.arrows
    , clock
    ]


model : Signal Model
model =
  Signal.foldp update initialModel actions

main : Signal Element
main =
  Signal.map view model
