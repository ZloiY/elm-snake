module Snake exposing (main)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import List exposing (length)
import Time
import Browser
import Random
import Browser.Events exposing (onKeyPress)
import Html.Attributes exposing (attribute)
import Debug exposing (log)
import Json.Decode as D
import Html.Events exposing (on)
import Html.Attributes exposing (shape)
import List exposing (append)
canvasWidth : Int
canvasWidth = 700
canvasHeight : Int
canvasHeight = 700

bodyPartLength: Length
bodyPartLength = 20
bodyPartsOffset: Int
bodyPartsOffset = 2

foodMinScore : Int
foodMinScore = 10

foodMaxScore : Int
foodMaxScore = 20

initHeadPos: Position
initHeadPos = ((canvasWidth // 2), (canvasHeight // 2))

initTailList: List Position
initTailList = [ (nextPosition Left initHeadPos), (nextPosition Left (nextPosition Left initHeadPos)) ]

initGameSpeed: Speed
initGameSpeed = 500

type alias Position = (Int, Int)
type alias Length = Int
type alias Score = Int
type alias Food = (Maybe Position, Score)
type alias Speed = Int

type Msg = Tick Time.Posix
  | KeyPressed MovingDirection
  | SpawnFood (Position, Score)

type MovingDirection = Left
  | Right
  | Up
  | Down
  | Spacebar
  | Other

type alias Snake = 
  { length: Length
  , head: Position
  , tail: List Position
  , movingDirection: MovingDirection
  }

type Game = Pending
  | Started Snake Food Score Speed

init: () -> (Game, Cmd Msg)
init _ = (Pending, Cmd.none)

initSnake: Snake
initSnake =
  ({ length = 3
  , head = initHeadPos
  , tail = initTailList
  , movingDirection = Right
  })

nextPosition : MovingDirection -> Position -> Position
nextPosition direction (posX, posY) =
  case direction of
    Left ->
      ((posX + bodyPartLength + bodyPartsOffset), posY)
    Right ->
      ((posX - (bodyPartLength + bodyPartsOffset)), posY)
    Up ->
      (posX, (posY - (bodyPartLength + bodyPartsOffset)))
    Down ->
      (posX, (posY + bodyPartLength + bodyPartsOffset))
    _ -> (posX, posY)

rollCoordinate : Random.Generator Int
rollCoordinate = Random.int bodyPartLength canvasWidth

rollScore : Random.Generator Score
rollScore = Random.int foodMinScore foodMaxScore

spawnFood : Random.Generator (Position, Score)
spawnFood = Random.pair (Random.pair rollCoordinate rollCoordinate) rollScore

nextTailPosition : Position -> List Position -> List Position
nextTailPosition prevHead list = prevHead :: List.take (List.length list - 1) list
  
renderSquare : Position -> Renderable
renderSquare (posX, posY) =
  shapes [ fill (Color.black) ]
      [ rect (toFloat posX, toFloat posY) (toFloat bodyPartLength) (toFloat bodyPartLength) ]

renderSnake : Snake -> List Renderable
renderSnake { head, tail } = renderSquare head :: (List.map (\tailNode -> renderSquare tailNode) tail)

renderFood : Food -> Renderable
renderFood (position, score) =
  case position of
      Just (posX, posY) -> shapes [ fill Color.red ] [ circle (toFloat posX, toFloat posY) (toFloat score) ]
      Nothing -> shapes [] []

isOverlap : Position -> Food -> Bool
isOverlap (posX, posY) food =
  case food of
    (Just (foodX, foodY), foodRadius) ->
      let
        (xd, yd) = (foodX - posX, foodY - posY)
        distance = sqrt (toFloat (xd * xd + yd * yd))
      in 
        distance <= toFloat (foodRadius * 2)
    (Nothing, _) -> False

isGameOver : Position -> List Position -> Bool
isGameOver head tail =
  List.any ((==) head) tail
  || Tuple.first head < 0
  || Tuple.second head < 0
  || Tuple.first head > canvasWidth
  || Tuple.second head > canvasHeight

keyDecoder : D.Decoder Msg
keyDecoder = 
  D.field "key" D.string
    |> D.map toDirection
    |> D.map (\key -> KeyPressed key)

toDirection : String -> MovingDirection
toDirection string =
  case string of
    "d" ->
      Left
    "a" ->
      Right
    "w"->
      Up
    "s" ->
      Down
    " " ->
      Spacebar
    _ ->
      Other

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case game of
    Pending ->
      case msg of
        KeyPressed Spacebar -> (Started initSnake (Nothing, 0) 0 initGameSpeed, Cmd.none)
        _ -> (game, Cmd.none)
    Started snake food score speed ->
      case msg of
        Tick _ -> if (Tuple.first food) == Nothing then 
          (Started {
            snake
            | head = nextPosition snake.movingDirection snake.head
            , tail = nextTailPosition snake.head snake.tail
            } (Nothing, 0) score speed, Random.generate SpawnFood spawnFood)
          else if isOverlap snake.head food then
            (Started {
            snake
            | head = nextPosition snake.movingDirection snake.head
            , tail = snake.head :: snake.tail
            , length = snake.length + 1
            } (Nothing, 0) (score + (Tuple.second food)) (speed - 20), Cmd.none)
          else if isGameOver snake.head snake.tail then
            (Pending, Cmd.none)
          else
            (Started {
            snake
            | head = nextPosition snake.movingDirection snake.head
            , tail = nextTailPosition snake.head snake.tail
            } food score speed, Cmd.none)

        KeyPressed direction ->
          if (direction == Right || direction == Left) && (snake.movingDirection == Right || snake.movingDirection == Left) && snake.movingDirection /= direction then
            (game, Cmd.none)
          else if (direction == Up || direction == Down) && (snake.movingDirection == Up || snake.movingDirection == Down) && snake.movingDirection /= direction then
            (game, Cmd.none)
          else
            (Started { snake | movingDirection = direction } food score speed, Cmd.none)

        SpawnFood (position, radius) ->
          (Started snake (Just position, radius) score speed, Cmd.none)

subscriptions : Game -> Sub Msg
subscriptions game =
  case game of
    Pending -> onKeyPress keyDecoder
    Started _ _ _ speed ->
      Sub.batch
        [ Time.every (toFloat speed) Tick
        , onKeyPress keyDecoder]

view : Game -> Html Msg
view game =
  case game of
      Pending ->
        div [ style "width" ((String.fromInt canvasWidth) ++ "px")
            , style "height" ((String.fromInt canvasHeight) ++ "px")
            , style "text-align" "center"
            , style "background-color" "lightgrey"
            ]
            [ Html.text "Use 'w', 'a', 's', 'd' for your controls. Press spacebar to start" ]
      Started snake food score _ ->
        div [ style "width" "300px" ]
            [ Html.text ("Score: " ++ (String.fromInt score) ++ " Snake length: " ++ (String.fromInt snake.length))
            , Canvas.toHtml (canvasWidth, canvasHeight)
                []
                (List.append
                  [ clear (0, 0) (toFloat canvasWidth) (toFloat canvasHeight)
                  , shapes [ fill Color.lightGrey ] [ rect (0, 0) (toFloat canvasWidth) (toFloat canvasHeight) ]
                  , renderFood food
                  ] (renderSnake snake))
            ]

main : Program () Game Msg
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }