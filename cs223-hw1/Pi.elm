module Pi where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (foldp, map, map2)
import Random exposing (generate, float)
import Window
import Time exposing (inMilliseconds, every, millisecond )

type alias State = ((Int, List Point), (Int, List Point)) -- (hits, misses)
type alias Point = { x:Float, y:Float }

initState = ((0,[]), (0,[]))

states : Signal State
states = foldp upstate initState signalPoint

view : Signal State -> Signal (List Form)
view =
  let
    f ((hits, ps), (misses, qs)) =
      (pointsToCircles lightGreen ps)
      ++ (pointsToCircles lightRed qs)
  in
    map f

background : List Form
background =
  [ square 512
        |> filled clearGrey
      ,circle 256
        |> filled (rgba 255 255 255 1)
    ]

myPi : Signal State -> Signal Element
myPi =
  let
    f ((hits, _), (misses, _)) =
      4 * (toFloat hits)/(toFloat (hits + misses))
        |> show
  in
    map f

toList : Signal (List Element)
toList = map2 (\e1 e2 -> [e1, e2]) (myPi states) (map ((\f -> background ++ f) >> collage 512 512) (view states))

main : Signal Element
main = map (flow down) toList

-- x and y must satisfy (x - center_x)^2 + (y - center_y)^2 < radius^2
hit : Point -> Bool
hit p = (p.x^2 + p.y^2) < 256^2

upstate : Point -> State -> State
upstate pt ((hits, ps), (misses, qs)) =
  if (hit pt)
  then ((hits + 1, pt :: ps), (misses, qs))
  else ((hits, ps), (misses + 1, pt :: qs))

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.1

lightRed : Color
lightRed =
  rgba 200 0 0 0.4

lightGreen : Color
lightGreen =
  rgba 0 200 0 0.4

pointsToCircles : Color -> List Point -> List Form
pointsToCircles c ps = List.map (pointsToCircle c) ps

pointsToCircle : Color -> Point -> Form
pointsToCircle c p = circle 4
                      |> filled c
                      |> move (p.x,p.y)

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s =
  let
      (x', s')   = generate (float -256 256) s
      (y', s'')  = generate (float -256 256) s'
  in
      ({x=x',y=y'}, s'')

timeSeed : Signal Int
timeSeed = map (inMilliseconds >> round) (every millisecond)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
  let
    f : Int -> (Point, Random.Seed) -> (Point, Random.Seed)
    f i s = genPoint (snd s)
  in
    foldp f ({x=0,y=0}, Random.initialSeed 0) timeSeed

signalPoint : Signal Point
signalPoint = map fst signalPointSeed
