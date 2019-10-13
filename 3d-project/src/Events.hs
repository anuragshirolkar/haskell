module Events (
    Movement (..), Action (..), fromEvent
) where

import Graphics.Gloss.Interface.IO.Interact
import Point3

data Movement = 
    MoveForward
    | MoveBackward
    | MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | TurnRight
    | TurnLeft
    | TurnUp
    | TurnDown
    | TiltRight
    | TiltLeft
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Action = Add Movement | Remove Movement | Reset | NoAction

fromEvent :: Event -> Action
fromEvent (EventKey (SpecialKey KeyUp) Down _ _) = Add MoveForward
fromEvent (EventKey (SpecialKey KeyUp) Up _ _) = Remove MoveForward

fromEvent (EventKey (SpecialKey KeyDown) Down _ _) = Add MoveBackward
fromEvent (EventKey (SpecialKey KeyDown) Up _ _) = Remove MoveBackward

fromEvent (EventKey (SpecialKey KeyLeft) Down _ _) = Add TurnLeft
fromEvent (EventKey (SpecialKey KeyLeft) Up _ _) = Remove TurnLeft

fromEvent (EventKey (SpecialKey KeyRight) Down _ _) = Add TurnRight
fromEvent (EventKey (SpecialKey KeyRight) Up _ _) = Remove TurnRight

fromEvent (EventKey (Char 'a') Down _ _) = Add MoveLeft
fromEvent (EventKey (Char 'a') Up _ _) = Remove MoveLeft

fromEvent (EventKey (Char 'd') Down _ _) = Add MoveRight
fromEvent (EventKey (Char 'd') Up _ _) = Remove MoveRight

fromEvent (EventKey (Char 's') Down _ _) = Add MoveDown
fromEvent (EventKey (Char 's') Up _ _) = Remove MoveDown

fromEvent (EventKey (Char 'w') Down _ _) = Add MoveUp
fromEvent (EventKey (Char 'w') Up _ _) = Remove MoveUp

fromEvent (EventKey (Char 'r') Down _ _) = Reset
fromEvent _ = NoAction